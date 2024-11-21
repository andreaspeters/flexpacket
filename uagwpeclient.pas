unit uagwpeclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  Graphics, StrUtils, utypes, RegExpr, Sockets;

type
  { TAGWPEClient }

  PTFPConfig = ^TFPConfig;
  TLinkStatus = array[0..2] of string;
  TChannelString = array[0..10] of string;
  TChannelStatus = array[0..10] of TStatusLine;
  TChannelCallsign = array[0..10] of String;

  TAGWPEConnectRequest = packed record
    Port: Byte;                   // AGWPE Port (z. B. 0 = Port 1)
    Reserved1: array[0..2] of Byte; // Reservierte 3 Bytes, setzen auf 0
    DataKind: Byte;                // 'C' für Verbindung
    Reserved2: Byte;               // 1 Byte reserviert, setzen auf 0
    PID: Byte;                     // PID für AX.25 (z. B. 0xF0)
    Reserved3: Byte;               // weiteres reserviertes Byte, setzen auf 0
    CallFrom: array[0..9] of Byte; // Eigenes Rufzeichen (CallSign) + SSID, 10 Bytes
    CallTo: array[0..9] of Byte;   // Ziel-Rufzeichen (CallSign) + SSID, 10 Bytes
    DataLen: Integer;              // Länge der Nutzdaten (4 Bytes, hier 0, da keine Daten)
    Data: array[0..3] of Byte;     // weitere 4 reservierte Bytes, setzen auf 0
  end;

  TAGWPEClient = class(TThread)
  private
    FSocket: TSocket;
    FPConfig: PTFPConfig;
    procedure ReceiveData;
    procedure Connect;
    function DecodeLinkStatus(Text:string):TLinkStatus;
    function PrepareCredentials(const UserId, Password: string): TBytes;
  protected
    procedure Execute; override;
  public
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    constructor Create(Config: PTFPConfig);
    procedure Disconnect;
    procedure LoadTNCInit;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Data: string);
    destructor Destroy; override;
  end;

const
  WPEConnectRequestSize = SizeOf(TAGWPEConnectRequest);

implementation

{ TAGWPEClient }

constructor TAGWPEClient.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FreeOnTerminate := True;
  Start;

  if not IsValidIPAddress(FPConfig^.AGWServerIP) then
  begin
    ShowMessage('AGW Server IP is not valid.');
    Exit;
  end;
end;

destructor TAGWPEClient.Destroy;
begin
  Disconnect;
  inherited Destroy;
end;

procedure TAGWPEClient.Connect;
var Addr: TInetSockAddr;
    IPAddr: string;
    Octets: array[0..3] of Byte;
    Parts: TStringArray;
begin
  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FSocket = -1 then
  begin
    write('Failed to create socket.');
    Exit;
  end;

  IPAddr := '127.0.0.1';
  Parts := SplitString(IPAddr, '.');

  Addr.sin_family := AF_INET;
  Addr.sin_port := htons(8000);

  Octets[0] := StrToInt(Parts[0]);
  Octets[1] := StrToInt(Parts[1]);
  Octets[2] := StrToInt(Parts[2]);
  Octets[3] := StrToInt(Parts[3]);

  Addr.sin_addr.s_bytes[1] := Octets[0];
  Addr.sin_addr.s_bytes[2] := Octets[1];
  Addr.sin_addr.s_bytes[3] := Octets[2];
  Addr.sin_addr.s_bytes[4] := Octets[3];

  if fpConnect(FSocket, @Addr, SizeOf(Addr)) < 0 then
  begin
    fpShutdown(FSocket,  SHUT_RDWR);
    write('Failed to connect to AGWPE server');
  end;
end;

procedure TAGWPEClient.Disconnect;
begin
  if FSocket <> -1 then
  begin
    fpShutdown(FSocket, SHUT_RDWR);
    FSocket := -1;
  end;
end;

procedure TAGWPEClient.Execute;
begin
  try
    Connect;
    LoadTNCInit;
    SetCallsign;
    // Initialisierung des AGWPE-Clients
    SendStringCommand(0, 1, 'G');
    if (Length(FPConfig^.AGWServerUsername) > 0) and (Length(FPConfig^.AGWServerPassword) > 0) then
      SendStringCommand(0, 1, 'P');
    while not Terminated do
    begin
      ReceiveData;
      write('.');
      Sleep(5);
    end;
  finally
    Disconnect;
  end;
end;

procedure TAGWPEClient.SendStringCommand(const Channel, Code: byte; const Data: String);
var Request: TAGWPEConnectRequest;
    SentBytes: SizeInt;
    i: Integer;
    ByteCmd: TBytes;
    ChannelDestCallsign, ChannelFromCallsign: TChannelCallsign;
    Command: String;
begin
  ByteCmd := TBytes.Create;
  Request := Default(TAGWPEConnectRequest);
  FillChar(Request, WPEConnectRequestSize, 0);
  Command := Data;

  // if it' a command, take the first char and then remove the first two
  if Code = 1 then
  begin
    Request.DataKind := Ord(Command[1]);
    Delete(Command, 1, 2);
    Delete(Command, Pos(' ', Command), Length(Command) - Pos(' ', Command) + 1);
    ChannelFromCallsign[Channel] := UpperCase(FPConfig^.Callsign);
    ChannelDestCallsign[Channel] := UpperCase(Command);

    if Chr(Request.DataKind) = 'X' then
      ChannelDestCallsign[Channel] := '';

    // Send Authentication Frame
    if (Chr(Request.DataKind) = 'P') and (Length(FPConfig^.AGWServerUSername) > 0) and (Length(FPConfig^.AGWServerPassword) > 0) then
    begin
      ChannelFromCallsign[Channel] := '';
      ChannelDestCallsign[Channel] := '';
      Request.DataLen := 510;
      SetLength(ByteCmd, 510);
      ByteCmd := PrepareCredentials(FPConfig^.AGWServerUsername, FPConfig^.AGWServerPassword);
    end;
  end;

  // If it's not a command, then send a Data Frame
  if Code = 0 then
  begin
    Request.DataKind := Ord('D');
    Request.DataLen := Length(Command);
    SetLength(ByteCmd, Length(Command));
    for i := 1 to Length(Command) do
      ByteCmd[i-1] := Ord(Command[i]);
  end;

  Request.Port := 0;
  Request.PID := $00;

  for i := 0 to Length(ChannelDestCallsign[Channel])-1 do
    Request.CallTo[i] := Ord(ChannelDestCallsign[Channel][i+1]);
  for i := 0 to Length(ChannelFromCallsign[Channel])-1 do
    Request.CallFrom[i] := Ord(ChannelFromCallsign[Channel][i+1]);

  SentBytes := fpSend(FSocket, @Request, SizeOf(Request), 0);
  if SentBytes < 0 then
    writeln('Error during sending data to AGW');

  if (Code = 0) or (Chr(Request.DataKind) = 'P') then
  begin
    SentBytes := fpSend(FSocket, @ByteCmd, SizeOf(ByteCmd), 0);
    if SentBytes < 0 then
      writeln('Error during sending data to AGW');
  end;
end;

function TAGWPEClient.PrepareCredentials(const UserId, Password: string): TBytes;
var
  UserIdBytes, PasswordBytes, ResultArray: TBytes;
  i, lP, lU: Integer;
begin
  ResultArray := TBytes.Create;
  UserIdBytes := TBytes.Create;
  PasswordBytes := TBytes.Create;

  SetLength(UserIdBytes, 255);
  SetLength(PasswordBytes, 255);

  lU := Length(UserId);
  for i := 0 to lU do
    UserIdBytes[i] := Byte(UserId[i + 1]);
  for i := lU to 255 do
    UserIdBytes[i+1] := 0;

  lP := Length(Password);
  for i := 0 to lP do
    PasswordBytes[i] := Byte(Password[i + 1]);
  for i := lP to 255 do
    PasswordBytes[i+1] := 0;

  SetLength(ResultArray, 510);
  for i := 0 to 254 do
    ResultArray[i] := UserIdBytes[i];

  for i := 0 to 254 do
    ResultArray[255 + i] := PasswordBytes[i];

  Result := ResultArray;
end;

procedure TAGWPEClient.ReceiveData;
var Request: TAGWPEConnectRequest;
    Buffer, ReqArray: TBytes;
    TotalReceived, Received: Integer;
    RemainingData, i: Integer;
    Data : String;
    LinkStatus: TLinkStatus;
begin
  Buffer := TBytes.Create;
  ReqArray := TBytes.Create;
  Request := Default(TAGWPEConnectRequest);

  SetLength(Buffer, WPEConnectRequestSize);
  SetLength(ReqArray, WPEConnectRequestSize);

  TotalReceived := 0;
  RemainingData := WPEConnectRequestSize;

  // read data until request struct is complete
  while TotalReceived < WPEConnectRequestSize do
  begin
    // Empfange Daten aus dem Socket
    Received := fpRecv(FSocket, @Buffer[TotalReceived], RemainingData, 0);
    if Received <= 0 then
      Exit;

    for i := TotalReceived to Received do
    begin
      ReqArray[i] := Buffer[i];
    end;
    // Daten zählen
    Inc(TotalReceived, Received);
    RemainingData := WPEConnectRequestSize - TotalReceived;
  end;

  Move(ReqArray[0], Request, WPEConnectRequestSize);

  // read data
  data := '';
  if Request.DataLen > 0 then
  begin
    SetLength(Buffer, Request.DataLen);
    Received := fpRecv(FSocket, @Buffer[0], Request.DataLen, 0);
    if Received <= 0 then
      Exit;

    for i := 0 to Received do
    begin
      Data := Data + Chr(Buffer[i]);
    end;
  end;

  write(Data);

  case Chr(Request.DataKind) of
    'C': // connection response
    begin
      if Length(Data) > 0 then
      begin
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + #27'[32m' + '>>> LINK STATUS: ' + Data + #13#27'[0m';
      end;
      write(Data);
    end;
    'd': // disconnect command response
    begin
      if Length(Data) > 0 then
      begin
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + #27'[32m' + '>>> LINK STATUS: ' + Data + #13#27'[0m';
        LinkStatus := DecodeLinkStatus(Data);
        ChannelStatus[Request.Port+1][6] := LinkStatus[0]; // Status Text CONNECTED, DISCONNECTED, etc
        ChannelStatus[Request.Port+1][7] := LinkStatus[1]; // Call of the other station
      end;
      write(Data);
    end;
    'D': // data
    begin
      if Length(Data) > 0 then
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + Data;
      write(Data);
    end;
    'I': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data;
      write(Data);
    end;
    'm': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data;
      write(Data);
    end;
    'S': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data;
      write(Data);
    end;
    'U': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data;
      write(Data);
    end;
    'T': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data;
      write(Data);
    end;
  end;
end;

function TAGWPEClient.DecodeLinkStatus(Text:string):TLinkStatus;
var Regex: TRegExpr;
    Status, CallSign: string;
begin
  Regex := TRegExpr.Create;

  try
    // Regular Expression für verschiedene Textmuster
    Regex.Expression := '^(\*\*\*)\s+(CONNECTED|DISCONNECTED|CONNECTED RETRYOUT|DISCONNECTED RETRYOUT|)\s+(With|To Station)\s+([A-Z0-9\-]+)?';
    Regex.ModifierI := True;

    if Regex.Exec(Text) then
    begin
      Status := Regex.Match[2];   // CONNECTED, DISCONNECTED, etc.
      CallSign := Regex.Match[5]; // {call}

      Result[0] := StringReplace(Status, ' ', '', [rfReplaceAll]);
      Result[1] := StringReplace(Callsign, ' ', '', [rfReplaceAll]);
    end;
  finally
    Regex.Free;
  end;
end;

procedure TAGWPEClient.LoadTNCInit;
var FileHandle: TextFile;
    HomeDir, Line: string;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  AssignFile(FileHandle, HomeDir + '/agw_init');

  // write init file if it does not exist
  if not FileExists(HomeDir + '/agw_init') then
  begin
    Rewrite(FileHandle);
    try
      WriteLn(FileHandle, 'M');
    finally
      CloseFile(FileHandle);
    end;
  end;

  Reset(FileHandle);
  try
    // send needed parameter
    SendStringCommand(0,1,'M');

    // send parameter from init file
    while not EOF(FileHandle) do
    begin
         Readln(FileHandle, Line);
         SendStringCommand(0,1,Line);
    end;
  finally
    CloseFile(FileHandle);
  end;
end;

procedure TAGWPEClient.SetCallsign;
begin
  SendStringCommand(0,1,'X');
end;


end.

