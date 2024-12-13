unit uagwpeclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  Graphics, utypes, RegExpr,
  {$IFDEF UNIX}Sockets, netdb{$ELSE}WinSock{$ENDIF};

type
  { TAGWPEClient }

  TLinkStatus = array[0..2] of string;
  TChannelString = array[0..10] of string;
  TChannelStatus = array[0..10] of TStatusLine;
  TChannelCallsign = array[0..10] of String;

  TAGWPEConnectRequest = packed record
    Port: Byte;                    // AGWPE Port (z. B. 0 = Port 1)
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
    procedure AGWConnect;
    function DecodeLinkStatus(Text:string):TLinkStatus;
    function PrepareCredentials(const UserId, Password: string): TBytes;
  protected
    procedure Execute; override;
  public
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    Connected: Boolean;
    constructor Create(Config: PTFPConfig);
    procedure Disconnect;
    procedure LoadTNCInit;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Data: string);
    destructor Destroy; override;
  end;

const
  WPEConnectRequestSize = SizeOf(TAGWPEConnectRequest);

var
  ChannelDestCallsign, ChannelFromCallsign: TChannelCallsign;


implementation

{ TAGWPEClient }

constructor TAGWPEClient.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FreeOnTerminate := True;
  Connected := False;
end;

destructor TAGWPEClient.Destroy;
begin
  Connected := False;
  Disconnect;
  inherited Destroy;
end;

{$IFDEF UNIX}
procedure TAGWPEClient.AGWConnect;
var Addr: TInetSockAddr;
    Host: Array [1..10] of THostAddr;
    i: Integer;
begin
  FSocket := fpSocket(AF_INET, SOCK_STREAM, 0);
  if FSocket = -1 then
  begin
    write('Failed to create socket.');
    Exit;
  end;

  Addr.sin_family := AF_INET;

  Addr.sin_port := htons(FPConfig^.AGWServerPort);

  if IsValidIPAddress(FPConfig^.AGWServer) then
    Addr.sin_addr := StrToNetAddr(FPConfig^.AGWServer)
  else
  begin
    i := ResolveName(FPConfig^.AGWServer, Host);
    if i = 0 then
    begin
      writeln('Cannot Resolve '+FPConfig^.AGWServer);
      Exit;
    end;
    Addr.sin_addr := Host[1];
  end;

  if fpConnect(FSocket, @Addr, SizeOf(Addr)) < 0 then
  begin
    Disconnect;
    write('Failed to connect to AGWPE server');
    Exit;
  end;

  Connected := True;
end;
{$ELSE}
procedure TAGWPEClient.AGWConnect;
var
  WSAData: TWSAData;
  Addr: TSockAddrIn;
  HostEnt: PHostEnt;
  SockState: Integer;
begin
  // WinSock initialisieren
  WSAData := Default(TWSAData);
  if WSAStartup($0202, WSAData) <> 0 then
    Exit;

  FSocket := socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
  begin
    WSACleanup();
    Exit;
  end;

  if IsValidIPAddress(FPConfig^.AGWServer) then
  begin
    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FPConfig^.AGWServerPort);
    Addr.sin_addr.S_addr := inet_addr(PAnsiChar(FPConfig^.AGWServer));
  end
  else
  begin
    HostEnt := gethostbyname(PAnsiChar(FPConfig^.AGWServer));
    if HostEnt = nil then
    begin
      closesocket(FSocket);
      WSACleanup();
      Exit;
    end;

    Addr.sin_family := AF_INET;
    Addr.sin_port := htons(FPConfig^.AGWServerPort);
    Addr.sin_addr := PInAddr(HostEnt^.h_addr_list^)^;
  end;

  SockState := connect(FSocket, TSockAddr(Addr), SizeOf(Addr));
  if SockState = SOCKET_ERROR then
    Disconnect;

  Connected := True;
end;
{$ENDIF}

procedure TAGWPEClient.Disconnect;
begin
  try
    {$IFDEF MSWINDOWS}
    closesocket(FSocket);
    WSACleanup();
    {$ENDIF}
    {$IFDEF UNIX}
    fpShutdown(FSocket, SHUT_RDWR);
    {$ENDIF}
  except
     on E: Exception do
     begin
       {$IFDEF UNIX}
       writeln('Receive Data Error: ', E.Message);
       {$ENDIF}
     end;
  end;
end;

procedure TAGWPEClient.Execute;
begin
  try
    AGWConnect;
    SetCallsign;
    // Initialisierung des AGWPE-Clients
    SendStringCommand(0, 1, 'G');
    SendStringCommand(0, 1, 'R');
    if (Length(FPConfig^.AGWServerUsername) > 0) and (Length(FPConfig^.AGWServerPassword) > 0) then
      SendStringCommand(0, 1, 'P');
    while not Terminated do
    begin
      ReceiveData;
      Sleep(5);
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Receive Data Error: ', E.Message);
      {$ENDIF}
    end;
  end;
end;

procedure TAGWPEClient.SendStringCommand(const Channel, Code: byte; const Data: String);
var Request: TAGWPEConnectRequest;
    SentBytes: SizeInt;
    i: Integer;
    ByteCmd: TBytes;
    Command: String;
begin
  if not Connected then
    Exit;

  ByteCmd := TBytes.Create;
  Request := Default(TAGWPEConnectRequest);
  FillChar(Request, WPEConnectRequestSize, 0);
  Request.Port := 0;
  Command := Data;
  try
    // if it' a command, take the first char and then remove the first two
    if Code = 1 then
    begin
      Request.DataKind := Ord(Command[1]);
      Delete(Command, 1, 2);
      Delete(Command, Pos(' ', Command), Length(Command) - Pos(' ', Command) + 1);
      ChannelFromCallsign[Channel] := UpperCase(FPConfig^.Callsign);

      // Register Callsign into AGW Server
      if (Chr(Request.DataKind) = 'X') or (Chr(Request.DataKind) = 'M') then
        ChannelDestCallsign[Channel] := '';

      // Use the destination callsign for the Connect command
      if UpperCase(Chr(Request.DataKind)) = 'C' then
      begin
        Request.DataKind := Ord('C');
        ChannelDestCallsign[Channel] := UpperCase(Command);
      end;

      // Send Authentication Frame for the AGW Server
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
      Request.DataLen := Length(Command)+1;
      SetLength(ByteCmd, Length(Command)+1);
      for i := 0 to Length(Command) do
        ByteCmd[i] := Ord(Command[i+1]);

      // add CR
      ByteCmd[Length(Command)] := 13;
    end;

    Request.Port := 0;
    Request.PID := $00;

    // Set Callsigned as Byte
    if (Length(ChannelDestCallsign[Channel]) > 0) and (Length(ChannelFromCallsign[Channel]) > 0) then
    begin
      Move(ChannelDestCallsign[Channel][1], Request.CallTo[0], Length(ChannelDestCallsign[Channel]));
      Move(ChannelFromCallsign[Channel][1], Request.CallFrom[0], Length(ChannelFromCallsign[Channel]));
    end;

    // Send Header
    {$IFDEF UNIX}
    SentBytes := fpSend(FSocket, @Request, SizeOf(Request), 0);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    SentBytes := send(FSocket, Request, SizeOf(Request), 0);
    {$ENDIF}

    {$IFDEF UNIX}
    if SentBytes < 0 then
      writeln('Error during sending data to AGW');
    {$ENDIF}

    // Send Data
    if (Code = 0) or (Chr(Request.DataKind) = 'P') and (Request.DataLen > 0) then
    begin
      {$IFDEF UNIX}
      SentBytes := fpSend(FSocket, @ByteCmd[0], Length(ByteCmd), 0);
      {$ENDIF}
      {$IFDEF MSWINDOWS}
      SentBytes := send(FSocket, @ByteCmd[0], Length(ByteCmd), 0);
      {$ENDIF}
      if SentBytes < 0 then
      begin
        {$IFDEF UNIX}
        writeln('Error during sending data to AGW');
        {$ENDIF}
      end;
    end;
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('Send String Error: ' + E.Message);
      {$ENDIF}
      Exit;
    end;
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
    Buffer: TBytes;
    TotalReceived, RemainingData, Received, i: Integer;
    Data : String;
    LinkStatus: TLinkStatus;
    TempString: RawByteString;
    MajorVersion, MinorVersion: Word;
begin
  if not Connected then
    Exit;

  Buffer := TBytes.Create;
  Request := Default(TAGWPEConnectRequest);

  SetLength(Buffer, WPEConnectRequestSize);

  TotalReceived := 0;
  RemainingData := WPEConnectRequestSize;

  // read data until request struct is complete
  while TotalReceived < WPEConnectRequestSize do
  begin
    // Empfange Daten aus dem Socket
    {$IFDEF UNIX}
    Received := fpRecv(FSocket, @Buffer[TotalReceived], RemainingData, 0);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Received := recv(FSocket, Buffer[TotalReceived], RemainingData, 0);
    {$ENDIF}


    if Received <= 0 then
      Exit;

    // Daten zählen
    Inc(TotalReceived, Received);
    RemainingData := WPEConnectRequestSize - TotalReceived;
  end;

  Move(Buffer[0], Request, WPEConnectRequestSize);

  // read data
  data := '';
  if Request.DataLen > 0 then
  begin
    SetLength(Buffer, Request.DataLen);
    {$IFDEF UNIX}
    Received := fpRecv(FSocket, @Buffer[0], Request.DataLen, 0);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Received := recv(FSocket, Buffer[0], Request.DataLen, 0);
    {$ENDIF}
    if Received <= 0 then
      Exit;

    SetLength(TempString, Received);
    Move(Buffer[0], TempString[1], Received);
    Data := UTF8Decode(TempString);

    if Chr(Request.DataKind) = 'R' then
    begin
      FPConfig^.AGWVersionMajor := (Word(Buffer[1]) shl 8) or Word(Buffer[0]);
      FPConfig^.AGWVersionMinor := (Word(Buffer[5]) shl 8) or Word(Buffer[4]);
    end;
  end;

  case Chr(Request.DataKind) of
    'C', 'd': // connection response
    begin
      if Length(Data) > 0 then
      begin
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + #27'[32m' + '>>> LINK STATUS: ' + Data + #13#27'[0m';
        LinkStatus := DecodeLinkStatus(Data);
        ChannelStatus[Request.Port+1][6] := LinkStatus[0]; // Status Text CONNECTED, DISCONNECTED, etc
        ChannelStatus[Request.Port+1][7] := LinkStatus[1]; // Call of the other station
      end;
    end;
    'D': // data
    begin
      if Length(Data) > 0 then
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + Data;
    end;
    'I': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data + #13;
    end;
    'm': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data + #13;
    end;
    'S': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data + #13;
    end;
    'U': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data + #13;
    end;
    'T': // Monitoring
    begin
      if Length(Data) > 0 then
        ChannelBuffer[0] := ChannelBuffer[0] + Data + #13;
    end;
  end;
end;

function TAGWPEClient.DecodeLinkStatus(Text:string):TLinkStatus;
var Regex: TRegExpr;
    Status, CallSign: string;
begin
  Regex := TRegExpr.Create;

  try
    Regex.Expression := '^.*\*{3}\s+(CONNECTED|DISCONNECTED|CONNECTED RETRYOUT|DISCONNECTED RETRYOUT|).*Station ?(\S*)?';
    Regex.ModifierI := True;

    if Regex.Exec(Text) then
    begin
      Status := Regex.Match[1];   // CONNECTED, DISCONNECTED, etc.
      CallSign := Regex.Match[2]; // {call}

      Result[0] := Trim(Status);
      Result[1] := Trim(Callsign);
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

