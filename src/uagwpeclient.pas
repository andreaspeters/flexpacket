unit uagwpeclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  Graphics, utypes, RegExpr,
  {$IFDEF UNIX}Sockets, netdb, BaseUnix{$ELSE}WinSock{$ENDIF};

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
    procedure SetStatusMessage(const Msg: String);
    function ReceiveExact(var Buffer: TBytes; Count: Integer): Boolean;
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
    procedure SendStringCommand(const Channel, Code: byte; const Data: string;
      AppendCR: Boolean = True);
    destructor Destroy; override;
  end;

const
  WPEConnectRequestSize = SizeOf(TAGWPEConnectRequest);
  MAX_AGW_DATA_LENGTH = 1024 * 1024;

var
  ChannelDestCallsign, ChannelFromCallsign: TChannelCallsign;


implementation

{ TAGWPEClient }

procedure TAGWPEClient.SetStatusMessage(const Msg: String);
var
  I: Integer;
begin
  for I := 0 to FPConfig^.MaxChannels do
    ChannelStatus[I][9] := Msg;
end;

constructor TAGWPEClient.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FreeOnTerminate := False;
  FSocket := TSocket(-1);
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

  FillChar(Addr, SizeOf(Addr), 0);
  FillChar(Host, SizeOf(Host), 0);
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
      Disconnect;
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
  begin
    Disconnect;
    Exit;
  end;

  Connected := True;
end;
{$ENDIF}

procedure TAGWPEClient.Disconnect;
begin
  Connected := False;
  if FSocket = TSocket(-1) then
    Exit;

  try
    {$IFDEF MSWINDOWS}
    closesocket(FSocket);
    WSACleanup();
    {$ENDIF}
    {$IFDEF UNIX}
    fpShutdown(FSocket, SHUT_RDWR);
    fpClose(FSocket);
    {$ENDIF}
  except
     on E: Exception do
     begin
       {$IFDEF UNIX}
       writeln('Receive Data Error: ', E.Message);
       {$ENDIF}
     end;
  end;
  FSocket := TSocket(-1);
end;

procedure TAGWPEClient.Execute;
begin
  while not Terminated do
  begin
    try
      SetStatusMessage('AGW connecting to ' + FPConfig^.AGWServer + ':' +
        IntToStr(FPConfig^.AGWServerPort));
      AGWConnect;

      if Connected then
      begin
        SetStatusMessage('AGW initializing');

        // Initialisierung des AGWPE-Clients
        SendStringCommand(0, 1, 'X');
        SendStringCommand(0, 1, 'm');
        SendStringCommand(0, 1, 'G');
        SendStringCommand(0, 1, 'R');
        if (Length(FPConfig^.AGWServerUsername) > 0) and
          (Length(FPConfig^.AGWServerPassword) > 0) then
          SendStringCommand(0, 1, 'P');

        SetStatusMessage('AGW ready');
      end;

      while not Terminated and Connected do
      begin
        ReceiveData;
        Sleep(5);
      end;
    except
      on E: Exception do
      begin
        SetStatusMessage('AGW error: ' + E.Message);
        {$IFDEF UNIX}
        writeln('Receive Data Error: ', E.Message);
        {$ENDIF}
      end;
    end;

    Disconnect;
    if not Terminated then
    begin
      SetStatusMessage('AGW connection failed; retrying');
      Sleep(1000);
    end;
  end;

  Connected := False;
end;

procedure TAGWPEClient.SendStringCommand(const Channel, Code: byte;
  const Data: String; AppendCR: Boolean);
var Request: TAGWPEConnectRequest;
    SentBytes: SizeInt;
    i: Integer;
    ByteCmd: TBytes;
    Command: String;
begin
  if (not Connected) or (Channel > MAX_CHANNEL) then
    Exit;

  if (Code = 1) and (Data = '') then
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
      if Chr(Request.DataKind) = 'X' then
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
      Request.DataLen := Length(Command) + Ord(AppendCR);
      SetLength(ByteCmd, Request.DataLen);
      for i := 1 to Length(Command) do
        ByteCmd[i - 1] := Ord(Command[i]);

      if AppendCR then
        ByteCmd[Length(Command)] := 13;
    end;

    Request.Port := 0;
    Request.PID := $00;

    // Set Callsigned as Byte
    if Length(ChannelDestCallsign[Channel]) > 0 then
      Move(ChannelDestCallsign[Channel][1], Request.CallTo[0],
        Min(Length(ChannelDestCallsign[Channel]), SizeOf(Request.CallTo)));

    if Length(ChannelFromCallsign[Channel]) > 0 then
      Move(ChannelFromCallsign[Channel][1], Request.CallFrom[0],
        Min(Length(ChannelFromCallsign[Channel]), SizeOf(Request.CallFrom)));

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
    if ((Code = 0) or (Chr(Request.DataKind) = 'P')) and
      (Request.DataLen > 0) and (Length(ByteCmd) > 0) then
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
  CopyLength: Integer;
begin
  SetLength(Result, 510);
  FillByte(Result[0], Length(Result), 0);

  CopyLength := Min(Length(UserId), 255);
  if CopyLength > 0 then
    Move(UserId[1], Result[0], CopyLength);

  CopyLength := Min(Length(Password), 255);
  if CopyLength > 0 then
    Move(Password[1], Result[255], CopyLength);
end;

function TAGWPEClient.ReceiveExact(var Buffer: TBytes; Count: Integer): Boolean;
var
  Received, TotalReceived: Integer;
begin
  Result := False;
  TotalReceived := 0;

  while (TotalReceived < Count) and not Terminated do
  begin
    {$IFDEF UNIX}
    Received := fpRecv(FSocket, @Buffer[TotalReceived], Count - TotalReceived, 0);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Received := recv(FSocket, Buffer[TotalReceived], Count - TotalReceived, 0);
    {$ENDIF}

    if Received <= 0 then
    begin
      Connected := False;
      Terminate;
      Exit;
    end;
    Inc(TotalReceived, Received);
  end;

  Result := TotalReceived = Count;
end;

procedure TAGWPEClient.ReceiveData;
var Request: TAGWPEConnectRequest;
    Buffer: TBytes;
    PayloadLength: Integer;
    Data : String;
    LinkStatus: TLinkStatus;
    TempString: RawByteString;
begin
  if not Connected then
    Exit;

  TempString := Default(RawByteString);
  Buffer := TBytes.Create;
  Request := Default(TAGWPEConnectRequest);

  SetLength(Buffer, WPEConnectRequestSize);
  if not ReceiveExact(Buffer, WPEConnectRequestSize) then
    Exit;

  Move(Buffer[0], Request, WPEConnectRequestSize);

  // read data
  data := '';
  PayloadLength := Request.DataLen;
  if (PayloadLength < 0) or (PayloadLength > MAX_AGW_DATA_LENGTH) then
  begin
    Connected := False;
    Terminate;
    Exit;
  end;

  if PayloadLength > 0 then
  begin
    SetLength(Buffer, PayloadLength);
    if not ReceiveExact(Buffer, PayloadLength) then
      Exit;

    SetLength(TempString, PayloadLength);
    Move(Buffer[0], TempString[1], PayloadLength);
    Data := UTF8Decode(TempString);

    if (Chr(Request.DataKind) = 'R') and (PayloadLength >= 6) then
    begin
      FPConfig^.AGWVersionMajor := (Word(Buffer[1]) shl 8) or Word(Buffer[0]);
      FPConfig^.AGWVersionMinor := (Word(Buffer[5]) shl 8) or Word(Buffer[4]);
    end;
  end;

  case Chr(Request.DataKind) of
    'C', 'd': // connection response
    begin
      if (Length(Data) > 0) and (Request.Port < MAX_CHANNEL) then
      begin
        ChannelBuffer[Request.Port+1] := ChannelBuffer[Request.Port+1] + #13#27'[32m' + '>>> LINK STATUS: ' + Data + #27'[0m'#13;
        LinkStatus := DecodeLinkStatus(Data);
        ChannelStatus[Request.Port+1][6] := LinkStatus[0]; // Status Text CONNECTED, DISCONNECTED, etc
        ChannelStatus[Request.Port+1][7] := LinkStatus[1]; // Call of the other station
      end;
    end;
    'D': // data
    begin
      if (Length(Data) > 0) and (Request.Port < MAX_CHANNEL) then
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
  Result := Default(TLinkStatus);
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

end.

