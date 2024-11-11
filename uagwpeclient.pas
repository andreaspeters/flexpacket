unit uagwpeclient;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  Graphics, StrUtils, utypes, RegExpr, Sockets;

type
  { TAGWPEClient }

  PTFPConfig = ^TFPConfig;
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
    DataLen: Integer;     // Länge der Nutzdaten (4 Bytes, hier 0, da keine Daten)
    UserReserved: array[0..3] of Byte; // weitere 4 reservierte Bytes, setzen auf 0
  end;

  TAGWPEClient = class(TThread)
  private
    FSocket: TSocket;
    FBuffer: string;
    FOnDataReceived: TNotifyEvent;
    FPConfig: PTFPConfig;
    ChannelDestCallsign: TChannelCallsign;
    procedure ReceiveData;
    function ReadPacket: string;
    procedure Connect;
    procedure Disconnect;
  protected
    procedure Execute; override;
  public
    constructor Create(Config: PTFPConfig);
    procedure SendByteCommand(Channel, Code: byte; Command: string);
    destructor Destroy; override;
    property OnDataReceived: TNotifyEvent read FOnDataReceived write FOnDataReceived;
  end;

implementation

{ TAGWPEClient }

constructor TAGWPEClient.Create(Config: PTFPConfig);
begin
  write('-');
  inherited Create(True);
  FPConfig := Config;
  FreeOnTerminate := True;
  Resume;
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

//  Addr.sin_addr.s_addr := (Octets[0] shl 24) or (Octets[1] shl 16) or (Octets[2] shl 8) or Octets[3];
  Addr.sin_addr.s_addr := 0;

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
    // Initialisierung des AGWPE-Clients
    SendByteCommand(0, 1, 'G');
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

procedure TAGWPEClient.ReceiveData;
var
  Packet: string;
begin
  Packet := ReadPacket;
  if Packet <> '' then
  begin
    FBuffer := FBuffer + Packet;
    if Assigned(FOnDataReceived) then
      FOnDataReceived(Self);
  end;
end;

procedure TAGWPEClient.SendByteCommand(Channel, Code: byte; Command: string);
var Request: TAGWPEConnectRequest;
    SentBytes: SizeInt;
    i: Integer;
    ByteCmd: array of Byte;
begin
  FillChar(Request, SizeOf(Request), 0);

  // if it' a command, take the first char and then remove the first two
  if Code = 1 then
  begin
    Request.DataKind := Ord(Command[1]);
    Delete(Command, 1, 2);
    Delete(Command, Pos(' ', Command), Length(Command) - Pos(' ', Command) + 1);
    ChannelDestCallsign[Channel] := Command
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

  Request.Port := Channel;
  Request.PID := $00;

  for i := 1 to Length(ChannelDestCallsign[Channel]) do
    Request.CallTo[i-1] := Ord(ChannelDestCallsign[Channel][i]);
  for i := 1 to Length(FPConfig^.Callsign) do
    Request.CallFrom[i-1] := Ord(FPConfig^.Callsign[i]);

  SentBytes := fpSend(FSocket, @Request, SizeOf(Request), 0);
  if SentBytes < 0 then
    writeln('Error during sending data to AGW');

  if Code = 0 then
  begin
    SentBytes := fpSend(FSocket, @ByteCmd, SizeOf(ByteCmd), 0);
    if SentBytes < 0 then
      writeln('Error during sending data to AGW');
  end;
end;



function TAGWPEClient.ReadPacket: string;
var
  Buffer: string;
  BytesRead: SizeInt;
begin
  Result := '';
  SetLength(Buffer, 1024);  // Maximale Paketgröße
  BytesRead := fpRecv(FSocket, @Buffer, Length(Buffer), 0);
  //if BytesRead > 0 then
  //begin
  //  SetLength(Buffer, BytesRead);  // Trim the buffer to the actual data length
  //  Result := Buffer;
  //end;
end;

end.

