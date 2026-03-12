unit ukissmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls, uax25,
  Graphics, utypes, RegExpr, uhostmode, baseunix, sockets, bluetooth;

type
  { TKISSMode }
  TKISSFrame = record
    Port    : Byte;
    Command : Byte;
    AX25Raw : TBytes;
  end;

  TChannel = Record
    Connected: Boolean;
    DestinationCall: String;
    NS: Byte;
    NR: Byte;
  end;

  TKISSMode = class(THostmode)
  private
    FSerial: Integer;
    FConnected: boolean;
    FEnableKISSMode: boolean;
    FCheckKISSConnect: boolean;
    AX25: TAX25;
    TNCPort: array[0..10] of TChannel;
    procedure ProcessFrame(Data: TBytes);
    procedure ProcessTextFrame(const Text: string);
    procedure ProcessCommandFrame(const Cmd: string);
    procedure ProcessStatusFrame(const Data: string);
    procedure SetTNCStatusMessage(msg: String);
    procedure SendBytesWithKISS(const Channel: byte; const Data: TBytes);
    procedure ReceiveData;
    procedure SendKISSEscapeCommand(const Command: string);
    procedure ProcessAX25(const KISSData: TBytes);
    function ConnectRFCOMM: Boolean;
    function SendKISSFrame(const Channel: Byte; const Data: TBytes): Boolean;
    function SendCommandFrame(const Cmd: Integer; const Command: PChar): Boolean;
    function RecvSocketData(var Buffer: Byte; var BytesReceived: Integer): Boolean;
    function WaitForData(Timeout: Cardinal): Boolean;
    function BuildKISSFrame(const Channel, Command: byte; const Data: TBytes): TBytes;
    function ParseKISSFrame(const Data: TBytes): TKISSFrame;
  protected
    procedure Execute; override;
  public
    property Connected: boolean read FConnected;
    destructor Destroy; override;
    procedure SendStringCommand(const Channel, Code: byte; const Command: string);
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    function SendSocketData(Data: TBytes): Boolean;
    function GetConnected: boolean;
  end;

const
  FEND  = $C0;
  FESC  = $DB;
  TFEND = $DC;
  TFESC = $DD;

implementation

function TKISSMode.ParseKISSFrame(const Data: TBytes): TKISSFrame;
var kissByte : Byte;
begin
  if Length(Data) = 0 then
    Exit;

  kissByte := Data[0];

  // Port und Command extrahieren
  Result.Port    := kissByte shr 4;
  Result.Command := kissByte and $0F;

  // AX25 Frame ist alles nach dem ersten Byte
  if Length(Data) > 1 then
  begin
    SetLength(Result.AX25Raw, Length(Data)-1);
    Move(Data[1], Result.AX25Raw[0], Length(Data)-1);
  end
  else
    SetLength(Result.AX25Raw,0);
end;

function TKISSMode.GetConnected: boolean;
begin
  Result := FConnected;
end;

function TKISSMode.SendSocketData(Data: TBytes): Boolean;
var i: Integer;
begin
  Result := False;

  if FSerial < 0 then
    exit;

  for i := 0 to Length(Data) - 1 do
  begin
    if fpwrite(FSerial, Data[i], 1) <= 0 then
      exit;
  end;

  Result := True;
end;


// Oder für ASCII-Darstellung:
function BytesToASCII(Data: TBytes): string;
var
  i: Integer;
  c: Char;
begin
  Result := '';
  for i := 0 to High(Data) do
  begin
    if (Data[i] >= 32) and (Data[i] <= 126) then
      c := Chr(Data[i])
    else
      c := '.';
    Result := Result + c;
  end;
end;


procedure TKISSMode.ReceiveData;
var buffer: array[0..65535] of byte;
    BytesReceived: ssize_t;
    FrameData: TBytes;
    i, j: Integer;
    Port: Byte;
    AX25Frame: TAX25Frame;
    KISSFrame: TKISSFrame;
begin
  try
    if FSerial < 0 then Exit;
    if not WaitForData(500) then Exit;

    BytesReceived := fpread(FSerial, @buffer, SizeOf(buffer));
    if BytesReceived <= 0 then Exit;

    SetLength(FrameData, BytesReceived);

    j := 0;
    i := 0;

    while i < BytesReceived do
    begin
      case buffer[i] of

        FESC:
          begin
            Inc(i);
            if i < BytesReceived then
            begin
              if buffer[i] = TFEND then
                FrameData[j] := FEND
              else if buffer[i] = TFESC then
                FrameData[j] := FESC
              else
              begin
                Inc(i);
                Continue;
              end;

              Inc(j);
            end;
          end;

        FEND:
          begin
            // Frame separator ignorieren
          end;

      else
        begin
          FrameData[j] := buffer[i];
          Inc(j);
        end;

      end;

      Inc(i);
    end;

    SetLength(FrameData, j);

    // Answer with RR
    ProcessAX25(FrameData);

    KISSFrame := ParseKISSFrame(FrameData);

    // --- Debug Ausgabe ---
//    WriteLn('KISS RAW (', BytesReceived, ' bytes):');
//    for i := 0 to j - 1 do
//      Write(IntToHex(KISSFrame.AX25Raw[i], 2), ' ');
//    Writeln;
//
//    WriteLn('KISS DECODED: ', BytesToASCII(KISSFrame.AX25Raw));
    // --- Debug ENDE Ausgabe ---

    // --- AX.25 Parser ---
    try
      AX25Frame := AX25.ParseAX25Frame(KISSFrame.AX25Raw);
      if AX25Frame.FrameType = axIFrame then
      begin
        Port := KISSFrame.Port+1;

        if Length(AX25Frame.Payload) > 0 then
        begin
          if not TNCPort[Port].Connected then
          begin
            ChannelBuffer[Port] := ChannelBuffer[Port] + #13#10#27'[32m' + '>>> LINK STATUS: Connected to ' + AX25Frame.SrcCall + #27'[0m'#13#10;
            ChannelStatus[Port][6] := 'CONNECTED';
            ChannelStatus[Port][7] := AX25Frame.SrcCall;

            TNCPort[Port].DestinationCall := AX25Frame.SrcCall;
            TNCPort[Port].Connected := True;
          end;
          ChannelBuffer[Port] := ChannelBuffer[Port] + AX25Frame.Payload;
        end;
      end;
      AX25.PrintAX25Frame(AX25Frame);
    except
      on E: Exception do
        Writeln('Parse Error: ', E.Message);
    end;

  except
    on E: Exception do
      Writeln('ReceiveData Error: ', E.Message);
  end;
end;



procedure TKISSMode.ProcessAX25(const KISSData: TBytes);
var
  KISSFrame: TKISSFrame;
  AXFrame: TAX25Frame;
  NR: Byte;
  AXSend, Frame: TBytes;
begin
  KISSFrame := ParseKISSFrame(KISSData);

  try
    AXFrame := AX25.ParseAX25Frame(KISSFrame.AX25Raw);
  except
    on E: Exception do
    begin
      WriteLn('AX25 Parse Error: ', E.Message);
      Exit;
    end;
  end;

  AX25.PrintAX25Frame(AXFrame);

    case AXFrame.FrameType of
      axIFrame:
        begin
          NR := (AXFrame.NS + 1) and $07;
          TNCPort[KISSFrame.Port].NR := NR;

          if TNCPort[KISSFrame.Port].NS = 0 then
            TNCPort[KISSFrame.Port].NS := NR;

          AXSend := AX25.BuildRRFrame(AXFrame.SrcCall, AXFrame.DestCall, NR);
        end;

    axSFrame:
      begin
        case AXFrame.SFrameType of
          sfRR:
            WriteLn('Empfänger bereit (RR) – sende ggf. weitere I-Frames');
          sfRNR:
            WriteLn('Empfänger nicht bereit (RNR) – Stoppe Sending');
          sfREJ:
            begin
              WriteLn('REJ empfangen – retransmit ab NR=', AXFrame.NR);
            end;
        end;
      end;

    axUFrame:
      begin
        case AXFrame.Control of
          CTRL_SABM: WriteLn('SABM empfangen – Verbindung aufbauen');
          CTRL_DISC: WriteLn('DISC empfangen – Verbindung trennen');
          CTRL_UA:   WriteLn('UA empfangen – Unnumbered Acknowledgment');
          CTRL_FRMR: WriteLn('FRMR empfangen – Frame Reject / Fehlerbericht');
        else
          WriteLn('U-Frame empfangen: unbekannter Typ (Control=0x', IntToHex(AXFrame.Control,2), ')');
        end;
      end;

  else
    WriteLn('Unbekannter AX25 Frame Type');
  end;

  if Length(AXSend) > 0 then
  begin
    Frame := BuildKISSFrame(KISSFrame.Port, 0, AXSend);
    SendKISSFrame(KISSFrame.Port, @Frame[0]);
  end;
end;

function TKISSMode.SendCommandFrame(const Cmd: Integer; const Command: PChar): Boolean;
var
  cmdBytes, Frame: TBytes;
  i, p: Integer;
  Port, CmdByte: Byte;

begin
  Result := False;

  Port := (Cmd and $F0) shr 4;
  CmdByte := (Cmd and $0F);

  if StrLen(Command) = 0 then Exit;

  cmdBytes := BytesOf(Command);

  SetLength(Frame, 0);

  Frame := BuildKISSFrame(Port, 0, cmdBytes);

  Result := SendKISSFrame(Port, @Frame[0]);
end;

function TKISSMode.ConnectRFCOMM: Boolean;
var loc_addr: sockaddr_rc;
  opt: Integer;
  s: Integer;
  status: Integer;
  bt_addr: array[0..18] of char;
  bd_addr: bdaddr_t;
  channel: Byte;
begin
  Result := False;
  SetTNCStatusMessage('Connecting to RFCOMM');

  bt_addr := '38:D2:00:01:2F:3A';
  channel := 1;
  opt := SizeOf(loc_addr);

  s := fpsocket(AF_BLUETOOTH, SOCK_STREAM, BTPROTO_RFCOMM);

  if s < 0 then
  begin
    {$IFDEF UNIX}
    WriteLn('Socket creation failed');
    {$ENDIF}
    exit;
  end;

  str2ba(@bt_addr[0], @bd_addr);

  loc_addr.rc_family := AF_BLUETOOTH;
  loc_addr.rc_bdaddr := bd_addr;
  loc_addr.rc_channel := channel;

  {$IFDEF UNIX}
  writeln('Trying connection to ', bt_addr);
  {$ENDIF}

  status := fpconnect(s, @loc_addr, opt);

  channel := 1;
  writeln('channel: ', channel, '  result: ', status);
  if status = 0 then
  begin
    FSerial := s;
    Result := True;
    {$IFDEF UNIX}
    WriteLn('RFCOMM connected successfully');
    {$ENDIF}
  end;
end;

function TKISSMode.BuildKISSFrame(const Channel, Command: Byte; const Data: TBytes): TBytes;
var i, p: Integer;
    Frame: TBytes;
    ByteToEscape: Byte;
begin
  Frame := TBytes.Create;
  SetLength(Frame, Length(Data) * 2 + 3);

  p := 0;
  Frame[p] := $C0;
  Inc(p);
  Frame[p] := $00;
  Inc(p);

  for i := 0 to High(Data) do
  begin
    ByteToEscape := Data[i];

    if ByteToEscape = $C0 then
    begin
      Frame[p] := $DB;
      Inc(p);
      Frame[p] := $DC;
      Inc(p);
    end
    else if ByteToEscape = $DB then
    begin
      Frame[p] := $DB;
      Inc(p);
      Frame[p] := $DD;
      Inc(p);
    end
    else
    begin
      Frame[p] := ByteToEscape;
      Inc(p);
    end;
  end;

  Frame[p] := $C0;
  Inc(p);

  SetLength(Frame, p);
  Result := Frame;
end;

function TKISSMode.SendKISSFrame(const Channel: Byte; const Data: TBytes): Boolean;
var
  bytesSent: ssize_t;
  i: Integer;
  c: Char;
begin
  Result := False;

  if FSerial < 0 then Exit;
  if Length(Data) = 0 then Exit;

  // --- Debug-Ausgabe als Char ---
  Write('Sending KISS Frame (Channel ', Channel, ', Length ', Length(Data), '): ');
  for i := 0 to High(Data) do
  begin
    // Byte zu Char konvertieren; nicht druckbare Zeichen als '.' darstellen
    if (Data[i] >= 32) and (Data[i] <= 126) then
      c := Chr(Data[i])
    else
      c := '.';
    Write(c);
  end;
  Writeln;

  // --- Debug: Hex-Dump ---
  Write('Sending KISS Frame (Channel ', Channel, ', Length ', Length(Data), '): ');
  for i := 0 to High(Data) do
    Write(IntToHex(Data[i], 2), ' ');
  Writeln;

  // --- Serielles Senden ---
  bytesSent := fpwrite(FSerial, @Data[0], Length(Data));
  if bytesSent <> Length(Data) then Exit;

  Result := True;
end;

function TKISSMode.RecvSocketData(var Buffer: Byte; var BytesReceived: Integer): Boolean;
var bytesRecv: size_t;
begin
  Result := False;

  if FSerial < 0 then
    exit;

  bytesRecv := fpread(FSerial, @Buffer, SizeOf(Buffer));
  BytesReceived := bytesRecv;

  Result := bytesRecv > 0;
end;

function TKISSMode.WaitForData(Timeout: Cardinal): Boolean;
var
  StartTime: QWord;
  ReadFds: TFDSet;
  TimeVal: TTimeVal;
begin
  Result := False;
  StartTime := GetTickCount64;

  while (FSerial >= 0) and (GetTickCount64 - StartTime < Timeout) do
  begin
    fpFD_ZERO(ReadFds);
    fpFD_SET(FSerial, ReadFds);

    TimeVal.tv_sec := Timeout div 1000;
    TimeVal.tv_usec := (Timeout mod 1000) * 1000;

    if fpSelect(FSerial + 1, @ReadFds, nil, nil, @TimeVal) > 0 then
    begin
      Result := True;
      Exit;
    end;

    Sleep(10);
  end;
end;

procedure TKISSMode.ProcessFrame(Data: TBytes);
var i: Integer;
  Ch: Byte;
  TempFrame: TBytes;
  TempLen: Integer;
begin
  if Length(Data) = 0 then Exit;

  Ch := Data[1];
  SetLength(TempFrame, Length(Data) - 3);

  for i := 3 to Length(Data) - 1 do
    TempFrame[i - 3] := Data[i];

  if Ch = 1 then
    ProcessCommandFrame(AnsiString(@TempFrame[0]))
  else if Ch = 2 then
    ProcessTextFrame(AnsiString(@TempFrame[0]))
  else if Ch = 3 then
    SetTNCStatusMessage(AnsiString(@TempFrame[0]))
  else
    SetTNCStatusMessage(AnsiString(@TempFrame[0]));
end;

procedure TKISSMode.SendBytesWithKISS(const Channel: Byte; const Data: TBytes);
var Frame: TBytes;
begin
  Frame := BuildKISSFrame(Channel, 0, Data);
  SendKISSFrame(Channel, @Frame[0]);
end;

procedure TKISSMode.ProcessTextFrame(const Text: string);
var Line, Pos: string;
  PosIndex: Integer;
begin
  PosIndex := 1;
  while PosIndex <= Length(Text) do
  begin
    Pos := '';

    while (PosIndex <= Length(Text)) and (Text[PosIndex] <> #13) and (Text[PosIndex] <> #10) do
    begin
      Pos := Pos + Text[PosIndex];
      Inc(PosIndex);
    end;

    if (PosIndex <= Length(Text)) and ((Text[PosIndex] = #13) or (Text[PosIndex] = #10)) then
    begin
      PosIndex := PosIndex + 1;
    end;

    if Pos <> '' then
      ProcessStatusFrame(Pos);

    Inc(PosIndex);
  end;
end;

procedure TKISSMode.ProcessCommandFrame(const Cmd: string);
var Regex: TRegExpr;
  Index: Integer;
begin
  Regex := TRegExpr.Create;

  try
    if Regex.Exec(Cmd) then
    begin
      if Regex.SubExprMatchCount > 0 then
      begin
        for Index := 0 to Min(FPConfig^.MaxChannels-1, Regex.SubExprMatchCount-1) do
          FPConfig^.Callsign := Regex.Match[Index+1];
      end;
    end;
  finally
    Regex.Free;
  end;
end;

procedure TKISSMode.ProcessStatusFrame(const Data: string);
begin
  SetTNCStatusMessage('Status: '+Data);
end;

procedure TKISSMode.SetTNCStatusMessage(msg: String);
var i: Integer;
begin
  for i := 0 to FPConfig^.MaxChannels do
    ChannelStatus[i][9] := msg;
end;

procedure TKISSMode.SendKISSEscapeCommand(const Command: string);
var Bytes: TBytes;
  i, p: Integer;
  Frame: TBytes;
  ByteToEscape: Byte;
const
  FEND  = $C0;
  FESC  = $DB;
  TFEND = $DC;
  TFESC = $DD;
begin
  Bytes := BytesOf(Command);
  SetLength(Frame, Length(Bytes) * 2 + 3);

  p := 0;
  Frame[p] := FEND;
  Inc(p);
  Frame[p] := 0;
  Inc(p);

  for i := 0 to High(Bytes) do
  begin
    ByteToEscape := Bytes[i];

    if ByteToEscape = FEND then
    begin
      Frame[p] := FESC;
      Inc(p);
      Frame[p] := TFEND;
      Inc(p);
    end
    else if ByteToEscape = FESC then
    begin
      Frame[p] := FESC;
      Inc(p);
      Frame[p] := TFESC;
      Inc(p);
    end
    else
    begin
      Frame[p] := ByteToEscape;
      Inc(p);
    end;
  end;

  Frame[p] := FEND;
  Inc(p);

  SetLength(Frame, p);

  SendKISSFrame(0, @Frame[0]);
  SysUtils.Sleep(200);
end;

procedure TKISSMode.SendStringCommand(const Channel, Code: byte; const Command: string);
var Bytes: TBytes;
  i: Integer;
  AX, Frame: TBytes;

begin
  if Code = 1 then
     AX := AX25.BuildSABMFrame(FPConfig^.Callsign, 'DB0APK-7');

  if (Code = 0) and (TNCPort[Channel].Connected) and (Length(TNCPort[Channel].DestinationCall) > 0) then
  begin
    AX := AX25.BuildIFrame(FPConfig^.Callsign, TNCPort[Channel].DestinationCall, TNCPort[Channel].NS, TNCPort[Channel].NR, Command);

    inc(TNCPort[Channel].NS);
    if TNCPort[Channel].NS > 8 then
      TNCPort[Channel].NS := 0;
  end;

  if Length(AX) > 0 then
  begin
    Frame := BuildKISSFrame(Channel, 0, AX);
    SendKISSFrame(Channel, @Frame[0]);
  end;
end;

procedure TKISSMode.SendByteCommand(const Channel, Code: Byte; const Data: TBytes);
var Frame, Encoded: TBytes;
  CR: Integer;
  cmdBytes: TBytes;
  i: Integer;
begin
  if not Connected then
    Exit;

  if FSerial < 0 then
    Exit;

  SetLength(cmdBytes, Length(Data));
  for i := 0 to High(Data) do
    cmdBytes[i] := Data[i];

  CR := 0;

  if Code = 0 then
  begin
    Encoded := cmdBytes;
    SendKISSFrame(Channel, @Encoded[0]);
  end
  else if Code = 1 then
  begin
    Encoded := BuildKISSFrame(Channel, 0, cmdBytes);
    SetLength(Encoded, Length(Encoded) + 1);
    Encoded[High(Encoded)] := $0D;
    SendKISSFrame(Channel, @Encoded[0]);
  end;
end;

procedure TKISSMode.Execute;
var LastSendTimeG, LastSendTimeL: Cardinal;
  resp: String;
  i: Integer;
begin
  repeat
    SetTNCStatusMessage('Connecting');
    if FPConfig^.ComPort <> '' then
    begin
      if ConnectRFCOMM then
        break;
    end;
    Sleep(200);
  until False;


  FConnected := True;

  SetTNCStatusMessage('TNC Ready');

  LastSendTimeG := GetTickCount64;
  LastSendTimeL := GetTickCount64;

  while not Terminated do
  begin
    try
      ReceiveData;
      if (GetTickCount64 - LastSendTimeG) >= 1000 then
      begin
        SendG;
        LastSendTimeG := GetTickCount64;
      end;
      if (GetTickCount64 - LastSendTimeL) >= 10000 then
      begin
        SendL;
        LastSendTimeL := GetTickCount64;
      end;
      Sleep(5);
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Receive Data Error: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;

  FConnected := False;
end;

destructor TKISSMode.Destroy;
begin
  FConnected := False;
  inherited Destroy;
end;

end.
