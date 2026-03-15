unit ukissmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls, uax25,
  Graphics, utypes, RegExpr, uhostmode, baseunix, sockets, bluetooth,
  Generics.Collections;

type
  TKISSFrame = record
    Port    : Byte;
    Command : Byte;
    AX25Raw : TBytes;
  end;

  TChannel = Record
    Connected: Boolean;
    DestinationCall: String;
    T1: Cardinal;
    T2: Cardinal;
    T1Running: Boolean;
    T2Running: Boolean;
    LastFrames: array[0..7] of TBytes;
    Last: TBytes;
    VS: Byte;  // Sendefolgenummer
    VR: Byte; // Empfangsfolgezählers
    RXBuffer : array[0..7] of AnsiString;
  end;

  { TKISSMode }
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
    procedure SendRR(Channel: Byte; PF: Boolean; NR: Byte);
    procedure SendUA(Channel: Byte; PF: Boolean);
    procedure SendI(Channel: Byte; PF: Boolean; Command: AnsiString);
    procedure DebugAX25FromKISS(const Data: TBytes);
    function ConnectRFCOMM: Boolean;
    function SendKISSFrame(const Channel: Byte; const Data: TBytes): Boolean;
    function SendCommandFrame(const Cmd: Integer; const Command: PChar): Boolean;
    function RecvSocketData(var Buffer: Byte; var BytesReceived: Integer): Boolean;
    function WaitForData(Timeout: Cardinal): Boolean;
    function BuildKISSFrame(const Channel, Command: byte; const Data: TBytes): TBytes;
    function ParseKISSFrame(const Data: TBytes): TKISSFrame;
    function PadCallsign(Call: string): string;
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
  except
    on E: Exception do
      Writeln('ReceiveData Error: ', E.Message);
  end;
end;


procedure TKISSMode.ProcessAX25(const KISSData: TBytes);
var
  KISSFrame: TKISSFrame;
  AXFrame: TAX25Frame;
  Port: Byte;
  AX, Frame: TBytes;
begin
  KISSFrame := ParseKISSFrame(KISSData);

  try
    AXFrame := AX25.ParseAX25Frame(KISSFrame.AX25Raw);
    AX25.PrintAX25Frame(AXFrame);
  except
    on E: Exception do
    begin
      Writeln('AX25 Parse Error: ', E.Message);
      Exit;
    end;
  end;

  Port := KISSFrame.Port + 1;

  ChannelBuffer[0] := ChannelBuffer[0] + AX25.GetAX25Monitor(AXFrame);

  case AXFrame.FrameType of

    axIFrame:
    begin
      if not TNCPort[Port].Connected then
        Exit;

      if AXFrame.NS = TNCPort[Port].VR then
      begin
        ChannelBuffer[Port] := ChannelBuffer[Port] + AXFrame.Payload;
        TNCPort[Port].VR := (TNCPort[Port].VR + 1) mod 8;
      end;

      while TNCPort[Port].VS <> AXFrame.NR do
      begin
        TNCPort[Port].LastFrames[TNCPort[Port].VS] := nil;
        TNCPort[Port].VS := (TNCPort[Port].VS + 1) mod 8;
      end;

      TNCPort[Port].T1Running := False;

      SendRR(Port, AXFrame.PF, TNCPort[Port].VR);
    end;

    axSFrame:
    begin
      case AXFrame.SFrameType of

        sfRR:
        begin
          while TNCPort[Port].VS <> AXFrame.NR do
          begin
            TNCPort[Port].LastFrames[TNCPort[Port].VS] := nil;
            TNCPort[Port].VS := (TNCPort[Port].VS + 1) mod 8;
          end;

          TNCPort[Port].T1Running := False;
        end;

        sfRNR:
        begin
          TNCPort[Port].T1Running := False;
        end;

        sfREJ:
        begin
          AX := TNCPort[Port].LastFrames[AXFrame.NR];

          if Length(AX) > 0 then
          begin
            Frame := BuildKISSFrame(Port, 0, AX);
            SendKISSFrame(Port, @Frame[0]);
          end;

          TNCPort[Port].VS := AXFrame.NR;
        end;

      end;
    end;

    axUFrame:
      begin
        case AXFrame.Control of
          CTRL_SABM:
            begin
              Writeln('SABM empfangen – Verbindung aufbauen');
              SendUA(Port, AXFrame.PF);
            end;
          CTRL_DISC:
            begin
              Writeln('DISC empfangen - Verbindung trennen');

              if not AXFrame.PF then
                 SendUA(Port, AXFrame.PF);

              // T1 stoppen, falls NR bestätigt
              if AXFrame.NR = TNCPort[Port].VS then
                TNCPort[Port].T1Running := False;

              if TNCPort[Port].Connected then
              begin
                ChannelBuffer[Port] := ChannelBuffer[Port] + #13#10#27'[32m' + '>>> LINK STATUS: Disconnected from ' + TNCPort[Port].DestinationCall + #27'[0m'#13#10;
                ChannelStatus[Port][6] := 'DISCONNECTED';
                TNCPort[Port].Connected := False;
                TNCPort[Port] := Default(TChannel);
              end;
            end;
          CTRL_UA:
            begin
              Writeln('UA empfangen - Unnumbered Acknowledgment');

              TNCPort[Port].T2 := GetTickCount64;
              TNCPort[Port].T2Running := True;

              if not TNCPort[Port].Connected then
              begin
                TNCPort[Port].DestinationCall := AXFrame.SrcCall;
                TNCPort[Port].Connected := True;
                ChannelBuffer[Port] := ChannelBuffer[Port] + #13#10#27'[32m' + '>>> LINK STATUS: Connected to ' + AXFrame.SrcCall + #27'[0m'#13#10;
                ChannelStatus[Port][6] := 'CONNECTED';
                ChannelStatus[Port][7] := AXFrame.SrcCall;
              end;
            end;
          CTRL_FRMR: Writeln('FRMR empfangen - Frame Reject / Fehlerbericht');
        else
          Writeln('U-Frame empfangen: unbekannter Typ (Control=0x', IntToHex(AXFrame.Control,2), ')');
        end;
      end;

  else
    Writeln('Unbekannter AX25 Frame Type');
  end;
end;

procedure TKISSMode.SendRR(Channel: Byte; PF: Boolean; NR: Byte);
var AXSend, Frame: TBytes;
begin
  if not TNCPort[Channel].Connected then
    Exit;

  AXSend := AX25.BuildRRFrame(FPConfig^.Callsign, TNCPort[Channel].DestinationCall, NR, PF);

  Frame := BuildKISSFrame(Channel, 0, AXSend);
  SendKISSFrame(Channel, Frame);

  TNCPort[Channel].T2Running := False;
end;

procedure TKISSMode.SendUA(Channel: Byte; PF: Boolean);
var AXSend, Frame: TBytes;
begin
  if not TNCPort[Channel].Connected then
    Exit;

  AXSend := AX25.BuildUAFrame(FPConfig^.Callsign, TNCPort[Channel].DestinationCall, PF);

  Frame := BuildKISSFrame(Channel, 0, AXSend);
  SendKISSFrame(Channel, Frame);
end;

procedure TKISSMode.SendI(Channel: Byte; PF: Boolean; Command: AnsiString);
var AXSend, Frame: TBytes;
    NS: Byte;
begin
  if not TNCPort[Channel].Connected then
    Exit;

  NS := TNCPort[Channel].VS;

  AXSend := AX25.BuildIFrame(
              FPConfig^.Callsign,
              TNCPort[Channel].DestinationCall,
              NS,
              TNCPort[Channel].VR,
              Command + #13,
              PF);

  TNCPort[Channel].LastFrames[NS] := AXSend;
  TNCPort[Channel].Last := AXSend;

  TNCPort[Channel].VS := (TNCPort[Channel].VS + 1) mod 8;

  TNCPort[Channel].T1 := GetTickCount64;
  TNCPort[Channel].T1Running := True;

  Frame := BuildKISSFrame(Channel, 0, AXSend);
  SendKISSFrame(Channel, Frame);
end;

function TKISSMode.SendCommandFrame(const Cmd: Integer; const Command: PChar): Boolean;
var
  cmdBytes, Frame: TBytes;
  Port, CmdByte: Byte;

begin
  Result := False;

  Port := (Cmd and $F0) shr 4;
  CmdByte := (Cmd and $0F);

  if StrLen(Command) = 0 then Exit;

  cmdBytes := BytesOf(Command);

  SetLength(Frame, 0);

  Frame := BuildKISSFrame(Port, 0, cmdBytes);

  Result := SendKISSFrame(Port, Frame);
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
  if (Length(FPConfig^.KISSBluetoothMac) <> 17) or (FPConfig^.KISSBluetoothMac = '00:00:00:00:00:00') then
    Exit;

  SetTNCStatusMessage('Connecting to ' + FPConfig^.KISSBluetoothName);

  bt_addr := FPConfig^.KISSBluetoothMac;
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
    CurrentLen: Integer = 0;
begin
  Frame := TBytes.Create;
  SetLength(Frame, Length(Data) * 2 + 3);

  p := 0;
  Frame[p] := FEND; Inc(p);
  Frame[p] := (Channel shl 4) or Command; Inc(p);

  for i := 0 to High(Data) do
  begin
    ByteToEscape := Data[i];

    if CurrentLen >= 253 then
    begin
      Frame[p] := FEND; Inc(p);
      Frame[p] := 0; Inc(p);
      CurrentLen := 0;
    end;

    if ByteToEscape = FEND then
    begin
      Frame[p] := FESC; Inc(p);
      Frame[p] := TFEND; Inc(p);
    end
    else if ByteToEscape = FESC then
    begin
      Frame[p] := FESC; Inc(p);
      Frame[p] := TFESC; Inc(p);
    end
    else
    begin
      Frame[p] := ByteToEscape; Inc(p);
      Inc(CurrentLen);
    end;
  end;

  Frame[p] := FEND; Inc(p);

  SetLength(Frame, p);
  Result := Frame;
end;

procedure TKISSMode.DebugAX25FromKISS(const Data: TBytes);
var
  AXFrame: TAX25Frame;
  AXData: TBytes;
  StartIdx: Integer;
begin
  // Prüfen, dass Data lang genug ist
  if Length(Data) < 2 then Exit;

  // Index nach KISS-Byte (erstes Byte = FEND, zweites Byte = KISS-Byte)
  StartIdx := 2;

  // Restliche Bytes sind AX.25
  SetLength(AXData, Length(Data) - StartIdx);
  if Length(AXData) > 0 then
    Move(Data[StartIdx], AXData[0], Length(AXData));

  // AX.25 Frame parsen
  AXFrame := AX25.ParseAX25Frame(AXData);

  // Debug ausgeben
  AX25.PrintAX25Frame(AXFrame);
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

  DebugAX25FromKISS(Data);

  // --- Debug-Ausgabe als Char ---
  Write('KISS Frame: ');
  for i := 0 to High(Data) do
    Write(IntToHex(Data[i], 2), ' ');
  Writeln;

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
  SendKISSFrame(Channel, Frame);
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

  SendKISSFrame(0, Frame);
  SysUtils.Sleep(200);
end;

function TKISSMode.PadCallsign(Call: string): string;
begin
  Result := Call + StringOfChar(' ', 6 - Length(Call));
end;

procedure TKISSMode.SendStringCommand(const Channel, Code: byte; const Command: string);
var AX, Frame: TBytes;
    AXFrame: TAX25Frame;
    Regex: TRegExpr;
begin
  AX := nil;

  if Code = 1 then
  begin
    if Length(Command) <= 0 then
      Exit;

    Regex := TRegExpr.Create;
    try
      writeln(Command);
      Regex.Expression := '^(\S) (\S*)(?:\svia (\S+))?';
      Regex.ModifierI := False;
      if Regex.Exec(Command) then
      begin
        if Regex.SubExprMatchCount < 2 then
          Exit;

        if (UpperCase(Regex.Match[1]) = 'C') and not (TNCPort[Channel].Connected) then
          AX := AX25.BuildSABMFrame(FPConfig^.Callsign, Regex.Match[2]);

        if (UpperCase(Regex.Match[1]) = 'D') and (TNCPort[Channel].Connected) then
          AX := AX25.BuildDISCFrame(FPConfig^.Callsign, TNCPort[Channel].DestinationCall);

      end;
    except
      {$IFDEF UNIX}
      on E: Exception do
        Writeln('SendStringCommand: Command Error:', E.Message);
      {$ENDIF}
    end;

    TNCPort[Channel].T1 := GetTickCount64;
    TNCPort[Channel].T1Running := True;
  end;

  if Code = 0 then
  begin
    if not TNCPort[Channel].Connected then Exit;
    SendI(Channel, AXFrame.PF, Command);
  end;

  if Length(AX) > 0 then
  begin
    Frame := BuildKISSFrame(Channel, 0, AX);

    // --- Debug: Parse und Ausgabe vor dem Senden ---
    try
      AXFrame := AX25.ParseAX25Frame(AX);   // Parse das rohe AX.25 Frame
      AX25.PrintAX25Frame(AXFrame);         // Ausgabe für Debug
      ChannelBuffer[0] := ChannelBuffer[0] + AX25.GetAX25Monitor(AXFrame);
    except
      on E: Exception do
        Writeln('AX25 Parse Error (before sending): ', E.Message);
    end;

    SendKISSFrame(Channel, Frame);
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
var resp: String;
    i: Integer;
    AX, Frame: TBytes;
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

  while not Terminated do
  begin
    try
      ReceiveData;

      for i := 0 to 10 do
      begin

        // Retransmission falls T1 abgelaufen
        if TNCPort[i].T1Running and ((GetTickCount64 - TNCPort[i].T1) >= 40000) then
        begin
          Writeln('T1 Timeout Port ', i);

          // Send last frame again
          AX := TNCPort[i].Last;
          if Length(AX) > 0 then
          begin
            Frame := BuildKISSFrame(i, 0, AX);
            SendKISSFrame(i, Frame);
          end;

          TNCPort[i].T1 := GetTickCount64;
        end;

        // T2 Send RR
        if TNCPort[i].T2Running and ((GetTickCount64 - TNCPort[i].T2) >= 5000) then
        begin
          SendRR(i, False,  TNCPort[i].VR);
        end;

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
