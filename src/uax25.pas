unit uax25;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes, utypes;

type
  TAX25FrameType = (axUnknown, axIFrame, axSFrame, axUFrame);
  TSFrameType = (sfUnknown, sfRR, sfRNR, sfREJ);
  TUFrameType = (ufUnknown, ufSABM, ufDISC, ufUA, ufFRMR);

  TAX25Frame = record
    DestCall: string;
    SrcCall: string;

    PF: Boolean;
    Control: Byte;
    PID: Byte;

    FrameType: TAX25FrameType;
    SFrameType: TSFrameType;
    UFrameType: TUFrameType;

    NS: Integer;
    NR: Integer;

    Payload: AnsiString;
    PayloadRaw: TBytes;
  end;

  { TAX25 }
  TAX25 = class
  private
    function FrameTypeToStr(t : TAX25FrameType) : String;
  public
    procedure PrintAX25Frame(const Frame: TAX25Frame);
    function ParseAX25Frame(const Data: TBytes): TAX25Frame;
    function DecodeCall(const Data: TBytes; offset: Integer): String;
    function EncodeCall(const Call: String; Last: Boolean): TBytes;
    function BuildSABMFrame(const SourceCall, DestCall: String): TBytes;
    function BuildRRFrame(const SourceCall, DestCall: String; NR: Byte; PF: Boolean): TBytes;
    function BuildDISCFrame(const SourceCall, DestCall: String): TBytes;
    function BuildFRMRFrame(const SourceCall, DestCall: String): TBytes;
    function BuildIFrame(const SourceCall, DestCall: String; NS, NR: Byte; Payload: AnsiString; PF: Boolean): TBytes;
    function BuildUAFrame(const SourceCall, DestCall: String; PF: Boolean): TBytes;
    function AddCRCToFrame(var FrameData: TBytes): TBytes;
    function CalculateCRC16CCITT(const Data: TBytes): Word;
    function HasPFBit(Control: Byte): Boolean;
    function GetAX25Monitor(const Frame: TAX25Frame): AnsiString;
    function InitAX25Frame: TAX25Frame;
    function HasCRC(const Data: TBytes): Boolean;
  end;

const
  // U-Frames
  CTRL_SABM = $2F;  // Set Asynchronous Balanced Mode
  CTRL_DISC = $43;  // Disconnect
  CTRL_UA   = $63;  // Unnumbered Acknowledgement
  CTRL_FRMR = $87;  // Frame Reject

  // S-Frames
  CTRL_RR  = $01;   // Receiver Ready (NR im oberen Nibble einfügen)
  CTRL_RNR = $05;   // Receiver Not Ready (NR im oberen Nibble einfügen)
  CTRL_REJ = $09;   // Reject (NR im oberen Nibble einfügen)

  // CRC-Polynom (AX.25 CCITT)
  POLY = $8408;
  CRC_INIT = $FFFF;

implementation

function TAX25.CalculateCRC16CCITT(const Data: TBytes): Word;
var
  i, j: Integer;
  crc: Word;
begin
  crc := $FFFF;

  for i := 0 to Length(Data) - 1 do
  begin
    crc := crc xor Data[i];

    for j := 0 to 7 do
    begin
      if (crc and $0001) <> 0 then
        crc := (crc shr 1) xor $8408
      else
        crc := crc shr 1;
    end;
  end;

  crc := not crc;   // sehr wichtig für AX.25

  Result := crc;
end;

function TAX25.AddCRCToFrame(var FrameData: TBytes): TBytes;
var
  crc: Word;
  len: Integer;
begin
  crc := CalculateCRC16CCITT(FrameData);

  len := Length(FrameData)-1;
  SetLength(FrameData, len + 2);

  FrameData[len] := crc and $FF;        // Low Byte
  FrameData[len + 1] := (crc shr 8) and $FF; // High Byte

  Result := FrameData;
end;

function TAX25.HasPFBit(Control: Byte): Boolean;
begin
  Result := (Control and $10) <> 0;
end;

function TAX25.BuildSABMFrame(const SourceCall, DestCall: String): TBytes;
var
  addrDst, addrSrc : TBytes;
  frame : TBytes;
begin

  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7+7+1+1);

  Move(addrDst[0], frame[0],7);
  Move(addrSrc[0], frame[7],7);

  frame[14] := CTRL_SABM;
  frame[15] := $00; 

  Result := AddCRCToFrame(frame);
end;


function TAX25.BuildRRFrame(const SourceCall, DestCall: String; NR: Byte; PF: Boolean): TBytes;
var
  addrDst, addrSrc : TBytes;
  frame : TBytes;
  controlByte : Byte;
begin
  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7 + 7 + 1 + 1);

  Move(addrDst[0], frame[0], 7);
  Move(addrSrc[0], frame[7], 7);

  controlByte := ((NR and $07) shl 5) or $01;
  if PF then
    controlByte := controlByte or $10;

  frame[14] := controlByte;
  frame[15] := $00; 

  Result := AddCRCToFrame(frame);
end;

function TAX25.BuildDISCFrame(const SourceCall, DestCall: String): TBytes;
var
  addrDst, addrSrc: TBytes;
  frame: TBytes;
begin
  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7 + 7 + 1 + 1);

  Move(addrDst[0], frame[0], 7);
  Move(addrSrc[0], frame[7], 7);

  frame[14] := CTRL_DISC;
  frame[15] := $00; 

  Result := AddCRCToFrame(frame);
end;


function TAX25.BuildFRMRFrame(const SourceCall, DestCall: String): TBytes;
var
  addrDst, addrSrc: TBytes;
  frame: TBytes;
begin
  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7 + 7 + 1 + 1);

  Move(addrDst[0], frame[0], 7);
  Move(addrSrc[0], frame[7], 7);

  frame[14] := CTRL_FRMR;
  frame[15] := $00; 

  Result := AddCRCToFrame(frame);
end;


function TAX25.BuildIFrame(const SourceCall, DestCall: String; NS, NR: Byte; Payload: AnsiString; PF: Boolean): TBytes;
var
  addrDst, addrSrc: TBytes;
  frame: TBytes;
  payloadBytes: TBytes;
  controlByte: Byte;
  payloadLen: Integer;
begin
  // sauber ASCII konvertieren
  payloadBytes := TEncoding.ASCII.GetBytes(Payload+#13);
  payloadLen := Length(payloadBytes) + 1;

  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7 + 7 + 1 + 1 + payloadLen);

  Move(addrDst[0], frame[0], 7);
  Move(addrSrc[0], frame[7], 7);

  // I-Frame Control Byte
  controlByte := ((NS and $07) shl 1) or ((NR and $07) shl 5);
  if PF then
    controlByte := controlByte or $10;

  frame[14] := controlByte;
  frame[15] := $F0;  // PID No layer 3

  if payloadLen > 0 then
    Move(payloadBytes[0], frame[16], payloadLen);

  if payloadLen > 256 then
    raise Exception.Create('AX25 Payload too large');

  // CRC wird von AddCRCToFrame korrekt LowByte + HighByte angehängt
  Result := AddCRCToFrame(frame);
end;

function TAX25.BuildUAFrame(const SourceCall, DestCall: String; PF: Boolean): TBytes;
var
  addrDst, addrSrc : TBytes;
  frame : TBytes;
  ctrl : Byte;
begin
  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7 + 7 + 1 + 1);

  Move(addrDst[0], frame[0], 7);
  Move(addrSrc[0], frame[7], 7);

  ctrl := $63;

  if PF then
    ctrl := ctrl or $10;

  frame[14] := ctrl;
  frame[15] := $00; 

  Result := AddCRCToFrame(frame);
end;

function TAX25.EncodeCall(const Call: string; Last: Boolean): TBytes;
var
  callOnly: string;
  ssid: Integer;
  p, i: Integer;
begin
  SetLength(Result, 7);

  p := Pos('-', Call);
  if p > 0 then
  begin
    callOnly := Copy(Call, 1, p-1);
    ssid := StrToIntDef(Copy(Call, p+1, 2), 0);
  end
  else
  begin
    callOnly := Call;
    ssid := 0;
  end;

  callOnly := UpperCase(callOnly);

  for i := 1 to 6 do
  begin
    if i <= Length(callOnly) then
      Result[i-1] := Ord(callOnly[i]) shl 1
    else
      Result[i-1] := Ord(' ') shl 1;
  end;

  Result[6] := (ssid and $0F) shl 1;
  if Last then
    Result[6] := Result[6] or 1;
end;

function TAX25.DecodeCall(const Data: TBytes; Offset: Integer): string;
var
  i, ssid: Integer;
  c: Char;
  call: string;
  b: Byte;
begin
  Result := '';
  call := '';

  if Length(Data) <= 0 then
    Exit;

  for i := 0 to 5 do
  begin
    b := Data[Offset + i] shr 1;

    if (b >= 32) and (b <= 126) then
    begin
      c := Chr(b);
      if c <> ' ' then
        call := call + c;
    end;
  end;

  // SSID (Byte 6)
  ssid := (Data[Offset + 6] shr 1) and $0F;

  if ssid > 0 then
    call := call + '-' + IntToStr(ssid);

  Result := call;
end;

function TAX25.HasCRC(const Data: TBytes): Boolean;
var
  frameWithoutCRC: TBytes;
  calculatedCRC, frameCRC: Word;
  len: Integer;
begin
  Result := False;

  if Length(Data) < 3 then Exit; // mindestens 1 Byte Payload + 2 Byte CRC nötig

  // Frame ohne CRC
  len := Length(Data) - 2;
  SetLength(frameWithoutCRC, len);
  Move(Data[0], frameWithoutCRC[0], len);

  // CRC über Frame berechnen
  calculatedCRC := CalculateCRC16CCITT(frameWithoutCRC);

  // CRC aus Frame auslesen (letzte 2 Bytes)
  frameCRC := Data[len] or (Data[len + 1] shl 8);

  // Vergleich
  Result := calculatedCRC = frameCRC;
end;

function TAX25.ParseAX25Frame(const Data: TBytes): TAX25Frame;
var
  ctrl: Byte;
  infoStart, payloadLen: Integer;
begin
  Result := InitAX25Frame;  // Frame initialisieren

  if Length(Data) <= 0 then Exit;

  Result.DestCall := DecodeCall(Data, 0);
  Result.SrcCall  := DecodeCall(Data, 7);

  ctrl := Data[14];

  // P/F Bit
  Result.PF := (ctrl and $10) <> 0;
  Result.Control := ctrl and not $10;

  if (ctrl and $01) = 0 then
  begin
    // I-Frame
    Result.FrameType := axIFrame;
    Result.NS := (ctrl shr 1) and $07;
    Result.NR := (ctrl shr 5) and $07;
    Result.PID := Data[15];
    infoStart := 16;
  end
  else if (ctrl and $03) = $01 then
  begin
    // S-Frame
    Result.FrameType := axSFrame;
    Result.NR := (ctrl shr 5) and $07;
    case (ctrl shr 2) and $03 of
      0: Result.SFrameType := sfRR;
      1: Result.SFrameType := sfRNR;
      2: Result.SFrameType := sfREJ;
    else
      Result.SFrameType := sfUnknown;
    end;
    infoStart := 15;
  end
  else
  begin
    // U-Frame
    Result.FrameType := axUFrame;
    case Result.Control of
      CTRL_SABM: Result.UFrameType := ufSABM;
      CTRL_DISC: Result.UFrameType := ufDISC;
      CTRL_UA:   Result.UFrameType := ufUA;
      CTRL_FRMR: Result.UFrameType := ufFRMR;
    else
      Result.UFrameType := ufUnknown;
    end;
    infoStart := 15;
  end;

  // Payload
  payloadLen := Length(Data) - infoStart;
  if HasCRC(Data) and (payloadLen >= 2) then
  begin
    // Letzte 2 Bytes als CRC
    payloadLen := payloadLen - 2;
    SetLength(Result.PayloadRaw, payloadLen + 2);
    if payloadLen > 0 then
      Move(Data[infoStart], Result.PayloadRaw[0], payloadLen);
    Result.PayloadRaw[payloadLen] := Data[infoStart + payloadLen];       // CRC Low
    Result.PayloadRaw[payloadLen + 1] := Data[infoStart + payloadLen + 1]; // CRC High
  end
  else
  begin
    // Kein CRC
    SetLength(Result.PayloadRaw, payloadLen);
    if payloadLen > 0 then
      Move(Data[infoStart], Result.PayloadRaw[0], payloadLen);
  end;

  // Payload als AnsiString ohne CRC
  if payloadLen > 0 then
    SetString(Result.Payload, PAnsiChar(@Result.PayloadRaw[0]), payloadLen)
  else
    Result.Payload := '';
end;

function TAX25.FrameTypeToStr(t : TAX25FrameType) : string;
begin
  case t of
    axIFrame: Result := 'I Frame';
    axSFrame: Result := 'S Frame';
    axUFrame: Result := 'U Frame';
  else
    Result := 'Unknown';
  end;
end;

procedure TAX25.PrintAX25Frame(const Frame: TAX25Frame);
var
  i: Integer;
  payloadLen: Integer;
  crcLow, crcHigh: Byte;
begin
  Writeln('---- AX25 FRAME ----');
  Writeln('Destination : ', Frame.DestCall);
  Writeln('Source      : ', Frame.SrcCall);

  Writeln('Frame Type  : ', FrameTypeToStr(Frame.FrameType));
  Writeln('Control     : 0x', IntToHex(Frame.Control and not $10, 2));
  Writeln('PF          : 0x', IntToHex(Frame.Control and $10, 2));
  Writeln('PID         : 0x', IntToHex(Frame.PID, 2));

  if Frame.FrameType = axSFrame then
  begin
    Writeln('NS (Send)   : ', (Frame.Control shr 1) and $07);
    Writeln('NR (Recv)   : ', (Frame.Control shr 5) and $07);
  end;

  if Frame.FrameType = axIFrame then
  begin
    Writeln('NS (Send)   : ', Frame.NS);
    Writeln('NR (Recv)   : ', Frame.NR);
  end;

  payloadLen := Length(Frame.PayloadRaw);
  if payloadLen > 2 then
    payloadLen := payloadLen - 2  // letzte 2 Bytes sind CRC
  else
    payloadLen := 0;

  Writeln('Payload Length (excl. CRC): ', payloadLen);

  if payloadLen > 0 then
  begin
    Writeln('Payload (Text): ', Copy(Frame.Payload, 1, payloadLen));

    Write('Payload (Hex) : ');
    for i := 0 to payloadLen - 1 do
      Write(IntToHex(Frame.PayloadRaw[i], 2), ' ');
    Writeln;
  end
  else
    Writeln('Payload      : <none>');

  // CRC ausgeben
  if HasCRC(Frame.PayloadRaw) then
  begin
    crcLow := Frame.PayloadRaw[High(Frame.PayloadRaw) - 1];
    crcHigh := Frame.PayloadRaw[High(Frame.PayloadRaw)];
    Writeln('CRC (Hex)    : ', IntToHex(crcLow, 2), ' ', IntToHex(crcHigh, 2));
  end
  else
    Writeln('CRC          : <none>');

  Writeln('--------------------');
end;

function TAX25.GetAX25Monitor(const Frame: TAX25Frame): AnsiString;
var
  line, frameTypeStr: string;
  pfChar: string;
  payloadLen: Integer;
  payloadData: AnsiString;
begin
  Result := '';

  // PF: '+' wenn PF=1, sonst '-'
  if Frame.PF then
    pfChar := '+'
  else
    pfChar := '-';

  // Frame-Type Text
  case Frame.FrameType of
    axIFrame:
      frameTypeStr := Format('I%d%d%s', [Frame.NS, Frame.NR, pfChar]);
    axSFrame:
      case Frame.SFrameType of
        sfRR:  frameTypeStr := Format('RR%d%s', [Frame.NR, pfChar]);
        sfRNR: frameTypeStr := Format('RNR%d%s', [Frame.NR, pfChar]);
        sfREJ: frameTypeStr := Format('REJ%d%s', [Frame.NR, pfChar]);
      else
        frameTypeStr := Format('S%d%s', [Frame.NR, pfChar]);
      end;
    axUFrame:
      frameTypeStr := 'UI';
  else
    frameTypeStr := '?';
  end;

  // Kopfzeile
  line := Format('fm %s to %s ctl %s pid %d' + #13,
    [Frame.SrcCall, Frame.DestCall, frameTypeStr, Frame.PID]);

  // Payload ohne CRC
  payloadLen := Length(Frame.PayloadRaw);
  if payloadLen > 2 then
  begin
    payloadLen := payloadLen - 3;  // letzte 2 Bytes = CRC
    SetString(payloadData, PAnsiChar(@Frame.PayloadRaw[0]), payloadLen);
    line := line + '  ' + RemoveANSICodes(payloadData) + #13;
  end;

  if Length(line) > 0 then
    Result := line;
end;

function TAX25.InitAX25Frame: TAX25Frame;
begin
  Result.DestCall := '';
  Result.SrcCall := '';

  Result.PF := False;
  Result.Control := 0;
  Result.PID := 0;

  Result.FrameType := axUnknown;
  Result.SFrameType := sfUnknown;
  Result.UFrameType := ufUnknown;

  Result.NS := 0;
  Result.NR := 0;

  Result.Payload := '';
  SetLength(Result.PayloadRaw, 0);
end;
end.


