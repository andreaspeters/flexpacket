unit uax25;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Classes;

type
  { TAX25 }
  TAX25FrameType = (
    axUnknown,
    axIFrame,
    axSFrame,
    axUFrame
  );

  TAX25Frame = record
    DestCall  : string;
    SrcCall   : string;

    Control   : Byte;
    PID       : Byte;

    FrameType : TAX25FrameType;

    NS : Integer;
    NR : Integer;

    Payload : AnsiString;
    PayloadRaw : TBytes;
  end;


  TAX25 = class
  private
    function FrameTypeToStr(t : TAX25FrameType) : String;
    function CalcCRC(const data: TBytes): Word;
  public
    procedure PrintAX25Frame(const Frame: TAX25Frame);
    function ParseAX25Frame(const Data: TBytes): TAX25Frame;
    function DecodeCall(const Data: TBytes; offset: Integer): String;
    function EncodeCall(const Call: String; Last: Boolean): TBytes;
    function BuildSABMFrame(const SourceCall, DestCall: String): TBytes;
  end;

const
  CTRL_SABM = $2F;
  POLY = $8408;

implementation


function TAX25.BuildSABMFrame(const SourceCall, DestCall: String): TBytes;
var
  addrDst, addrSrc : TBytes;
  frame : TBytes;
  crc : Word;
begin

  addrDst := EncodeCall(DestCall, False);
  addrSrc := EncodeCall(SourceCall, True);

  SetLength(frame, 7+7+1);

  Move(addrDst[0], frame[0],7);
  Move(addrSrc[0], frame[7],7);

  frame[14] := CTRL_SABM;

  crc := CalcCRC(frame);

  SetLength(frame, Length(frame)+2);

  frame[Length(frame)-2] := crc and $FF;
  frame[Length(frame)-1] := (crc shr 8) and $FF;

  Result := frame;
end;

function TAX25.EncodeCall(const Call: string; Last: Boolean): TBytes;
var
  callOnly : string;
  ssid : Integer;
  p : Integer;
  i : Integer;
begin
  SetLength(Result,7);

  p := Pos('-',Call);

  if p>0 then
  begin
    callOnly := Copy(Call,1,p-1);
    ssid := StrToIntDef(Copy(Call,p+1,2),0);
  end
  else
  begin
    callOnly := Call;
    ssid := 0;
  end;

  callOnly := UpperCase(callOnly);

  for i:=1 to 6 do
  begin
    if i<=Length(callOnly) then
      Result[i-1] := Ord(callOnly[i]) shl 1
    else
      Result[i-1] := Ord(' ') shl 1;
  end;

  Result[6] := (ssid and $0F) shl 1;
  Result[6] := Result[6] or $60;

  if Last then
    Result[6] := Result[6] or 1;
end;


function TAX25.CalcCRC(const data: TBytes): Word;
var
  crc : Word;
  i,j : Integer;
begin
  crc := $FFFF;

  for i := 0 to High(data) do
  begin
    crc := crc xor data[i];
    for j := 0 to 7 do
    begin
      if (crc and 1) <> 0 then
        crc := (crc shr 1) xor POLY
      else
        crc := crc shr 1;
    end;
  end;

  Result := not crc;
end;

function TAX25.DecodeCall(const Data: TBytes; Offset: Integer): string;
var
  i, ssid: Integer;
  c: Char;
  call: string;
begin
  call := '';
  for i := 0 to 5 do
  begin
    c := Chr(Data[Offset + i] shr 1); // ASCII zurück
    if c <> ' ' then
      call := call + c;
  end;

  // SSID aus Bits 1-4 von Byte6
  ssid := (Data[Offset + 6] shr 1) and $0F;
  if ssid > 0 then
    call := call + '-' + IntToStr(ssid);

  Result := call;
end;


function TAX25.ParseAX25Frame(const Data: TBytes): TAX25Frame;
var
  ctrl : Byte;
  infoStart : Integer;
begin

  Result.DestCall := DecodeCall(Data, 0);
  Result.SrcCall  := DecodeCall(Data, 7);

  ctrl := Data[14];
  Result.Control := ctrl;

  if (ctrl and $01) = 0 then
  begin
    Result.FrameType := axIFrame;

    Result.NS := (ctrl shr 1) and $07;
    Result.NR := (ctrl shr 5) and $07;

    Result.PID := Data[15];

    infoStart := 16;
  end
  else
  if (ctrl and $03) = $01 then
  begin
    Result.FrameType := axSFrame;

    Result.NR := (ctrl shr 5) and $07;

    infoStart := 15;
  end
  else
  begin
    Result.FrameType := axUFrame;

    infoStart := 15;
  end;

  if Length(Data) > infoStart+2 then
  begin
    SetLength(Result.PayloadRaw, Length(Data)-infoStart-2);
    Move(Data[infoStart], Result.PayloadRaw[0], Length(Result.PayloadRaw));

    SetString(Result.Payload, PAnsiChar(@Result.PayloadRaw[0]), Length(Result.PayloadRaw));
  end
  else
  begin
    SetLength(Result.PayloadRaw,0);
    Result.Payload := '';
  end;

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
  i : Integer;
  payloadStr : string;
begin

  Writeln('---- AX25 FRAME ----');

  Writeln('Destination : ', Frame.DestCall);
  Writeln('Source      : ', Frame.SrcCall);

  Writeln('Frame Type  : ', FrameTypeToStr(Frame.FrameType));
  Writeln('Control     : 0x', IntToHex(Frame.Control,2));
  Writeln('PID         : 0x', IntToHex(Frame.PID,2));

  if Frame.FrameType = axIFrame then
  begin
    Writeln('NS (Send)   : ', Frame.NS);
    Writeln('NR (Recv)   : ', Frame.NR);
  end;

  if Length(Frame.PayloadRaw) > 0 then
  begin
    Writeln('Payload (Text): ', Frame.Payload);

    Write('Payload (Hex) : ');
    for i := 0 to High(Frame.PayloadRaw) do
      Write(IntToHex(Frame.PayloadRaw[i],2),' ');
    Writeln;
  end
  else
    Writeln('Payload      : <none>');

  Writeln('--------------------');

end;


end.


