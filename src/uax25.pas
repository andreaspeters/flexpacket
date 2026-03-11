unit uax25;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
  { TAx25Frame - AX.25 Frame Structure }
  TAx25Frame = record
    SourceCall: string;
    DestCall: string;
    ViaCalls: TStringList;
    Control: Byte;
    PID: Byte;
    Info: TBytes;
    CRC: Word;
  end;

  { TAx25Kiss - KISS Interface Wrapper }
  TAx25Kiss = class
  private
    FEndOfFrame: Byte;
    FEscapeByte: Byte;
    FTransFrameEnd: Byte;
    FTransEscapeByte: Byte;
    procedure SetEndOfFrame(Value: Byte);
    procedure SetEscapeByte(Value: Byte);
    procedure SetTransFrameEnd(Value: Byte);
    procedure SetTransEscapeByte(Value: Byte);
  public
    constructor Create;
    function BuildKissFrame(const Channel, Command: Byte; const Data: TBytes): TBytes;
    function DecodeKissFrame(const Data: TBytes; out Channel: Byte; out FrameData: TBytes): Boolean;
    function BuildHDLCFRAME(const SourceCall, DestCall, ViaCalls: string; Control, PID: Byte; const Info: TBytes): TBytes;
    function BuildAX25Frame(const Frame: TAx25Frame): TBytes;
    function ValidateCRC(const Data: TBytes; out CRCValid: Boolean): Boolean;
    function ComputeCRC(const Data: TBytes): Word;
    function CallToBytes(const S: string; out SSID: Byte): TBytes;
    function BytesToCall(const Data: TBytes; InOffset: Integer; out Call: string; out SSID: Byte): Integer;
    property EndOfFrame: Byte read FEndOfFrame write SetEndOfFrame;
    property EscapeByte: Byte read FEscapeByte write SetEscapeByte;
    property TransFrameEnd: Byte read FTransFrameEnd write SetTransFrameEnd;
    property TransEscapeByte: Byte read FTransEscapeByte write SetTransEscapeByte;
  end;

implementation

constructor TAx25Kiss.Create;
begin
  FEndOfFrame := $C0;
  FEscapeByte := $DB;
  FTransFrameEnd := $DC;
  FTransEscapeByte := $DD;
end;

procedure TAx25Kiss.SetEndOfFrame(Value: Byte);
begin
  FEndOfFrame := Value;
end;

procedure TAx25Kiss.SetEscapeByte(Value: Byte);
begin
  FEscapeByte := Value;
end;

procedure TAx25Kiss.SetTransFrameEnd(Value: Byte);
begin
  FTransFrameEnd := Value;
end;

procedure TAx25Kiss.SetTransEscapeByte(Value: Byte);
begin
  FTransEscapeByte := Value;
end;

function TAx25Kiss.BuildKissFrame(const Channel, Command: Byte; const Data: TBytes): TBytes;
var
  i: Integer;
  DataLen: Integer;
  Offset: Integer;
  ResultLen: Integer;
begin
  if Length(Data) = 0 then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  DataLen := Length(Data);
  ResultLen := 2 + DataLen;
  SetLength(Result, ResultLen);

  Offset := 0;
  Result[Offset] := Channel;
  Offset := Offset + 1;

  Result[Offset] := Command;
  Offset := Offset + 1;

  for i := 0 to DataLen - 1 do
  begin
    Result[Offset + i] := Data[i];
  end;
end;

function TAx25Kiss.DecodeKissFrame(const Data: TBytes; out Channel: Byte; out FrameData: TBytes): Boolean;
var
  i, j: Integer;
  FrameLen: Integer;
  InEscape: Boolean;
  Frame: TBytes;
  Offset: Integer;
begin
  FrameLen := Length(Data);
  InEscape := False;
  SetLength(Frame, 0);
  SetLength(FrameData, 0);

  for i := 0 to FrameLen - 1 do
  begin
    if InEscape then
    begin
      if Data[i] = FTransFrameEnd then
        Frame[Length(Frame) - 1] := FEndOfFrame
      else if Data[i] = FTransEscapeByte then
        Frame[Length(Frame) - 1] := FEscapeByte;

      InEscape := False;
    end
    else
    begin
      if Data[i] = FEscapeByte then
        InEscape := True
      else if Data[i] <> FEndOfFrame then
      begin
        SetLength(Frame, Length(Frame) + 1);
        Frame[Length(Frame) - 1] := Data[i];
      end
      else
      begin
        Channel := Frame[0];
        SetLength(FrameData, Length(Frame) - 1);
        Offset := 0;
        for j := 0 to Length(FrameData) - 1 do
          FrameData[Offset + j] := Frame[Offset + j + 1];
        Result := True;
        Exit;
      end;
    end;
  end;

  Result := False;
end;

function TAx25Kiss.BuildHDLCFRAME(const SourceCall, DestCall, ViaCalls: string;
  Control, PID: Byte; const Info: TBytes): TBytes;
var
  ViaList: TStringList;
  i, j: Integer;
  Offset, InfoLen: Integer;
  ResultLen: Integer;
  Source, Dest: TBytes;
  SSID1, SSID2: Byte;
  SourceBytes, DestBytes: Integer;
  TotalLen: Integer;
begin
  ViaList := TStringList.Create;
  try
    if ViaCalls = '' then
      ViaList.Clear
    else
    begin
      ViaList.Delimiter := ',';
      ViaList.QuoteChar := ' ';
      ViaList.DelimitedText := ViaCalls;
    end;

    SetLength(Source, 7);
    Source := CallToBytes(SourceCall, SSID1);

    SetLength(Dest, 7);
    Dest := CallToBytes(DestCall, SSID2);

    SourceBytes := Length(Source);
    DestBytes := Length(Dest);

    InfoLen := Length(Info);
    TotalLen := SourceBytes + DestBytes + 2 + InfoLen + 2;
    SetLength(Result, TotalLen);

    Offset := 0;

    for i := 0 to SourceBytes - 1 do
    begin
      Result[Offset + i] := Source[i];
      Inc(Offset);
    end;

    for i := 0 to DestBytes - 1 do
    begin
      Result[Offset + i] := Dest[i];
      Inc(Offset);
    end;

    Result[Offset] := Control;
    Inc(Offset);

    Result[Offset] := PID;
    Inc(Offset);

    for i := 0 to InfoLen - 1 do
    begin
      Result[Offset + i] := Info[i];
      Inc(Offset);
    end;

  finally
    ViaList.Free;
  end;
end;

function TAx25Kiss.BuildAX25Frame(const Frame: TAx25Frame): TBytes;
var
  i, j: Integer;
  Offset, InfoLen, HeaderLen: Integer;
  Header: TBytes;
  Source, Dest: TBytes;
  SSID1, SSID2: Byte;
  SourceBytes, DestBytes: Integer;
  FrameLen: Integer;
begin
  FrameLen := 0;
  Offset := 0;
  SetLength(Result, 0);

  if (Frame.SourceCall = '') or (Frame.DestCall = '') then
  begin
    SetLength(Result, 0);
    Exit;
  end;

  Source := CallToBytes(Frame.SourceCall, SSID1);
  Dest := CallToBytes(Frame.DestCall, SSID2);

  SourceBytes := Length(Source);
  DestBytes := Length(Dest);

  HeaderLen := 7 + 7;
  InfoLen := Length(Frame.Info);

  FrameLen := HeaderLen + 3 + InfoLen + 2;
  SetLength(Result, FrameLen);

  Offset := 0;

  for i := 0 to SourceBytes - 1 do
  begin
    Result[Offset + i] := Source[i];
    Inc(Offset);
  end;

  for i := 0 to DestBytes - 1 do
  begin
    Result[Offset + i] := Dest[i];
    Inc(Offset);
  end;

  Result[Offset] := Frame.Control;
  Inc(Offset);

  Result[Offset] := Frame.PID;
  Inc(Offset);

  for i := 0 to InfoLen - 1 do
  begin
    Result[Offset + i] := Frame.Info[i];
    Inc(Offset);
  end;
end;

function TAx25Kiss.ValidateCRC(const Data: TBytes; out CRCValid: Boolean): Boolean;
var
  ComputedCRC, ReceivedCRC: Word;
begin
  ComputedCRC := ComputeCRC(Data);
  if Length(Data) < 2 then
  begin
    CRCValid := False;
    Exit;
  end;

  ReceivedCRC := (Word(Data[Length(Data) - 2]) shl 8) or Word(Data[Length(Data) - 1]);
  CRCValid := (ComputedCRC = ReceivedCRC);
  Result := CRCValid;
end;

function TAx25Kiss.ComputeCRC(const Data: TBytes): Word;
var
  i, j: Integer;
  CRC: Word;
begin
  CRC := 0;

  for i := 0 to Length(Data) - 1 do
  begin
    CRC := CRC xor (Word(Data[i]) shl 8);
    for j := 0 to 7 do
    begin
      if (CRC and $8000) <> 0 then
        CRC := (CRC shl 1) xor $1021
      else
        CRC := CRC shl 1;
    end;
  end;

  CRC := (CRC and $FE01) xor ($C000 shr (16 - $C000));
  Result := CRC;
end;

function TAx25Kiss.CallToBytes(const S: string; out SSID: Byte): TBytes;
var
  i: Integer;
  CharCode: Byte;
  Bytes: array[0..6] of Byte;
  CallLen: Integer;
  MaxLen: Integer;
begin
  SSID := 0;
  SetLength(Result, 7);

  CallLen := Length(S);
  MaxLen := 6;
  for i := 1 to CallLen do
    Bytes[i] := 0;

  for i := 1 to CallLen do
    if i <= MaxLen then
    begin
      if (S[i] >= 'A') and (S[i] <= 'Z') then
        Bytes[i] := Byte(Ord(S[i]) - Ord('A') + 1)
      else if (S[i] >= '0') and (S[i] <= '9') then
        Bytes[i] := Byte(Ord(S[i]) - Ord('0') + 11)
      else
      begin
        if S[i] >= 'a' then
          Bytes[i] := Byte(Ord(S[i]) - Ord('a') + 1)
        else if S[i] >= ' ' then
          Bytes[i] := Byte(Ord(S[i]) - Ord(' ') + 27)
        else
          Bytes[i] := 0;
      end;
    end;

  for i := 0 to 6 do
    Result[i] := Bytes[i];
end;

function TAx25Kiss.BytesToCall(const Data: TBytes; InOffset: Integer; out Call: string; out SSID: Byte): Integer;
var
  i: Integer;
  CharCode: Byte;
  Shift: Byte;
begin
  Call := '';
  SSID := 0;

  for i := 0 to 5 do
  begin
    CharCode := Data[InOffset + i] and $7F;

    if CharCode = 0 then
      Break;

    if CharCode <= 10 then
      Call := Call + Char(Char(Ord('A') + CharCode - 1))
    else if CharCode <= 36 then
      Call := Call + Char(Char(Ord('0') + CharCode - 11))
    else if CharCode <= 62 then
      Call := Call + Char(Char(Ord(' ') + CharCode - 26))
    else
      Call := Call + '#';
  end;

  Shift := 0;
  for i := 0 to 5 do
    if (Data[InOffset + i] and $80) <> 0 then
      Shift := (Shift or Data[i]) and $0F;

  SSID := Shift;

  Exit(i);
end;

end.
