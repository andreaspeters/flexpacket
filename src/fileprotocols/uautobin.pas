unit uautobin;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RegExpr;

function CalculateAutoBinCRC(const Data: TBytes): Integer;
function DateTimeToMSDOSTime(const ADateTime: TDateTime): LongWord;
function CreateAutoBinHeader(const FileName: String; const FileSize: Int64;
  const CRC: Integer; const FileDateTime: TDateTime): String;
function ParseAutoBinHeader(const Head: String): TStrings;

implementation

function CalculateAutoBinCRC(const Data: TBytes): Integer;
const
  POLYNOMIAL = $1021;
var
  I, J, CRC: Integer;
begin
  CRC := $FFFF;
  if Length(Data) = 0 then
    Exit(0);
  for I := 0 to High(Data) do
  begin
    CRC := CRC xor (Data[I] shl 8);
    for J := 0 to 7 do
      if (CRC and $8000) <> 0 then
        CRC := (CRC shl 1) xor POLYNOMIAL
      else
        CRC := CRC shl 1;
  end;
  Result := CRC and $FFFF;
end;

function DateTimeToMSDOSTime(const ADateTime: TDateTime): LongWord;
var
  Year, Month, Day: Word;
  Hour, Minute, Second, Millisecond: Word;
  DosDate, DosTime: Word;
begin
  DecodeDate(ADateTime, Year, Month, Day);
  DecodeTime(ADateTime, Hour, Minute, Second, Millisecond);
  if Year < 1980 then
    Year := 1980
  else if Year > 2107 then
    Year := 2107;
  DosDate := ((Year - 1980) shl 9) or (Month shl 5) or Day;
  DosTime := (Hour shl 11) or (Minute shl 5) or (Second div 2);
  Result := (LongWord(DosDate) shl 16) or DosTime;
end;

function CreateAutoBinHeader(const FileName: String; const FileSize: Int64;
  const CRC: Integer; const FileDateTime: TDateTime): String;
var
  DOSDateTime: LongWord;
begin
  Result := '';
  DOSDateTime := DateTimeToMSDOSTime(FileDateTime);
  if (CRC > 0) and (DOSDateTime > 0) then
    Result := '#BIN#' + IntToStr(FileSize) + '#|' + IntToStr(CRC) + '#$' +
      IntToStr(DOSDateTime) + '?#' + UpperCase(ExtractFileName(FileName));
end;

function ParseAutoBinHeader(const Head: String): TStrings;
var
  Regex: TRegExpr;
begin
  Result := TStringList.Create;
  Result.AddStrings(['', '', '', '', '']);
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^#(BIN|OK)#(?:(\d*)#\|(\d*)#\$(.*)\?#(.*))?$';
    Regex.ModifierI := True;
    if Regex.Exec(Head) then
    begin
      Result[0] := Regex.Match[1];
      Result[1] := Regex.Match[2];
      Result[2] := Regex.Match[3];
      Result[3] := Regex.Match[4];
      Result[4] := Regex.Match[5];
    end;
  finally
    Regex.Free;
  end;
end;

end.
