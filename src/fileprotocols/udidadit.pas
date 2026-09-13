unit udidadit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDidaditBlockType = (dbUnknown, dbInfo, dbStart, dbError, dbData, dbFin,
    dbRequest, dbFinAck, dbEchoRequest, dbEchoReply, dbAbort, dbChat);

const
  DIDADIT_FEND = $C0;
  DIDADIT_FESC = $DB;
  DIDADIT_TFEND = $DC;
  DIDADIT_TFESC = $DD;

function DidaditBlockType(const Value: Byte): TDidaditBlockType;
function DidaditCRC(const Data: TBytes): Word;
function DidaditStuff(const Data: TBytes): TBytes;
function DidaditUnstuff(const Data: TBytes): TBytes;
function DidaditBlock(const BlockType: Byte; const Data: TBytes): TBytes;
function DidaditLooksLike(const Data: TBytes): Boolean;
function DidaditDecodeBlock(const Framed: TBytes; out BlockType: Byte;
  out Data: TBytes): Boolean;

implementation

function DidaditBlockType(const Value: Byte): TDidaditBlockType;
begin
  case Value of
    1: Result := dbInfo;
    2: Result := dbStart;
    3: Result := dbError;
    4: Result := dbData;
    5: Result := dbFin;
    6: Result := dbRequest;
    7: Result := dbFinAck;
    9: Result := dbEchoRequest;
    10: Result := dbEchoReply;
    11: Result := dbAbort;
    12: Result := dbChat;
  else
    Result := dbUnknown;
  end;
end;

function DidaditCRC(const Data: TBytes): Word;
const Polynomial = $1021;
var I, J: Integer; CRC: Cardinal;
begin
  CRC := $FFFF;
  for I := 0 to High(Data) do
  begin
    CRC := CRC xor (Cardinal(Data[I]) shl 8);
    for J := 0 to 7 do
      if (CRC and $8000) <> 0 then CRC := (CRC shl 1) xor Polynomial
      else CRC := CRC shl 1;
  end;
  Result := CRC and $FFFF;
end;

function DidaditStuff(const Data: TBytes): TBytes;
var I, N: Integer;
begin
  Result := nil;
  SetLength(Result, 0);
  for I := 0 to High(Data) do
  begin
    N := Length(Result);
    if (Data[I] = DIDADIT_FEND) or (Data[I] = DIDADIT_FESC) then
      SetLength(Result, N + 2)
    else
      SetLength(Result, N + 1);
    if Data[I] = DIDADIT_FEND then begin Result[N] := DIDADIT_FESC; Result[N+1] := DIDADIT_TFEND end
    else if Data[I] = DIDADIT_FESC then begin Result[N] := DIDADIT_FESC; Result[N+1] := DIDADIT_TFESC end
    else Result[N] := Data[I];
  end;
end;

function DidaditUnstuff(const Data: TBytes): TBytes;
var I, N: Integer;
begin
  Result := nil;
  SetLength(Result, 0); I := 0;
  while I <= High(Data) do
  begin
    N := Length(Result); SetLength(Result, N + 1);
    if (Data[I] = DIDADIT_FESC) and (I < High(Data)) then
    begin
      Inc(I);
      if Data[I] = DIDADIT_TFEND then Result[N] := DIDADIT_FEND
      else if Data[I] = DIDADIT_TFESC then Result[N] := DIDADIT_FESC
      else Result[N] := Data[I];
    end else Result[N] := Data[I];
    Inc(I);
  end;
end;

function DidaditBlock(const BlockType: Byte; const Data: TBytes): TBytes;
var Raw, Stuffed: TBytes; C: Word; N: Integer;
begin
  Result := nil;
  if DidaditBlockType(BlockType) = dbUnknown then
    raise EArgumentException.Create('Unknown DIDADIT block type');
  SetLength(Raw, Length(Data) + 4);
  Raw[0] := BlockType; Raw[1] := 0;
  if Length(Data) > 0 then Move(Data[0], Raw[2], Length(Data));
  C := DidaditCRC(Copy(Raw, 0, Length(Raw) - 2));
  N := Length(Raw); Raw[N-2] := C and $FF; Raw[N-1] := C shr 8;
  Stuffed := DidaditStuff(Raw);
  SetLength(Result, Length(Stuffed) + 2); Result[0] := DIDADIT_FEND;
  if Length(Stuffed) > 0 then Move(Stuffed[0], Result[1], Length(Stuffed));
  Result[High(Result)] := DIDADIT_FEND;
end;

function DidaditLooksLike(const Data: TBytes): Boolean;
begin
  Result := (Length(Data) >= 3) and (Data[0] = DIDADIT_FEND) and
    (DidaditBlockType(DidaditUnstuff(Copy(Data, 1, Length(Data)-2))[0]) <> dbUnknown);
end;

function DidaditDecodeBlock(const Framed: TBytes; out BlockType: Byte;
  out Data: TBytes): Boolean;
var Raw: TBytes; Stored, Calculated: Word;
begin
  Result := False; BlockType := 0; Data := nil;
  if (Length(Framed) < 6) or (Framed[0] <> DIDADIT_FEND) or
     (Framed[High(Framed)] <> DIDADIT_FEND) then Exit;
  Raw := DidaditUnstuff(Copy(Framed, 1, Length(Framed) - 2));
  if (Length(Raw) < 4) or (DidaditBlockType(Raw[0]) = dbUnknown) or
     (Raw[1] <> 0) then Exit;
  Stored := Raw[Length(Raw)-2] or (Word(Raw[Length(Raw)-1]) shl 8);
  Calculated := DidaditCRC(Copy(Raw, 0, Length(Raw)-2));
  if Stored <> Calculated then Exit;
  BlockType := Raw[0];
  SetLength(Data, Length(Raw)-4);
  if Length(Data) > 0 then Move(Raw[2], Data[0], Length(Data));
  Result := True;
end;

end.
