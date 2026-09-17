unit udidadit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TDIDADITType = (ddInfo = 1, ddStart = 2, ddErr = 3, ddData = 4,
    ddFin = 5, ddReq = 6, ddFinAck = 7, ddEchoRequest = 9,
    ddEchoReply = 10, ddAbort = 11, ddChat = 12);

const
  DIDADIT_FEND = $C0;
  DIDADIT_FESC = $DB;
  DIDADIT_TFEND = $DC;
  DIDADIT_TFESC = $DD;

function DIDADITCRC(const Data: TBytes): Word;
function DIDADITStuff(const Data: TBytes): TBytes;
function DIDADITUnstuff(const Data: TBytes): TBytes;
function DIDADITEncode(const BlockType: TDIDADITType; const Data: TBytes;
  IncludeMD5: Boolean = False; const MD5: TBytes = nil): TBytes;
function DIDADITDecode(const Frame: TBytes; out BlockType: TDIDADITType;
  out Data: TBytes): Boolean;

implementation

function DIDADITCRC(const Data: TBytes): Word;
var
  I, J: Integer;
  C: Integer;
begin
  C := 0;
  for I := 0 to High(Data) do
  begin
    C := C xor (Integer(Data[I]) shl 8);
    for J := 0 to 7 do
    begin
      if (C and $8000) <> 0 then
        C := ((C shl 1) xor $1021) and $FFFF
      else
        C := (C shl 1) and $FFFF;
    end;
  end;
  Result := C;
end;

function DIDADITStuff(const Data: TBytes): TBytes;
var I, N: Integer;
begin
  SetLength(Result, 0); N := 0; SetLength(Result, Length(Data) * 2 + 2); Result[N] := DIDADIT_FEND; Inc(N);
  for I := 0 to High(Data) do begin case Data[I] of DIDADIT_FEND: begin Result[N] := DIDADIT_FESC; Result[N+1] := DIDADIT_TFEND; Inc(N, 2) end; DIDADIT_FESC: begin Result[N] := DIDADIT_FESC; Result[N+1] := DIDADIT_TFESC; Inc(N, 2) end; else Result[N] := Data[I]; Inc(N) end end;
  Result[N] := DIDADIT_FEND; SetLength(Result, N + 1);
end;

function DIDADITUnstuff(const Data: TBytes): TBytes;
var I, N: Integer; Esc: Boolean;
begin
  SetLength(Result, Length(Data)); N := 0; Esc := False;
  for I := 0 to High(Data) do begin if (Data[I] = DIDADIT_FEND) then Continue; if Esc then begin if Data[I] = DIDADIT_TFEND then Result[N] := DIDADIT_FEND else if Data[I] = DIDADIT_TFESC then Result[N] := DIDADIT_FESC else Continue; Esc := False; Inc(N) end else if Data[I] = DIDADIT_FESC then Esc := True else begin Result[N] := Data[I]; Inc(N) end end;
  SetLength(Result, N);
end;

function DIDADITEncode(const BlockType: TDIDADITType; const Data: TBytes; IncludeMD5: Boolean; const MD5: TBytes): TBytes;
var Raw: TBytes; L, I: Integer; C: Word;
begin
  L := 1 + Length(Data);
  if IncludeMD5 then
    Inc(L, 16);
  SetLength(Raw, L);
  Raw[0] := Ord(BlockType);
  I := 1;
  if IncludeMD5 then
  begin
    if Length(MD5) <> 16 then
      raise EArgumentException.Create('DIDADIT MD5 must be 16 bytes');
    Move(MD5[0], Raw[I], 16);
    Inc(I, 16);
  end;
  if Length(Data) > 0 then
    Move(Data[0], Raw[I], Length(Data));
  C := DIDADITCRC(Raw);
  SetLength(Raw, L + 2);
  Raw[L] := C shr 8;
  Raw[L+1] := C and $FF;
  Result := DIDADITStuff(Raw);
end;

function DIDADITDecode(const Frame: TBytes; out BlockType: TDIDADITType; out Data: TBytes): Boolean;
var Raw: TBytes; C, Given: Word; L: Integer;
begin
  Result := False; SetLength(Data, 0); Raw := DIDADITUnstuff(Frame); if Length(Raw) < 3 then Exit; Given := (Word(Raw[Length(Raw)-2]) shl 8) or Raw[Length(Raw)-1]; SetLength(Raw, Length(Raw)-2); C := DIDADITCRC(Raw); if C <> Given then Exit; if (Raw[0] < 1) or (Raw[0] > 12) then Exit; try BlockType := TDIDADITType(Raw[0]) except Exit end; L := Length(Raw)-1; if L > 0 then begin SetLength(Data, L); Move(Raw[1], Data[0], L) end; Result := True;
end;

end.