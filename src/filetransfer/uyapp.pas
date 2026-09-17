unit uyapp;

{$mode ObjFPC}{$H+}
{$UNITPATH .}

interface

uses
  Classes, SysUtils, uyappc;

type
  TYAPPType = (yappSI, yappRR, yappRF, yappAF, yappAT, yappRT,
    yappCA, yappHD, yappDT, yappEF, yappET, yappNR, yappRE, yappCN);
  TYAPPDecoded = record
    PacketType: TYAPPType;
    Payload: TBytes;
  end;

const
  YAPP_ENQ = $05;
  YAPP_SOH = $01;
  YAPP_STX = $02;
  YAPP_ETX = $03;
  YAPP_EOT = $04;
  YAPP_ACK = $06;
  YAPP_NAK = $15;
  YAPP_CAN = $18;

function YAPPEncodeSimple(const PacketType: TYAPPType): TBytes;
function YAPPEncodeHeader(const FileName: String; const FileSize: Int64): TBytes;
function YAPPEncodeData(const Data: TBytes; Offset, Count: Integer): TBytes;
function YAPPEncodeDataChecked(const Data: TBytes; Offset, Count: Integer): TBytes;
function YAPPEncodeAck(SubType: Byte): TBytes;
function YAPPEncodeAbort(const Reason: TBytes): TBytes;
function YAPPDecode(const Frame: TBytes): TYAPPDecoded;

implementation

function YAPPEncodeSimple(const PacketType: TYAPPType): TBytes;
var C, S: Byte;
begin
  case PacketType of
    yappSI: begin C := YAPP_ENQ; S := 1 end;
    yappEF: begin C := YAPP_ETX; S := 1 end;
    yappET: begin C := YAPP_EOT; S := 1 end;
  else SetLength(Result, 0); Exit end;
  SetLength(Result, 2); Result[0] := C; Result[1] := S;
end;

function YAPPEncodeHeader(const FileName: String; const FileSize: Int64): TBytes;
var S: RawByteString; L: Integer;
begin
  S := ExtractFileName(FileName) + #0 + IntToStr(FileSize) + #0;
  L := Length(S); if L > 255 then L := 255;
  SetLength(Result, L + 2); Result[0] := YAPP_SOH; Result[1] := L;
  if L > 0 then Move(S[1], Result[2], L);
end;

function YAPPEncodeData(const Data: TBytes; Offset, Count: Integer): TBytes;
begin
  if Offset < 0 then Offset := 0;
  if Count < 0 then Count := 0;
  if Count > 256 then Count := 256;
  if Offset > Length(Data) then Count := 0
  else if Offset + Count > Length(Data) then Count := Length(Data) - Offset;
  SetLength(Result, Count + 2); Result[0] := YAPP_STX;
  if Count = 256 then Result[1] := 0 else Result[1] := Count;
  if Count > 0 then Move(Data[Offset], Result[2], Count);
end;

function YAPPEncodeDataChecked(const Data: TBytes; Offset, Count: Integer): TBytes;
var Payload: TBytes;
begin
  Payload := YAPPEncodeData(Data, Offset, Count);
  SetLength(Result, Length(Payload) + 1);
  if Length(Payload) > 0 then Move(Payload[0], Result[0], Length(Payload));
  if Length(Payload) > 2 then begin
    SetLength(Payload, Length(Payload) - 2);
    Result[High(Result)] := YAPPCChecksum(Payload);
  end else Result[High(Result)] := 0;
end;

function YAPPEncodeAck(SubType: Byte): TBytes;
begin SetLength(Result, 2); Result[0] := YAPP_ACK; Result[1] := SubType end;

function YAPPEncodeAbort(const Reason: TBytes): TBytes;
var L: Integer;
begin
  L := Length(Reason); if L > 255 then L := 255;
  SetLength(Result, L + 2); Result[0] := YAPP_CAN; Result[1] := L;
  if L > 0 then Move(Reason[0], Result[2], L);
end;

function YAPPDecode(const Frame: TBytes): TYAPPDecoded;
var L: Integer;
begin
  Result.PacketType := yappCN; SetLength(Result.Payload, 0);
  if Length(Frame) = 0 then Exit;
  case Frame[0] of
    YAPP_ENQ: if (Length(Frame) > 1) and (Frame[1] = 1) then Result.PacketType := yappSI;
    YAPP_SOH: Result.PacketType := yappHD;
    YAPP_STX: Result.PacketType := yappDT;
    YAPP_ETX: if (Length(Frame) > 1) and (Frame[1] = 1) then Result.PacketType := yappEF;
    YAPP_EOT: if (Length(Frame) > 1) and (Frame[1] = 1) then Result.PacketType := yappET;
    YAPP_ACK: if Length(Frame) > 1 then case Frame[1] of
      1: Result.PacketType := yappRR; 2: Result.PacketType := yappRF;
      3: Result.PacketType := yappAF; 4: Result.PacketType := yappAT;
      5: Result.PacketType := yappCA; else Result.PacketType := yappRT end;
    YAPP_NAK: Result.PacketType := yappNR;
    YAPP_CAN: Result.PacketType := yappCN;
  end;
  if Result.PacketType in [yappHD, yappDT, yappCN, yappNR, yappRE] then begin
    if Length(Frame) > 1 then begin
      L := Frame[1]; if (Result.PacketType = yappDT) and (L = 0) then L := 256;
      if L > Length(Frame) - 2 then L := Length(Frame) - 2;
      if L > 0 then begin SetLength(Result.Payload, L); Move(Frame[2], Result.Payload[0], L) end;
    end;
  end;
end;

end.
