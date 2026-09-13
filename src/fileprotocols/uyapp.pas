unit uyapp;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TYappPacketKind = (ypUnknown, ypReceiveReady, ypReceiveFile, ypAckEOF,
    ypAckEOT, ypSendInit, ypHeader, ypData, ypEOF, ypEOT, ypNotReady,
    ypCancel, ypCancelAck);

const
  YAPP_ENQ = $05;
  YAPP_SOH = $01;
  YAPP_STX = $02;
  YAPP_ETX = $03;
  YAPP_EOT = $04;
  YAPP_ACK = $06;
  YAPP_NAK = $15;
  YAPP_CAN = $18;
  YAPP_DLE = $10;

function YappPacketKind(const Code: Byte; const Value: Byte = 0): TYappPacketKind;
function YappPacket(const Code: Byte; const Payload: TBytes): TBytes;
function YappDataPacket(const Data: TBytes; WithChecksum: Boolean): TBytes;
function YappChecksum(const Data: TBytes): Byte;
function YappHeader(const FileName: String; const FileSize: Int64): TBytes;
function YappLooksLike(const Data: TBytes): Boolean;
function YappHeaderFields(const Payload: TBytes; out FileName: String;
  out FileSize: Int64): Boolean;

implementation

function YappPacketKind(const Code: Byte; const Value: Byte): TYappPacketKind;
begin
  Result := ypUnknown;
  case Code of
    YAPP_ENQ:
      if Value = 1 then Result := ypSendInit;
    YAPP_SOH: Result := ypHeader;
    YAPP_STX: Result := ypData;
    YAPP_ETX: Result := ypEOF;
    YAPP_EOT: Result := ypEOT;
    YAPP_ACK:
      case Value of
        1: Result := ypReceiveReady;
        2: Result := ypReceiveFile;
        3: Result := ypAckEOF;
        4: Result := ypAckEOT;
        5: Result := ypCancelAck;
      end;
    YAPP_NAK: Result := ypNotReady;
    YAPP_CAN: Result := ypCancel;
  end;
end;

function YappPacket(const Code: Byte; const Payload: TBytes): TBytes;
begin
  Result := nil;
  if Length(Payload) > 256 then
    raise EArgumentOutOfRangeException.Create('YAPP payload exceeds 256 bytes');
  SetLength(Result, Length(Payload) + 2);
  Result[0] := Code;
  Result[1] := Length(Payload) and $FF;
  if Length(Payload) > 0 then
    Move(Payload[0], Result[2], Length(Payload));
end;

function YappChecksum(const Data: TBytes): Byte;
var I, Sum: Integer;
begin
  Sum := 0;
  for I := 0 to High(Data) do
    Sum := (Sum + Data[I]) and $FF;
  Result := Sum;
end;

function YappDataPacket(const Data: TBytes; WithChecksum: Boolean): TBytes;
var Payload: TBytes;
begin
  Payload := Copy(Data, 0, Length(Data));
  if WithChecksum then
  begin
    SetLength(Payload, Length(Payload) + 1);
    Payload[High(Payload)] := YappChecksum(Data);
  end;
  Result := YappPacket(YAPP_STX, Payload);
end;

function YappHeader(const FileName: String; const FileSize: Int64): TBytes;
var S: AnsiString;
begin
  S := AnsiString(ExtractFileName(FileName)) + #0 + AnsiString(IntToStr(FileSize)) + #0;
  Result := TBytes(S);
end;

function YappLooksLike(const Data: TBytes): Boolean;
begin
  Result := (Length(Data) >= 2) and
    (YappPacketKind(Data[0], Data[1]) <> ypUnknown);
end;

function YappHeaderFields(const Payload: TBytes; out FileName: String;
  out FileSize: Int64): Boolean;
var S: AnsiString; P: Integer; SizeText: String;
begin
  Result := False;
  FileName := '';
  FileSize := 0;
  if Length(Payload) = 0 then Exit;
  S := AnsiString(PAnsiChar(@Payload[0]));
  P := Pos(#0, String(S));
  if P <= 1 then Exit;
  FileName := Copy(String(S), 1, P - 1);
  S := Copy(S, P + 1, Length(S));
  P := Pos(#0, String(S));
  if P > 0 then SizeText := Copy(String(S), 1, P - 1)
  else SizeText := String(S);
  if not TryStrToInt64(SizeText, FileSize) then Exit;
  Result := True;
end;

end.
