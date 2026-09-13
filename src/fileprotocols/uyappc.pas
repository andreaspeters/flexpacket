unit uyappc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, uyapp;

function YappCDataPacket(const Data: TBytes): TBytes;
function YappCChecksumValid(const Data: TBytes): Boolean;
function YappCNegotiation(const Data: TBytes): Boolean;
function YappCLooksLike(const Data: TBytes): Boolean;

implementation

function YappCDataPacket(const Data: TBytes): TBytes;
begin
  Result := YappDataPacket(Data, True);
end;

function YappCChecksumValid(const Data: TBytes): Boolean;
var Payload: TBytes;
begin
  Result := Length(Data) > 0;
  if not Result then Exit;
  Payload := Copy(Data, 0, Length(Data) - 1);
  Result := Data[High(Data)] = YappChecksum(Payload);
end;

function YappCNegotiation(const Data: TBytes): Boolean;
begin
  Result := (Length(Data) = 2) and (Data[0] = YAPP_ACK) and
    (Data[1] = YAPP_ACK);
end;

function YappCLooksLike(const Data: TBytes): Boolean;
begin
  Result := YappLooksLike(Data) or YappCNegotiation(Data);
end;

end.
