unit uyappc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

function YAPPCChecksum(const Data: TBytes): Byte;
function YAPPCVerify(const Data: TBytes; Checksum: Byte): Boolean;

implementation

function YAPPCChecksum(const Data: TBytes): Byte;
var I, S: Integer;
begin S := 0; for I := 0 to High(Data) do S := (S + Data[I]) and $FF; Result := S end;

function YAPPCVerify(const Data: TBytes; Checksum: Byte): Boolean;
begin Result := YAPPCChecksum(Data) = Checksum end;

end.