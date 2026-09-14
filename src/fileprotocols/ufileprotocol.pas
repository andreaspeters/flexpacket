unit ufileprotocol;

{$mode ObjFPC}{$H+}
{$UNITPATH .}

interface

uses
  Classes, SysUtils, uautobin, uyapp, uyappc, udidadit;

type
  TFileProtocol = (fpUnknown, fpAutoBin, fpYapp, fpYappC, fpDidadit);

function DetectFileProtocol(const TextData: String; const ByteData: TBytes): TFileProtocol;
function FileProtocolName(const Protocol: TFileProtocol): String;

implementation

function DetectFileProtocol(const TextData: String; const ByteData: TBytes): TFileProtocol;
begin
  Result := fpUnknown;
  if (Pos('#BIN#', UpperCase(Trim(TextData))) = 1) or
     (Pos('#OK#', UpperCase(Trim(TextData))) = 1) or
     (Pos('#ABORT#', UpperCase(Trim(TextData))) = 1) then
    Exit(fpAutoBin);
  if YappCNegotiation(ByteData) then
    Exit(fpYappC);
  if YappLooksLike(ByteData) then
    Exit(fpYapp);
end;

function FileProtocolName(const Protocol: TFileProtocol): String;
begin
  case Protocol of
    fpAutoBin: Result := 'AutoBin';
    fpYapp: Result := 'YAPP';
    fpYappC: Result := 'YAPP-C';
    fpDidadit: Result := 'DIDADIT';
  else
    Result := 'Unknown';
  end;
end;

end.
