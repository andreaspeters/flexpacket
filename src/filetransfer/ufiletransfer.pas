unit ufiletransfer;

{$mode ObjFPC}{$H+}
{$UNITPATH .}

interface

uses
  Classes, SysUtils, uyapp, uyappc, udidadit, ufileprotocol;


function EncodeFilePayload(const Protocol: TFileTransferProtocol;
  const Data: TBytes; Offset, Count: Integer): TBytes;

implementation

function EncodeFilePayload(const Protocol: TFileTransferProtocol;
  const Data: TBytes; Offset, Count: Integer): TBytes;
var Body: TBytes; I: Integer;
begin
  case Protocol of
    ftpYAPP: Result := YAPPEncodeData(Data, Offset, Count);
    ftpYAPPC: Result := YAPPEncodeDataChecked(Data, Offset, Count);
    ftpDIDADIT:
      begin
        SetLength(Body, 6 + Count);
        Body[0] := (Offset shr 24) and $FF; Body[1] := (Offset shr 16) and $FF;
        Body[2] := (Offset shr 8) and $FF; Body[3] := Offset and $FF;
        Body[4] := (Count shr 8) and $FF; Body[5] := Count and $FF;
        for I := 0 to Count - 1 do Body[6 + I] := Data[Offset + I];
        Result := DIDADITEncode(ddData, Body);
      end;
  else
    begin SetLength(Result, Count); if Count > 0 then Move(Data[Offset], Result[0], Count) end;
  end;
end;

end.