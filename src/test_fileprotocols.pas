program test_fileprotocols;

{$mode ObjFPC}{$H+}

uses
  SysUtils, Classes, uautobin, uyapp, uyappc, udidadit;

procedure Check(const Condition: Boolean; const Message: String);
begin
  if not Condition then
    raise Exception.Create('FAIL: ' + Message);
end;

var
  Data, Packet, DecodedData: TBytes;
  BlockType: Byte;
  FileName: String;
  FileSize: Int64;
  Fields: TStrings;
begin
  Data := TBytes.Create(0, $C0, $DB, $FF);
  Packet := YappDataPacket(Data, False);
  Check((Length(Packet) = Length(Data) + 2) and (Packet[0] = YAPP_STX),
    'YAPP packet framing');
  Check((Packet[1] = Length(Data)) and (Packet[2] = 0),
    'YAPP packet payload length');

  Packet := YappCDataPacket(Data);
  Check(YappCChecksumValid(Copy(Packet, 2, Length(Packet) - 2)),
    'YAPP-C checksum');
  Packet[High(Packet)] := Packet[High(Packet)] xor 1;
  Check(not YappCChecksumValid(Copy(Packet, 2, Length(Packet) - 2)),
    'YAPP-C rejects invalid checksum');

  Packet := DidaditBlock(4, Data);
  Check(DidaditDecodeBlock(Packet, BlockType, DecodedData),
    'DIDADIT frame decode');
  Check((BlockType = 4) and (Length(DecodedData) = Length(Data)) and
    (CompareByte(DecodedData[0], Data[0], Length(Data)) = 0),
    'DIDADIT payload roundtrip');

  Fields := ParseAutoBinHeader('#BIN#4#|123#$456?#TEST.BIN');
  try
    Check((Fields[0] = 'BIN') and (Fields[1] = '4') and
      (Fields[2] = '123') and (Fields[4] = 'TEST.BIN'),
      'AutoBin header parsing');
  finally
    Fields.Free;
  end;

  Check(YappHeaderFields(YappHeader('folder/test.bin', 42), FileName, FileSize),
    'YAPP header parsing');
  Check((FileName = 'test.bin') and (FileSize = 42),
    'YAPP filename and size');
  WriteLn('file protocol tests passed');
end.
