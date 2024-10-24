unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo;

type
  TCom = record
    Port: string;
    Speed: integer;
  end;

  TFPConfig = record
    Channel: array[0..4] of TRichMemo;
    Com: TCom;
    Callsign: string;
  end;

implementation

end.

