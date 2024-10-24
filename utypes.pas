unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, StdCtrls;

type
  TCom = record
    Port: string;
    Speed: integer;
  end;

  TFPConfig = record
    Channel: array[0..4] of TRichMemo;
    Com: TCom;
    TNCInit: String;
    Callsign: string;
  end;

implementation

end.

