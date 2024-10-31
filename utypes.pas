unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, StdCtrls;

type
  TFPConfig = record
    Channel: array[0..4] of TRichMemo;
    ComPort: string;
    ComSpeed: integer;
    TNCInit: String;
    Callsign: string;
  end;

  TStatusLine = array[0..8] of string;

implementation

end.

