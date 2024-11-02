unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, StdCtrls, Graphics;

type
  TFPConfig = record
    Channel: array[0..4] of TRichMemo;
    ComPort: string;
    ComSpeed: integer;
    TNCInit: String;
    Callsign: string;
    TerminalBGColor: TColor;
    TerminalFontSize: Integer;
    TerminalFontColor: TColor;
  end;

  TStatusLine = array[0..8] of string;

implementation

end.

