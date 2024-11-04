unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, Buttons, StdCtrls, Graphics;

type
  TFPConfig = record
    Channel: array[0..10] of TRichMemo;
    MaxChannels: Byte;
    ComPort: string;
    ComSpeed: integer;
    TNCInit: String;
    Callsign: string;
    TerminalBGColor: TColor;
    TerminalFontSize: Integer;
    TerminalFontColor: TColor;
  end;

  TBChannel = array[0..10] of TBitBtn;
  TStatusLine = array[0..8] of string;

implementation

end.

