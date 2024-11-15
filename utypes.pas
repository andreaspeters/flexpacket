unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, Buttons, StdCtrls, Graphics;

type
  TFPConfig = record
    Channel: array[0..10] of TRichMemo;
    Active: array[0..10] of Boolean;
    Connected: array[0..10] of Boolean;
    MaxChannels: Byte;
    ComPort: string;
    ComSpeed: integer;
    TNCInit: String;
    EnableTNC: Boolean;
    EnableAGW: Boolean;
    Callsign: string;
    TerminalBGColor: TColor;
    TerminalFontSize: Integer;
    TerminalFontColor: TColor;
    AGWServerIP: String;
    AGWServerPort: Integer;
    AGWServerUsername: String;
    AGWServerPassword: String;
  end;

  TBChannel = array[0..10] of TBitBtn;
  TLChannel = array[0..10] of TLabel;
  TStatusLine = array[0..8] of string;

implementation

end.

