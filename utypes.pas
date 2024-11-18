unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, Buttons, StdCtrls, Graphics;

type
  TFPConfig = record
    Channel: array[0..10] of TRichMemo;
    Active: array[0..10] of Boolean;     // channel is active means, user typed sth into MTx and we have to send 'g' to these channel
    Connected: array[0..10] of Boolean;  // channel is connected
    Upload: array[0..10] of Boolean;     // channel is in uploading state. MTx should be disabled.
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
  function IsValidIPAddress(const IP: string): Boolean;

implementation

function IsValidIPAddress(const IP: string): Boolean;
var
  Parts: TStringArray;
  PartValue, I: Integer;
begin
  Result := False;

  Parts := IP.Split(['.']);

  if Length(Parts) <> 4 then
    Exit;

  for I := 0 to High(Parts) do
  begin
    if not TryStrToInt(Parts[I], PartValue) then
      Exit;

    if (PartValue < 0) or (PartValue > 255) then
      Exit;
  end;

  Result := True;
end;



end.

