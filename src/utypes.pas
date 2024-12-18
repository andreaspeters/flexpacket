unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, Buttons, StdCtrls, Graphics, Process, ExtCtrls;

type
  TUpload = record
    Enabled: Boolean;
    FileName: String;
  end;
  TDownload = record
    Enabled: Boolean;
    FileSize: Integer;
    FileCRC: Integer;
    FileName: String;
  end;

  TFPConfig = record
    Channel: array[0..10] of TRichMemo;
    PTx: array[0..10] of TPanel;         // memo to send data
    MTx: array[0..10] of TMemo;          // memo to send data
    Connected: array[0..10] of Boolean;  // channel is connected
    Download: array[0..10] of TDownload; // channel is in download state.
    Upload: array[0..10] of TUpload;     // channel is in upload state.
    IsCommand: array[0..10] of Boolean;
    MaxChannels: Byte;
    ComPort: string;
    ComSpeed: integer;
    ComBits: Byte;
    ComParity: String;
    ComStopBit: Byte;
    Executable7Plus: String;
    ExecutableAPRSMap: String;
    Directory7Plus: String;
    DirectoryAutoBin: String;
    TNCInit: String;
    EnableTNC: Boolean;
    EnableAGW: Boolean;
    EnableKISS: Boolean;
    Callsign: string;
    TerminalBGColor: TColor;
    TerminalFontSize: Integer;
    TerminalFontColor: TColor;
    TerminalHeight: Integer;
    MainHeight: Integer;
    MainWidth: Integer;
    AGWServer: String;
    AGWServerPort: Integer;
    AGWServerUsername: String;
    AGWServerPassword: String;
    AGWVersionMinor: Integer;
    AGWVersionMajor: Integer;
  end;

  TBChannel = array[0..10] of TBitBtn;
  TLChannel = array[0..10] of TLabel;
  TStatusLine = array[0..10] of String;
  PTFPConfig = ^TFPConfig;

procedure RestartApplication;
function IsValidIPAddress(const IP: string): Boolean;
function Min(a, b: Double): Double; overload;
function Min(A, B: Integer): Integer; overload;

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

procedure RestartApplication;
var
  Process: TProcess;
begin
  Process := TProcess.Create(nil);
  try
    Process.Executable := ParamStr(0);
    Process.Execute;
    Halt(0);
  finally
    Process.Free;
  end;
end;


function Min(A, B: Integer): Integer; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Double): Double; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

end.

