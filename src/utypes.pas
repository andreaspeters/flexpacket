unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Buttons, StdCtrls, Graphics, Process, ExtCtrls,
  uCmdBox, uCmdBoxCustom;

type
  TUpload = record
    Enabled: Boolean;
    FileName: String;
  end;

  TMessageHeader = record
    FromCall: String;
    ToCall: String;
    FromCall2: String;
    ToCall2: String;
    DateStr: String;
    TimeStr: String;
    Lines: Integer;
    Bytes: Integer;
    BID: String;
    MID: String;
    ReadBy: String;
    Subject: String;
    MType: Char;
  end;

  TDownload = record
    Enabled: Boolean;
    FileSize: Integer;
    BlockSize: Integer;
    FileCRC: Integer;
    FileName: String;         // if it's a mail then a hash value, if it's autobin then the real filename)
    TempFileName: String;     // part0000.tmp
    Go7FileName: String;      // the go7 filename (test.p01..test.p0n)
    PartNumber: Integer;
    TotalParts: Integer;
    FileDateTime: TDateTime;
    AutoBin: Boolean;
    Mail: Boolean;
    Go7: Boolean;
    Lines: Integer;           // line of mail body (without header)
    LinesHeader: Integer;     // how many lines has the header
    Header: String;
    OpenBCM: Boolean;
    LinBPQ: Boolean;
  end;

  TFPConfig = record
    Channel: array[0..10] of TCmdBoxCustom;
    PTx: array[0..10] of TPanel;         // memo to send data
    MTx: array[0..10] of TMemo;          // memo to send data
    Connected: array[0..10] of Boolean;  // channel is connected
    Download: array[0..10] of TDownload; // channel is in download state.
    Upload: array[0..10] of TUpload;     // channel is in upload state.
    IsCommand: array[0..10] of Boolean;
    BayCom: array[0..10] of String;      // channel baycom string
    DestCallsign: array[0..10] of TStrings ;// destination callsign
    MaxChannels: Byte;
    ComPort: string;
    ComSpeed: integer;
    ComBits: Byte;
    ComParity: String;
    ComStopBit: Byte;
    Executable7Plus: String;
    ExecutableAPRSMap: String;
    ExecutableForms: String;
    Directory7Plus: String;
    DirectoryAutoBin: String;
    DirectoryMail: String;
    TNCInit: String;
    EnableTNC: Boolean;
    EnableAGW: Boolean;
    EnableKISS: Boolean;
    KISSPipe: String;
    Callsign: string;
    TerminalBGColor: TColor;
    TerminalFontSize: Integer;
    TerminalFontColor: TColor;
    TerminalFontName: String;
    TerminalHeight: Integer;
    TerminalToolbarBig: Boolean;
    MainHeight: Integer;
    MainWidth: Integer;
    AGWServer: String;
    AGWServerPort: Integer;
    AGWServerUsername: String;
    AGWServerPassword: String;
    AGWVersionMinor: Integer;
    AGWVersionMajor: Integer;
    MailHeight: Integer;
    MailWidth: Integer;
  end;

  TBChannel = array[0..10] of TBitBtn;
  TLChannel = array[0..10] of TLabel;
  TStatusLine = array[0..10] of String;
  PTFPConfig = ^TFPConfig;

procedure RestartApplication;
function IsValidIPAddress(const IP: string): Boolean;
function Min(a, b: Double): Double; overload;
function Min(A, B: Integer): Integer; overload;
function BytesToRawString(const Buffer: TBytes): String;
function LoadFileAsRawByteString(const FileName: String): RawByteString;
function RemoveNonPrintable(const S: AnsiString): AnsiString;

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

function BytesToRawString(const Buffer: TBytes): String;
var
  i: Integer;
begin
  SetLength(Result, Length(Buffer));
  for i := 0 to High(Buffer) do
    Result[i + 1] := AnsiChar(Buffer[i]);
end;

function LoadFileAsRawByteString(const FileName: String): RawByteString;
var Stream: TFileStream;
    Buffer: RawByteString;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  try
    SetLength(Buffer, Stream.Size);
    if Stream.Size > 0 then
      Stream.ReadBuffer(Buffer[1], Stream.Size);
  finally
    Stream.Free;
  end;
  Result := Buffer;
end;

function RemoveNonPrintable(const S: AnsiString): String;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(S) do
    if S[i] in [#32..#126] then  // Behalte nur druckbare ASCII-
      Result := Result + S[i];
end;


end.

