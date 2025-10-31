unit utypes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Buttons, StdCtrls, Graphics, Process, ExtCtrls,
  uCmdBox, uCmdBoxCustom, StrUtils;
  
Const
  MAX_CHANNEL = 10;
  ESC = #27;

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
    Channel: array[0..MAX_CHANNEL] of TCmdBoxCustom;
    PTx: array[0..MAX_CHANNEL] of TPanel;         // memo to send data
    MTx: array[0..MAX_CHANNEL] of TMemo;          // memo to send data
    Connected: array[0..MAX_CHANNEL] of Boolean;  // channel is connected
    Download: array[0..MAX_CHANNEL] of TDownload; // channel is in download state.
    Upload: array[0..MAX_CHANNEL] of TUpload;     // channel is in upload state.
    IsCommand: array[0..MAX_CHANNEL] of Boolean;  // is in command mode
    IsConvers: array[0..MAX_CHANNEL] of Boolean;  // is in convers mode
    BayCom: array[0..MAX_CHANNEL] of String;      // channel baycom string
    DestCallsign: array[0..MAX_CHANNEL] of TStrings ;// destination callsign
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
    TerminalSignature: String;
    MainHeight: Integer;
    MainWidth: Integer;
    MainX: Integer;
    MainY: Integer;
    AGWServer: String;
    AGWServerPort: Integer;
    AGWServerUsername: String;
    AGWServerPassword: String;
    AGWVersionMinor: Integer;
    AGWVersionMajor: Integer;
    MailHeight: Integer;
    MailWidth: Integer;
    MailFontBold: Boolean;
    ConversBGColor: TColor;
    ConversFontColor: TColor;
    ConversX: Integer;
    ConversY: Integer;
  end;

  TBChannel = array[0..MAX_CHANNEL] of TBitBtn;
  TLChannel = array[0..MAX_CHANNEL] of TLabel;
  TStatusLine = array[0..MAX_CHANNEL] of String;
  PTFPConfig = ^TFPConfig;

procedure RestartApplication;
function IsValidIPAddress(const IP: string): Boolean;
function Min(a, b: Double): Double; overload;
function Min(A, B: Integer): Integer; overload;
function BytesToRawString(const Buffer: TBytes): String;
function LoadFileAsRawByteString(const FileName: String): RawByteString;
function RemoveNonPrintable(const S: AnsiString): AnsiString;
function RemoveANSICodes(const S: AnsiString): AnsiString;
function ColorToANSI(Color: TColor; IsBackground: Boolean = False): AnsiString;
function ColorText(Msg: AnsiString; TC: TColor; IsBackground: Boolean): AnsiString;

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

function RemoveANSICodes(const S: AnsiString): String;
var
  i, Len: Integer;
  Buf: String;
  InEscSeq: Boolean;
begin
  SetLength(Buf, Length(S));
  Len := 0;
  InEscSeq := False;

  for i := 1 to Length(S) do
  begin
    if not InEscSeq then
    begin
      if S[i] = #27 then      // ESC beginnt eine ANSI-Sequenz
        InEscSeq := True
      else
      begin
        Inc(Len);
        Buf[Len] := S[i];
      end;
    end
    else
    begin
      // ANSI-Sequenzen enden normalerweise mit einem Buchstaben A–Z oder a–z
      if S[i] in ['A'..'Z', 'a'..'z'] then
        InEscSeq := False;
      // alle Zeichen der Sequenz werden übersprungen
    end;
  end;

  SetLength(Buf, Len);
  Result := Buf;
end;

function ColorToANSI(Color: TColor; IsBackground: Boolean = False): AnsiString;
begin
  case Color of
    clBlack:    Result := IfThen(IsBackground, '40', '30');
    clMaroon:   Result := IfThen(IsBackground, '41', '31');
    clGreen:    Result := IfThen(IsBackground, '42', '32');
    clOlive:    Result := IfThen(IsBackground, '43', '33');
    clNavy:     Result := IfThen(IsBackground, '44', '34');
    clPurple:   Result := IfThen(IsBackground, '45', '35');
    clTeal:     Result := IfThen(IsBackground, '46', '36');
    clSilver:   Result := IfThen(IsBackground, '47', '37');
    clGray:     Result := IfThen(IsBackground, '100', '90');
    clRed:      Result := IfThen(IsBackground, '101', '91');
    clLime:     Result := IfThen(IsBackground, '102', '92');
    clYellow:   Result := IfThen(IsBackground, '103', '93');
    clBlue:     Result := IfThen(IsBackground, '104', '94');
    clFuchsia:  Result := IfThen(IsBackground, '105', '95');
    clAqua:     Result := IfThen(IsBackground, '106', '96');
    clWhite:    Result := IfThen(IsBackground, '107', '97');
  else
    Result := IfThen(IsBackground, '49', '39'); // Default terminal color
  end;

  Result := ESC+'['+Result+'m';
end;


function ColorText(Msg: AnsiString; TC: TColor; IsBackground: Boolean): AnsiString;
begin
  Result := ColorToANSI(TC, IsBackground) + Msg + ESC+'[0m';
end;

end.

