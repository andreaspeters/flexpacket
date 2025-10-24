unit ufileupload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, RegExpr, uresize, ExtCtrls, utypes, FileUtil;

type

  { TFFileUpload }

  TFFileUpload = class(TForm)
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    STFilename: TStaticText;
    STFileSize: TStaticText;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FOnUpload: TNotifyEvent;
    procedure GetGoSeven(const Data: AnsiString; const Channel: Byte);
    function GetDateTime(const FileName: string): TDateTime;
    function DateTimeToMSDOSTime(DateTime: TDateTime): LongWord;
    function CalculateCRC(const Data: TBytes): Integer;
    function WriteDataToFile(const FileName: string; const Data: TBytes):Integer;
    function WriteDataToFile(const FileName: string; const Data: AnsiString):Integer;
    function FileEnd(const ChannelBuffer: AnsiString): Boolean;
  public
    AutoBin: String;
    Buffer: TBytes;
    FileName: String;
    procedure FileDownload(const ChannelBuffer: TBytes; const Channel: Byte);
    procedure FileDownload(const ChannelBuffer: AnsiString; const Channel: Byte);
    procedure SetConfig(Config: PTFPConfig);
    function IsAutoBin(const Head:string):TStrings;
    function Parse7PlusHeader(const Download: TDownload): TDownload;
    function Default:TDownload;
    function LineContainsKeyword(const Line: String): Integer;
    property OnUpload: TNotifyEvent read FOnUpload write FOnUpload;
  end;

var
  FPConfig: PTFPConfig;
  FFileUpload: TFFileUpload;
  OrigWidth, OrigHeight: Integer;

implementation

{$R *.lfm}

{ TFFileUpload }

function TFFileUpload.Parse7PlusHeader(const Download: TDownload): TDownload;
var Header: String;
    posBase: Integer;
begin
  // Suche die Position von "go_7+." im Header-String
  posBase := Pos('go_7+.', Download.Header);
  Result := Download;
  Header := Download.Header;

  {
    Basierend auf dem Beispiel:
    "go_7+. 009 of 009 3YDX_1.JPG   0028024 0540 036 (7PLUS v2.1)"

    Felder relativ zum gefundenen Start (posBase):
      - posBase .. posBase+5:  "go_7+." (Kennung)
      - posBase+6:             Leerzeichen
      - posBase+7,  3 Zeichen: Part-Nummer ("009")
      - posBase+10, 4 Zeichen: fester String " of "
      - posBase+14, 3 Zeichen: Gesamtanzahl ("009")
      - posBase+17:            Leerzeichen
      - posBase+18, 10 Zeichen: Dateiname ("3YDX_1.JPG")
      - posBase+28, 3 Zeichen: Füllung (Leerzeichen)
      - posBase+31, 7 Zeichen: Dateigröße ("0028024")
      - posBase+38:            Leerzeichen
      - posBase+39, 4 Zeichen: Blockgröße ("0540")
      - posBase+43:            Leerzeichen
      - posBase+44, 3 Zeichen: CRC ("036")
      - posBase+47:            Leerzeichen
      - posBase+48 bis Ende:   Versionsangabe ("(7PLUS v2.1)")
  }

  Result.PartNumber := StrToInt(Copy(Header, posBase + 7, 3));
  Result.TotalParts := StrToInt(Copy(Header, posBase + 14, 3));
  Result.FileSize   := StrToInt(Copy(Header, posBase + 31, 7));
  Result.BlockSize  := StrToInt(Copy(Header, posBase + 39, 4));
  Result.FileCRC    := StrToInt(Copy(Header, posBase + 44, 3));
end;

procedure TFFileUpload.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

{
  FileDownload

  For TBytes Data

  If user accept filedownload, these procedure will call WriteDataToFile
  and check if the written data equal to the predicted file size.
}
procedure TFFileUpload.FileDownload(const ChannelBuffer: TBytes; const Channel: Byte);
var FName: String;
begin
  if Length(ChannelBuffer) > 0 then
  begin
    if WriteDataToFile(FPConfig^.Download[Channel].TempFileName, ChannelBuffer) >=
       FPConfig^.Download[Channel].FileSize then
    begin
      FPConfig^.Channel[Channel].Writeln('Download Done');

      FName := FPConfig^.DirectoryAutobin + DirectorySeparator + FPConfig^.Download[Channel].FileName;
      RenameFile(FPConfig^.Download[Channel].TempFileName, FName);
      FPConfig^.Download[Channel] := Default;
    end;
  end;
end;

{
  FileDownload

  For String Data

  If user accept filedownload, these procedure will call WriteDataToFile
  and check if the written data equal to the predicted file size.
}
procedure TFFileUpload.FileDownload(const ChannelBuffer: AnsiString; const Channel: Byte);
var FName, Go7Name: String;
begin
  if Length(ChannelBuffer) > 0 then
  begin
    // Header Size
    FPConfig^.Download[Channel].LinesHeader := FPConfig^.Download[Channel].LinesHeader + LineContainsKeyword(ChannelBuffer);

    // Check if it's a Go7 File.
    GetGoSeven(ChannelBuffer, Channel);

    // The TempFileName is set in UMain in the SetMail Procedure
    if (WriteDataToFile(FPConfig^.Download[Channel].TempFileName, ChannelBuffer) >=
       (FPConfig^.Download[Channel].Lines + FPConfig^.Download[Channel].LinesHeader + 2)) or (FileEnd(ChannelBuffer)) then
    begin
      // change the temporary file name to the real filename
      FName := FPConfig^.DirectoryMail + DirectorySeparator + FPConfig^.Download[Channel].FileName;
      RenameFile(FPConfig^.Download[Channel].TempFileName, FName);

      // if it's a Go7 file, copy it into the 7plus directory with the 7plus filename
      if FPConfig^.Download[Channel].Go7 then
      begin
        Go7Name := FPConfig^.Directory7Plus + DirectorySeparator + FPConfig^.Download[Channel].Go7FileName;
        if Length(FPConfig^.Download[Channel].Go7FileName) > 0 then
          if not CopyFile(FName, Go7Name) then
            ShowMessage('Could not create ' + Go7Name);
      end;
      FPConfig^.Download[Channel] := Default;
    end;
  end;
end;

{
  FileEnd

  FileEnd will check if the Mail sending is finish. Thats important for the case,
  that the BBS software does not have the mail size as part of the mail header.
}
function TFFileUpload.FileEnd(const ChannelBuffer: AnsiString): Boolean;
begin
  Result := False;

  // For LinBPQ BBS
  if Pos('[End of', ChannelBuffer) > 0 then
    Result := True;

  Exit;
end;

{
  GetDateTime

  Get the Date and Timestamp of FileName.
}
function TFFileUpload.GetDateTime(const FileName: string): TDateTime;
var
  Info: TSearchRec;
begin
  Result := -1;
  if FileExists(FileName) and (FindFirst(FileName, faAnyFile, Info) = 0) then
  begin
    Result := Info.TimeStamp;
    FindClose(Info);
  end;
end;


procedure TFFileUpload.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

{
  DateTimeToMSDOSTime

  Convert Pascal TDateTime into MSDOS DateTime. It's used at the AutoBin Header.
}
function TFFileUpload.DateTimeToMSDOSTime(DateTime: TDateTime): LongWord;
var
  Year, Month, Day: Word;
  Hour, Minute, Second, Millisecond: Word;
  DosDate, DosTime: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  DecodeTime(DateTime, Hour, Minute, Second, Millisecond);

  // since autobin is using msdos date/time it's only working until 2107. Sorry!!!
  if Year < 1980 then
    Year := 1980
  else if Year > 2107 then
    Year := 2107;

  DosDate := ((Year - 1980) shl 9) or (Month shl 5) or Day;

  DosTime := (Hour shl 11) or (Minute shl 5) or (Second div 2);

  Result := (LongWord(DosDate) shl 16) or DosTime;
end;

{
  CalculateCRC

  Calculate the CRC of the Data Array. It's used for the AutoBin Header.
}
function TFFileUpload.CalculateCRC(const Data: TBytes): Integer;
const
  POLYNOMIAL = $1021;
var
  i, j, count, CRC: Integer;
begin
  Result := 0;
  CRC := $FFFF; // Initialwert
  count := Length(Data);
  if count <= 0 then
    Exit;

  for i := 0 to Length(Data) - 1 do
  begin
    CRC := CRC xor (Data[i] shl 8);
    for j := 0 to 7 do
    begin
      if (CRC and $8000) <> 0 then
        CRC := (CRC shl 1) xor POLYNOMIAL
      else
        CRC := CRC shl 1;
    end;
  end;

  CRC := CRC and $FFFF;

  Result := CRC;
end;

procedure TFFileUpload.FormShow(Sender: TObject);
var FileSize: Int64;
    MSDOSDateTime: LongWord;
    FileStream: TFileStream;
    CRC: Word;
begin
  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;
      SetLength(Buffer, FileSize);
      FileStream.ReadBuffer(Buffer[0], FileSize);
      CRC := CalculateCRC(Buffer);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Could not read file: ' + E.Message);
      Exit;
    end;
  end;

  STFileName.Caption := ExtractFileName(FileName);
  STFileSize.Caption := IntToStr(FileSize) + ' bytes';

  MSDOSDateTime := DateTimeToMSDOSTime(GetDateTime(FileName));

  AutoBin := '';
  if (CRC > 0) and (MSDOSDateTime > 0) then
    AutoBin := '#BIN#'+IntToStr(FileSize)+'#|'+IntToStr(CRC)+'#$'+IntToStr(MSDOSDateTime)+'?#'+UpperCase(ExtractFileName(FileName));
end;

procedure TFFileUpload.OKButtonClick(Sender: TObject);
begin
  if Assigned(FOnUpload) then
    FOnUpload(Self);
  Close;
end;

{
  IsAutoBin

  Check if the Message "Head" is a AutoBin Header. If it's so,
  return a String array with all header parts.
}
function TFFileUpload.IsAutoBin(const Head:string):TStrings;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  Result := TStringList.Create;
  Result.AddStrings(['', '', '', '', '']);

  try
    Regex.Expression := '^#(BIN|OK)#(?:(\d*)#\|(\d*)#\$(.*)\?#(.*))?$';
    Regex.ModifierI := True;

    if Regex.Exec(Head) then
    begin
      Result[0] := Regex.Match[1]; // BIN or OK Message
      Result[1] := Regex.Match[2]; // File length
      Result[2] := Regex.Match[3]; // CRC
      Result[3] := Regex.Match[4]; // MSDOS DateTime
      Result[4] := Regex.Match[5]; // Filename Uppercase
    end;
  finally
    Regex.Free;
  end;
end;

{
  WriteDataToFile

  Fot TBytes

  This function is writin data into a file (FileName). If the file already
  exist, it append the data.
}
function TFFileUpload.WriteDataToFile(const FileName: String; const Data: TBytes):Integer;
var
  FileStream: TFileStream;
  NumBytes: Integer;
  Content: TBytes;
begin
  NumBytes := Length(Data);

  if FileExists(FileName) then
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    FileStream := TFileStream.Create(FileName, fmCreate);

  Content := Data;
  if Pos('stop_', TEncoding.UTF8.GetString(Content)) > 0 then
     SetLength(Content, Pos('stop_', TEncoding.UTF8.GetString(Content)));

  try
    FileStream.Seek(0, soEnd);
    FileStream.Write(SwapEndian(Content[0]), NumBytes);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('FileDownload Error: ', E.Message);
      {$ENDIF}
    end;
  end;
  Result := FileStream.Size + 1; // in my test, the size is always one byte lesser.
  FileStream.Free;
end;

{
  WriteDataToFile

  For String

  This function is writing data into a file (FileName). If the file already
  exist, it append the data.
}
function TFFileUpload.WriteDataToFile(const FileName: String; const Data: AnsiString): Integer;
var FileStream: TFileStream;
    DataBytes: TBytes;
    LineBuffer: TMemoryStream;
    Line: AnsiString;
    c: AnsiChar;
begin
  Result := 0;

  Line := StringReplace(Data, #13#10, #10, [rfReplaceAll]);  // Unix
  Line := StringReplace(Line, #13, #10, [rfReplaceAll]);     // old mac
  Line := StringReplace(Line, #10, #13#10, [rfReplaceAll]);

  DataBytes := BytesOf(Line);

  if FileExists(FileName) then
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    FileStream := TFileStream.Create(FileName, fmCreate);

  try
    FileStream.Seek(0, soEnd);
    FileStream.WriteBuffer(DataBytes[0], Length(DataBytes));
  finally
    FileStream.Free;
  end;

  // count lines
  LineBuffer := TMemoryStream.Create;
  try
    LineBuffer.LoadFromFile(FileName);
    LineBuffer.Position := 0;

    while LineBuffer.Position < LineBuffer.Size do
    begin
      Line := '';
      while (LineBuffer.Position < LineBuffer.Size) do
      begin
        LineBuffer.ReadBuffer(c, 1);
        if c = #10 then Break;
        Line := Line + c;
      end;
      Inc(Result);
    end;
  finally
    LineBuffer.Free;
  end;
end;

{
  GetGoSeven

  Check if "Data" is an go7+ message.
}
procedure TFFileUpload.GetGoSeven(const Data: AnsiString; const Channel: Byte);
var Regex: TRegExpr;
    FName, FExt: String;
begin
  if (Length(Data) = 0) then
    Exit;

  if Pos('go_7+.',Data) > 0 then
  begin
    FPConfig^.Download[Channel].Go7 := True;
    FPConfig^.Download[Channel].Header := Copy(Data, Pos('go_7+.',Data), Length(Data));

    if Length(FPConfig^.Download[Channel].Header) < 70 then
      Exit;
  end;

  if (FPConfig^.Download[Channel].Go7) and (Length(FPConfig^.Download[Channel].Header) < 70) then
    FPConfig^.Download[Channel].Header := FPConfig^.Download[Channel].Header + Copy(Data, 1, Length(Data));

  Regex := TRegExpr.Create;
  Regex.Expression := 'stop_7.*\((\S+)\/.* ';
  Regex.ModifierI := True;

  if Regex.Exec(Data) then
  begin
    FExt := LowerCase(ExtractFileExt(Regex.Match[1]));
    FName := ChangeFileExt(Regex.Match[1], FExt);
    FPConfig^.Download[Channel].Go7FileName := FName;
  end
end;

{
  LineContainsKeyword

  Count number ob lines in the header
}
function TFFileUpload.LineContainsKeyword(const Line: String): Integer;
var i: Integer;
    HeaderKeywords: TStringList;
    Regex: TRegExpr;
    CleanLine: String;
begin
  Result := 0;

  if Length(Line) <= 0 then
    Exit;

  HeaderKeywords := TStringList.Create;
  HeaderKeywords.Add('Read:');
  HeaderKeywords.Add('Subj:');
  HeaderKeywords.Add('Path:');
  HeaderKeywords.Add('Sent:');
  HeaderKeywords.Add('From:');
  HeaderKeywords.Add('To:');
  HeaderKeywords.Add('X-Info:');
  HeaderKeywords.Add('BID:');
  HeaderKeywords.Add('Bid:');
  HeaderKeywords.Add('MID:');
  HeaderKeywords.Add('Title:');
  HeaderKeywords.Add('Date/Time:');

  // check OpenBCM Line
  // DC6AP  > ALL      21.10.25 15:50z 2 Lines 39 Bytes #999 (999) @ WW
  Regex := TRegExpr.Create;
  Regex.Expression := '^(?:[A-Z]{1,2}[0-9][A-Z]{1,4}|[A-Z]{3}[0-9]{1,3})\s*>\s*[A-Z0-9\-].*$';
  Regex.ModifierI := True;

  if Regex.Exec(Line) then
    Inc(Result);

  CleanLine := StringReplace(Line, ' ', '', [rfReplaceAll]);

  for i := 0 to HeaderKeywords.Count - 1 do
    if Pos(HeaderKeywords[i], CleanLine) > 0 then
      inc(Result);
end;

function TFFileUpload.Default:TDownload;
begin
  Result.Enabled := False;
  Result.FileSize := 0;
  Result.BlockSize := 0;
  Result.FileCRC := 0;
  Result.FileName := '';
  Result.Go7FileName := '';
  Result.TempFileName := '';
  Result.PartNumber := 0;
  Result.TotalParts := 0;
  Result.AutoBin := False;
  Result.Mail := False;
  Result.Lines := 0;
  Result.LinesHeader := 0;
  Result.Header := '';
  Result.Go7 := False;
end;

end.

