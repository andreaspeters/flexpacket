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
    function GetDateTime(const FileName: string): TDateTime;
    function DateTimeToMSDOSTime(DateTime: TDateTime): LongWord;
    function CalculateCRC(const Data: TBytes): Integer;
    function WriteDataToFile(const FileName: string; const Data: TBytes):Integer;
    function WriteDataToFile(const FileName: string; const Data: String):Integer;
  public
    AutoBin: String;
    Buffer: TBytes;
    FileName: String;
    procedure FileDownload(const ChannelBuffer: TBytes; const Channel: Byte);
    procedure FileDownload(const ChannelBuffer: String; const Channel: Byte);
    procedure SetConfig(Config: PTFPConfig);
    function IsAutoBin(const Head:string):TStrings;
    function Parse7PlusHeader(const Download: TDownload): TDownload;
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
  Result.FileName   := Trim(Copy(Header, posBase + 18, 10));
  Result.FileSize   := StrToInt(Copy(Header, posBase + 31, 7));
  Result.BlockSize  := StrToInt(Copy(Header, posBase + 39, 4));
  Result.FileCRC    := StrToInt(Copy(Header, posBase + 44, 3));

  FileName := ExtractFileName(Result.FileName);

  if Result.TotalParts = 1 then
    FileName := FileName + '.7pl'
  else
    FileName := Format('%s.p%.2d', [FileName, Result.PartNumber]);

  Result.FileName := StringReplace(FileName, ' ', '', [rfReplaceAll]);
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
    if FPConfig^.Download[Channel].TempFileName = '' then
      FPConfig^.Download[Channel].TempFileName := GetTempFileName(FPConfig^.DirectoryAutoBin, 'part');

    if WriteDataToFile(FPConfig^.Download[Channel].TempFileName, ChannelBuffer) = FPConfig^.Download[Channel].FileSize then
    begin
      FPConfig^.Channel[Channel].Writeln('Download Done');
      FName := FPConfig^.DirectoryAutoBin + '/' + FPConfig^.Download[Channel].FileName;
      FPConfig^.Download[Channel].Enabled := False;
      RenameFile(FPConfig^.Download[Channel].TempFileName, FName);
      FPConfig^.Download[Channel].TempFileName := '';
      FPConfig^.Download[Channel].FileName := '';
    end;
  end;
end;

{
  FileDownload

  For String Data

  If user accept filedownload, these procedure will call WriteDataToFile
  and check if the written data equal to the predicted file size.
}
procedure TFFileUpload.FileDownload(const ChannelBuffer: String; const Channel: Byte);
var FName: String;
begin
  if Length(ChannelBuffer) > 0 then
  begin
    if FPConfig^.Download[Channel].TempFileName = '' then
      FPConfig^.Download[Channel].TempFileName := GetTempFileName(FPConfig^.DirectoryAutoBin, 'part');

    if WriteDataToFile(FPConfig^.Download[Channel].TempFileName, ChannelBuffer) = FPConfig^.Download[Channel].FileSize then
    begin
      FPConfig^.Channel[Channel].Writeln('Download Done');
      FName := FPConfig^.DirectoryAutoBin + '/' + FPConfig^.Download[Channel].FileName;
      FPConfig^.Download[Channel].Enabled := False;
      RenameFile(FPConfig^.Download[Channel].TempFileName, FName);
      FPConfig^.Download[Channel].TempFileName := '';
      FPConfig^.Download[Channel].FileName := '';
    end;
  end;
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
function TFFileUpload.WriteDataToFile(const FileName: string; const Data: TBytes):Integer;
var
  FileStream: TFileStream;
  NumBytes: Integer;
  Content: TBytes;
begin
  NumBytes := Length(Data);

  if FileExists(FileName) then
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite)
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

  This function is writin data into a file (FileName). If the file already
  exist, it append the data.
}
function TFFileUpload.WriteDataToFile(const FileName: string; const Data: String):Integer;
var FileStream: TextFile;
begin
  AssignFile(FileStream, FileName);
  if FileExists(FileName) then
    Append(FileStream)
  else
    Rewrite(FileStream);

  try
    Write(FileStream, Data);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('FileDownload Error: ', E.Message);
      {$ENDIF}
    end;
  end;
  CloseFile(FileStream);
  Result := FileSize(FileName) + 1; // in my test, the size is always one byte lesser.
end;

end.

