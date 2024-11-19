unit ufileupload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls,
  ButtonPanel, RegExpr, ExtCtrls;

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
    function CalculateCRC(const Data: array of Byte): Word;
  public
    AutoBin: String;
    Buffer: TBytes;
    procedure SetFilename(FName: String);
    function IsAutoBin(Head:string):TStrings;
    property OnUpload: TNotifyEvent read FOnUpload write FOnUpload;
  end;

var
  FFileUpload: TFFileUpload;
  FileName: String;
  OrigWidth, OrigHeight: Integer;

implementation

{$R *.lfm}

{ TFFileUpload }

procedure TFFileUpload.SetFilename(FName: String);
begin
  FileName := FName;
end;

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

function TFFileUpload.CalculateCRC(const Data: array of Byte): Word;
const
  POLYNOMIAL = $1021;
var
  CRC: Word;
  i, j: Integer;
begin
  CRC := $FFFF; // Initialwert

  for i := 0 to High(Data) do
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

  Result := CRC and $FFFF; // Nur die unteren 16 Bit
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

function TFFileUpload.IsAutoBin(Head:string):TStrings;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  Result := TStringList.Create;
  Result.AddStrings(['', '', '', '', '']);

  try
    Regex.Expression := '#(BIN|OK)#(?:(\d*)#\|(\d*)#\$(.*)\?#(.*))?';
    Regex.ModifierI := True;

    if Regex.Exec(Text) then
    begin
      Result[0] := Regex.Match[0]; // BIN or OK Message
      Result[1] := Regex.Match[2]; // File length
      Result[2] := Regex.Match[3]; // CRC
      Result[3] := Regex.Match[4]; // MSDOS DateTime
      Result[4] := Regex.Match[5]; // Filename Uppercase
    end;
  finally
    Regex.Free;
  end;
end;


end.

