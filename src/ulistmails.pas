unit ulistmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids,
  utypes, RegExpr;

type

  { TFListMails }

  TFListMails = class(TForm)
    BTDefaultButtons: TButtonPanel;
    StringGrid1: TStringGrid;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesToGrid;
    procedure AutoSizeStringGridColumns;
    function ParseMessageHeader(const FileName: String): TMessageHeader;
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  FListMails: TFListMails;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TFListMails }

procedure TFListMails.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

procedure TFListMails.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFListMails.FormShow(Sender: TObject);
begin
  ListFilesToGrid;
end;

procedure TFListMails.ListFilesToGrid;
var
  SR: TSearchRec;
  Row: Integer;
  Path: String;
  Header: TMessageHeader;
begin
  Path := FPConfig^.DirectoryMail;

  StringGrid1.RowCount := 1;
  StringGrid1.ColCount := 6;

  StringGrid1.Cells[0, 0] := 'Date';
  StringGrid1.Cells[1, 0] := 'From';
  StringGrid1.Cells[2, 0] := 'To';
  StringGrid1.Cells[3, 0] := 'Subject';
  StringGrid1.Cells[4, 0] := 'Size (Bytes)';
  StringGrid1.Cells[5, 0] := 'Filename';

  Row := 1;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Header := ParseMessageHeader(Path + DirectorySeparator + SR.Name);

        StringGrid1.RowCount := Row + 1;
        StringGrid1.Cells[0, Row] := DateTimeToStr(FileDateToDateTime(SR.Time));
        StringGrid1.Cells[1, Row] := Header.FromCall;
        StringGrid1.Cells[2, Row] := Header.ToCall;
        StringGrid1.Cells[3, Row] := Header.Subject;
        StringGrid1.Cells[4, Row] := IntToStr(SR.Size);
        StringGrid1.Cells[5, Row] := SR.Name;
        Inc(Row);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  AutoSizeStringGridColumns;
end;

function TFListMails.ParseMessageHeader(const FileName: String): TMessageHeader;
var Regex: TRegExpr;
    sl: TStringList;
    i: Integer;
    Line: String;
begin
  FillChar(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);

    if sl.Count > 0 then
    begin
      Line := sl[0];
      Regex := TRegExpr.Create;
      Regex.Expression := '^(\S+).*>.*(\S+).*(\d{2}\.\d{2}\.\d{2}) (\d{2}:\d{2}z) (\d+) Lines (\d+) Bytes.*';
      Regex.ModifierI := True;

      if Regex.Exec(Line) then
      begin
        Result.DateStr := Regex.Match[3];
        Result.TimeStr := Regex.Match[4];
        Result.Lines := StrToInt(Regex.Match[5]);
        Result.Bytes := StrToInt(Regex.Match[6]);
      end
    end;

    for i := 1 to sl.Count - 1 do
    begin
      Line := sl[i];
      if Line.StartsWith('From:') then
        Result.FromCall := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('To  :') then
        Result.ToCall := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('BID :') then
        Result.BID := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('Read:') then
        Result.ReadBy := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('Subj:') then
        Result.Subject := Trim(Copy(Line, 6, Length(Line)));
    end;

  finally
    sl.Free;
  end;
end;

procedure TFListMails.AutoSizeStringGridColumns;
var Col, Row, W, MaxWidth: Integer;
    CellText: string;
    ACanvas: TCanvas;
begin
  ACanvas := StringGrid1.Canvas;

  for Col := 0 to StringGrid1.ColCount - 1 do
  begin
    MaxWidth := 0;
    for Row := 0 to StringGrid1.RowCount - 1 do
    begin
      CellText := StringGrid1.Cells[Col, Row];
      W := ACanvas.TextWidth(CellText) + 10; // +10 für Abstand/Padding
      if W > MaxWidth then
        MaxWidth := W;
    end;
    StringGrid1.ColWidths[Col] := MaxWidth;
  end;
end;

end.

