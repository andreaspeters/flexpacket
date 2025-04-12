unit ulistmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids,
  PairSplitter, RichMemo, utypes, RegExpr;

type

  { TFListMails }

  TFListMails = class(TForm)
    BTDefaultButtons: TButtonPanel;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    trmShowMail: TRichMemo;
    sgMailList: TStringGrid;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesToGrid;
    procedure AutoSizeStringGridColumns;
    procedure sgMailListClick(Sender: TObject);
    procedure SortGridByDate;
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
  SortGridByDate;
  PairSplitter1.Position := FListMails.Height div 2;
  trmShowMail.Font.Name := FPConfig^.TerminalFontName;
end;

procedure TFListMails.ListFilesToGrid;
var
  SR: TSearchRec;
  Row: Integer;
  Path: String;
  Header: TMessageHeader;
begin
  Path := FPConfig^.DirectoryMail;

  sgMailList.RowCount := 1;
  sgMailList.ColCount := 6;

  sgMailList.Cells[0, 0] := 'Date';
  sgMailList.Cells[1, 0] := 'Subject';
  sgMailList.Cells[2, 0] := 'From';
  sgMailList.Cells[3, 0] := 'To';
  sgMailList.Cells[4, 0] := 'Size (Bytes)';
  sgMailList.Cells[5, 0] := 'Filename';

  Row := 1;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Header := ParseMessageHeader(Path + DirectorySeparator + SR.Name);

        sgMailList.RowCount := Row + 1;
        sgMailList.Cells[0, Row] := DateTimeToStr(FileDateToDateTime(SR.Time));
        sgMailList.Cells[1, Row] := Header.Subject;
        sgMailList.Cells[2, Row] := Header.FromCall;
        sgMailList.Cells[3, Row] := Header.ToCall;
        sgMailList.Cells[4, Row] := IntToStr(SR.Size);
        sgMailList.Cells[5, Row] := SR.Name;
        Inc(Row);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  AutoSizeStringGridColumns;
end;

procedure TFListMails.SortGridByDate;
var  i, j, Col, RowCount: Integer;
     Date1, Date2: TDateTime;
     Temp: String;
begin
  RowCount := sgMailList.RowCount;
  Col := sgMailList.ColCount;

  // BubbleSort
  for i := 1 to RowCount - 2 do
    for j := i + 1 to RowCount - 1 do
    begin
      try
        Date1 := StrToDateTime(sgMailList.Cells[0, i]);
      except
        Date1 := 0;
      end;

      try
        Date2 := StrToDateTime(sgMailList.Cells[0, j]);
      except
        Date2 := 0;
      end;

      if Date1 < Date2 then
      begin
        for Col := 0 to sgMailList.ColCount - 1 do
        begin
          Temp := sgMailList.Cells[Col, i];
          sgMailList.Cells[Col, i] := sgMailList.Cells[Col, j];
          sgMailList.Cells[Col, j] := Temp;
        end;
      end;
    end;
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

procedure TFListMails.sgMailListClick(Sender: TObject);
var sl: TStringList;
begin
  sl := TStringList.Create;
  sl.LoadFromFile(FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[5, sgMailList.Row]);
  trmShowMail.Lines := sl;
end;

procedure TFListMails.AutoSizeStringGridColumns;
var Col, Row, W, MaxWidth: Integer;
    CellText: string;
    ACanvas: TCanvas;
begin
  ACanvas := sgMailList.Canvas;

  for Col := 0 to sgMailList.ColCount - 1 do
  begin
    MaxWidth := 0;
    for Row := 0 to sgMailList.RowCount - 1 do
    begin
      CellText := sgMailList.Cells[Col, Row];
      W := ACanvas.TextWidth(CellText) + 10; // +10 für Abstand/Padding
      if W > MaxWidth then
        MaxWidth := W;
    end;
    sgMailList.ColWidths[Col] := MaxWidth;
  end;
end;

end.

