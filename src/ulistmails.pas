unit ulistmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids,
  PairSplitter, Menus, RichMemo, utypes, RegExpr, FileUtil, ufileupload, LConvEncoding;

type

  { TFListMails }

  TFListMails = class(TForm)
    BTDefaultButtons: TButtonPanel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pmMailList: TPopupMenu;
    sdSaveAs: TSaveDialog;
    trmShowMail: TRichMemo;
    sgMailList: TStringGrid;
    procedure CloseButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesToGrid;
    procedure AutoSizeStringGridColumns;
    procedure sgMailListClick(Sender: TObject);
    procedure SortGridByDate;
    function ParseMessageHeader(const FileName: String): TMessageHeader;
    function ParseDateTimeString(const S: String): TDateTime;
    function IsGoSeven(const FileName: String): String;
  private

  public
    procedure SetConfig(Config: PTFPConfig);
    procedure DeleteMail;
    procedure ExportGo7;
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
  sgMailList.FixedCols := 0;
  ListFilesToGrid;
  SortGridByDate;
  PairSplitter1.Position := FListMails.Height div 2;
  trmShowMail.Font.Name := FPConfig^.TerminalFontName;
end;

procedure TFListMails.ExportGo7;
var FileName, Go7FileName: String;
    Row: Integer;
begin
  Row := sgMailList.Row;
  if Row <= 0 then
    Exit;

  FileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[7, Row];
  Go7FileName := IsGoSeven(FileName);
  if Length(Go7FileName) > 0 then
  begin
    sdSaveAs.InitialDir := FPConfig^.Directory7Plus;
    sdSaveAs.FileName := Go7FileName;
    if sdSaveAs.Execute then
      CopyFile(FileName, sdSaveAs.FileName);
  end
end;

procedure TFListMails.DeleteMail;
var Row: Integer;
    FileName: String;
    RowsToDelete: TList;
    i: Integer;
begin
  if sgMailList.Selection.Top <= 0 then
    Exit;

  if MessageDlg('Sure you want to delete the selected Mails?', mtConfirmation, [mbOK, mbCancel], 0) <> mrOK then
    Exit;

  RowsToDelete := TList.Create;
  try
    // get all selected rows
    for Row := sgMailList.Selection.Top to sgMailList.Selection.Bottom do
      if Row > 0 then // ignore header line
        RowsToDelete.Add(Pointer(Row));

    // delete from down to top to preserve index
    for i := RowsToDelete.Count -1 downto 0 do
    begin
      Row := Integer(RowsToDelete[i]);

      FileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[7, Row];

      if FileExists(FileName) then
        if not DeleteFile(FileName) then
          ShowMessage('Could not delete file: ' + FileName);

      sgMailList.DeleteRow(Row);
    end;

    ListFilesToGrid;
    SortGridByDate;
  finally
    RowsToDelete.Free;
  end;
end;


function TFListMails.IsGoSeven(const FileName: String): String;
var FileStream: TextFile;
    Line, Tmp, Go7FileName, FExt: String;
    Start, Stop: Boolean;
    Regex: TRegExpr;
    i: Integer;
begin
  Start := False;
  Stop := False;
  Go7FileName := '';
  Result := '';

  if not FileExists(FileName) then
    Exit;

  AssignFile(FileStream, FileName);
  try
    Reset(FileStream);
    while not Eof(FileStream) do
    begin
      ReadLn(FileStream, Tmp);
      for i := 1 to Length(Tmp) do
        if Tmp[i] in [#32..#126] then  // Behalte nur druckbare ASCII-
          Line := Line + Tmp[i];
    end;
  finally
    CloseFile(FileStream);
  end;


  if (Pos('go_7+.', Line) > 0) then
    Start := True;

  if (Pos('stop_7+.', Line) > 0) then
    Stop := True;

  Regex := TRegExpr.Create;
  Regex.Expression := 'stop_7+...(?:\()?(\S+)\/.*';
  Regex.ModifierI := True;

  if Regex.Exec(Line) then
    Go7FileName := Regex.Match[1];

  if Start and Stop and (Length(Go7FileName) > 0) then
  begin
    FExt := LowerCase(ExtractFileExt(Go7FileName));
    Go7FileName := ChangeFileExt(Go7FileName, FExt);
    Result := Go7FileName;
    Exit;
  end;

  if (not Start) and (not Stop) and (Length(Go7FileName) <= 0) then
  begin
    ShowMessage('This is not a Go7 file');
    Exit;
  end;

  if not (Start and Stop and (Length(Go7FileName) > 0)) then
    ShowMessage('This Go7 data are broken');
end;

procedure TFListMails.ListFilesToGrid;
var
  SR: TSearchRec;
  Row: Integer;
  Path: String;
  Header: TMessageHeader;
begin
  Path := FPConfig^.DirectoryMail;

  sgMailList.Clear;
  sgMailList.RowCount := 1;
  sgMailList.ColCount := 8;

  sgMailList.Cells[0, 0] := 'Nr';
  sgMailList.Cells[1, 0] := 'Date';
  sgMailList.Cells[2, 0] := 'Time';
  sgMailList.Cells[3, 0] := 'Subject';
  sgMailList.Cells[4, 0] := 'From';
  sgMailList.Cells[5, 0] := 'To';
  sgMailList.Cells[6, 0] := 'Size (Bytes)';
  sgMailList.Cells[7, 0] := 'Filename';

  Row := 1;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Header := ParseMessageHeader(Path + DirectorySeparator + SR.Name);

        sgMailList.RowCount := Row + 1;
        sgMailList.Cells[1, Row] := Header.DateStr;
        sgMailList.Cells[2, Row] := Header.TimeStr;
        sgMailList.Cells[3, Row] := Header.Subject;
        sgMailList.Cells[4, Row] := Header.FromCall;
        sgMailList.Cells[5, Row] := Header.ToCall;
        sgMailList.Cells[6, Row] := IntToStr(SR.Size);
        sgMailList.Cells[7, Row] := SR.Name;

        // Fallback
        if Length(Header.FromCall) <= 0 then
          sgMailList.Cells[4, Row] := Header.FromCall2;

        if Length(Header.ToCall) <= 0 then
          sgMailList.Cells[5, Row] := Header.ToCall2;

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
        Date1 := ParseDateTimeString(sgMailList.Cells[1, i] + ' ' + sgMailList.Cells[2, i]);
      except
        Date1 := 0;
      end;

      try
        Date2 := ParseDateTimeString(sgMailList.Cells[1, j] + ' ' + sgMailList.Cells[2, j]);
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
  for i := 1 to RowCount -1 do
    sgMailList.Cells[0, i] := IntToStr(i);
end;

function TFListMails.ParseDateTimeString(const S: String): TDateTime;
var
  FS: TFormatSettings;
  CleanStr: string;
begin
  Result := Now;

  // FormatSettings konfigurieren
  FS := DefaultFormatSettings;
  FS.DateSeparator := '.';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'dd.mm.yy';
  FS.ShortTimeFormat := 'hh:nn';

  // das 'z' entfernen
  CleanStr := StringReplace(S, 'z', '', [rfIgnoreCase]);

  Result := StrToDateTime(CleanStr, FS);
end;


function TFListMails.ParseMessageHeader(const FileName: String): TMessageHeader;
var Regex: TRegExpr;
    sl: TStringList;
    i: Integer;
    Line: String;
    parts: TStringArray;
begin
  FillChar(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);

    if sl.Count > 0 then
    begin
      Line := sl[1];
      Regex := TRegExpr.Create;
      Regex.Expression := '^(\S+).*>.*(\S+).*(\d{2}\.\d{2}\.\d{2}) (\d{2}:\d{2}z) (\d+) Lines (\d+) Bytes.*@ (\S+)';
      Regex.ModifierI := True;

      if Regex.Exec(Line) then
      begin
        Result.FromCall2 := Regex.Match[1];
        Result.ToCall2 := Regex.Match[7];
        Result.DateStr := Regex.Match[3];
        Result.TimeStr := Regex.Match[4];
        Result.Lines := StrToInt(Regex.Match[5]);
        Result.Bytes := StrToInt(Regex.Match[6]);
      end
    end;

    for i := 2 to sl.Count - 1 do
    begin
      Line := sl[i];

      // Does not have to read the whole file.
      if FFileUpload.LineContainsKeyword(Line) <= 0 then
        Exit;

      if Line.StartsWith('From:') then
        Result.FromCall := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('To  :') then // For OpenBCM
        Result.ToCall := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('To:') then  // For LinBQP BBS
        Result.ToCall := Trim(Copy(Line, 4, Length(Line)))
      else if Line.StartsWith('MID :') then // For OpenBCM
        Result.MID := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('BID :') then // For OpenBCM
        Result.BID := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('Bid:') then // For LinBQP BBS
        Result.BID := Trim(Copy(Line, 5, Length(Line)))
      else if Line.StartsWith('Read:') then // For OpenBCM
        Result.ReadBy := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('Subj:') then // For OpenBCM
        Result.Subject := Trim(Copy(Line, 6, Length(Line)))
      else if Line.StartsWith('Title:') then // For LinBQP BBS
        Result.Subject := Trim(Copy(Line, 7, Length(Line)))
      else if Line.StartsWith('Date/Time:') then // For LinBQP BBS
      begin
        parts := Line.Split([' ']);
        Result.DateStr := Regex.Match[1];
        Result.TimeStr := Regex.Match[2];
        if Pos('Z', Result.TimeStr) > 0 then
           Delete(Result.TimeStr, Length(Result.TimeStr), 1);
      end

    end;

  finally
    sl.Free;
  end;
end;

procedure TFListMails.sgMailListClick(Sender: TObject);
var raw: RawByteString;
    utf8Text: String;
    FileName: String;
begin
  fileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[7, sgMailList.Row];

  if not FileExists(FileName) then
     Exit;

  raw := LoadFileAsRawByteString(fileName);
  utf8Text := CP437ToUTF8(raw);

  trmShowMail.Lines.Text := utf8Text;
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
  sgMailList.ColWidths[7] := 0; // hide filename col
end;

end.

