unit ulistmails;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Grids,
  PairSplitter, Menus, ComCtrls, ActnList, RichMemo, utypes, RegExpr, FileUtil,
  LConvEncoding, PrintersDlgs, Printers, Types, LazUTF8;

type

  { TFListMails }

  TFListMails = class(TForm)
    actExportGo7: TAction;
    actDeleteMail: TAction;
    actClose: TAction;
    actBold: TAction;
    actPrint: TAction;
    actList: TActionList;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitterSide1: TPairSplitterSide;
    PairSplitterSide2: TPairSplitterSide;
    pmMailList: TPopupMenu;
    sdSaveAs: TSaveDialog;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    ToolButton7: TToolButton;
    trmShowMail: TRichMemo;
    sgMailList: TStringGrid;
    procedure actBoldExecute(Sender: TObject);
    procedure actCloseExecute(Sender: TObject);
    procedure actDeleteMailExecute(Sender: TObject);
    procedure actExportGo7Execute(Sender: TObject);
    procedure actPrintExecute(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure ListFilesToGrid;
    procedure AutoSizeStringGridColumns;
    procedure sgMailListClick(Sender: TObject);
    procedure sgMailListDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure SortGridByDate;
    procedure PrintMultilineText(const AText, Subject: String);
    function ParseMessageHeader(const FileName: String): TMessageHeader;
    function ParseDateTimeString(const S: String): TDateTime;
    function IsGoSeven(const FileName: String): String;
    function ExpandTabs(const S: String; TabWidth: Integer): String;
    function IsMessageHeaderLine(const Line: String): Boolean;
    procedure DisplayMailText(const Raw: RawByteString);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  FListMails: TFListMails;
  FPConfig: PTFPConfig;
  OldWidth, OldHeight: Integer;

implementation

{$R *.lfm}

type
  TAnsiSpan = record
    Start: Integer;
    Length: Integer;
    Color: TColor;
    Styles: TFontStyles;
    HasColor: Boolean;
  end;

{ TFListMails }

procedure TFListMails.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

procedure TFListMails.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFListMails.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FPConfig^.MailX := Left;
  FPConfig^.MailY := Top;
end;

procedure TFListMails.FormCreate(Sender: TObject);
begin
  OldWidth := Width;
  OldHeight := Height;
end;

procedure TFListMails.actPrintExecute(Sender: TObject);
var utf8Text, FileName, Subject: String;
    Row: Integer;
    Raw: RawByteString;
begin
  Row := sgMailList.Row;
  if Row <= 0 then
    Exit;

  FileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[8, sgMailList.Row];
  Subject := sgMailList.Cells[4, sgMailList.Row];

  if not FileExists(FileName) then
     Exit;

  raw := LoadFileAsRawByteString(FileName);
  utf8Text := CP437ToUTF8(raw);

  PrintMultilineText(utf8Text, Subject);
end;

function TFListMails.ExpandTabs(const S: string; TabWidth: Integer): string;
var
  i, Col: Integer;
  ResultStr: string;
begin
  Col := 0;
  ResultStr := '';
  for i := 1 to Length(S) do
  begin
    if S[i] = #9 then
    begin
      repeat
        ResultStr := ResultStr + ' ';
        Inc(Col);
      until (Col mod TabWidth = 0);
    end
    else
    begin
      ResultStr := ResultStr + S[i];
      Inc(Col);
    end;
  end;
  Result := ResultStr;
end;


procedure TFListMails.PrintMultilineText(const AText, Subject: string);
var
  PrintDlg: TPrintDialog;
  Paragraphs: TStringList;
  i, Y, LineHeight: Integer;
  Line: string;
begin
  PrintDlg := TPrintDialog.Create(nil);
  try
    if not PrintDlg.Execute then
      Exit;

    Paragraphs := TStringList.Create;
    try
      Paragraphs.Text := AText;  // behält CR/LF-Zeilen bei
      Printer.Title := Subject;

      Printer.BeginDoc;
      try
        Printer.Canvas.Font.Name := FPConfig^.TerminalFontName;
        Printer.Canvas.Font.Size := FPConfig^.TerminalFontSize;

        LineHeight := Printer.Canvas.TextHeight('Hg');
        Y := 100; // oberer Rand

        for i := 0 to Paragraphs.Count - 1 do
        begin
          Line := ExpandTabs(Paragraphs[i], 8); // Tabs → Spaces

          Printer.Canvas.TextOut(100, Y, Line);
          Inc(Y, LineHeight);

          // Seitenumbruch prüfen
          if Y + LineHeight > Printer.PageHeight - 100 then
          begin
            Printer.NewPage;
            Y := 100;
          end;
        end;

      finally
        Printer.EndDoc;
      end;
    finally
      Paragraphs.Free;
    end;

  finally
    PrintDlg.Free;
  end;
end;

procedure TFListMails.FormResize(Sender: TObject);
begin
  FPConfig^.MailWidth := Width;
  FPConfig^.MailHeight := Height;
end;

procedure TFListMails.FormShow(Sender: TObject);
begin
  // fix for wayland
  Height := OldHeight;
  Width := OldWidth;

  sgMailList.FixedCols := 0;
  ListFilesToGrid;
  SortGridByDate;
  PairSplitter1.Position := FListMails.Height div 2;
  trmShowMail.Font.Name := FPConfig^.TerminalFontName;
  trmShowMail.Font.Bold := FPConfig^.MailFontBold;
  Width := FPConfig^.MailWidth;
  Height := FPConfig^.MailHeight;

  if (FPConfig^.MailX > 0) and (FPConfig^.MailY > 0) then
  begin
    Left := FPConfig^.MailX;
    Top := FPConfig^.MailY;
  end;
end;

procedure TFListMails.actExportGo7Execute(Sender: TObject);
var FileName, Go7FileName: String;
    Row: Integer;
begin
  Row := sgMailList.Row;
  if Row <= 0 then
    Exit;

  FileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[8, Row];
  Go7FileName := IsGoSeven(FileName);
  if Length(Go7FileName) > 0 then
  begin
    sdSaveAs.InitialDir := FPConfig^.Directory7Plus;
    sdSaveAs.FileName := Go7FileName;
    if sdSaveAs.Execute then
      CopyFile(FileName, sdSaveAs.FileName);
  end
end;

procedure TFListMails.actDeleteMailExecute(Sender: TObject);
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

      FileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[8, Row];

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

procedure TFListMails.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TFListMails.actBoldExecute(Sender: TObject);
begin
  if trmShowMail.Font.Bold then
    trmShowMail.Font.Bold := False
  else
    trmShowMail.Font.Bold := True;

  FPConfig^.MailFontBold := trmShowMail.Font.Bold;
end;

function TFListMails.IsMessageHeaderLine(const Line: String): Boolean;
var
  HeaderLine: String;
begin
  HeaderLine := TrimLeft(Line);
  Result := HeaderLine.StartsWith('Read:') or
    HeaderLine.StartsWith('Subj:') or
    HeaderLine.StartsWith('Path:') or
    HeaderLine.StartsWith('Sent:') or
    HeaderLine.StartsWith('From:') or
    HeaderLine.StartsWith('To:') or
    HeaderLine.StartsWith('To  :') or
    HeaderLine.StartsWith('X-Info:') or
    HeaderLine.StartsWith('BID:') or
    HeaderLine.StartsWith('BID :') or
    HeaderLine.StartsWith('Bid:') or
    HeaderLine.StartsWith('MID:') or
    HeaderLine.StartsWith('MID :') or
    HeaderLine.StartsWith('Title:') or
    HeaderLine.StartsWith('Date/Time:') or
    HeaderLine.StartsWith('Body:') or
    HeaderLine.StartsWith('Type/Status:');
end;

procedure TFListMails.DisplayMailText(const Raw: RawByteString);
var
  MailText, PlainText, Params: String;
  Spans: array of TAnsiSpan;
  Foreground, Background: TColor;
  Styles: TFontStyles;
  HasForeground, Reverse: Boolean;
  I, J, K, SpanStart, CodeStart, CodeValue: Integer;

  function AnsiColor(const Code: Integer): TColor;
  begin
    case Code of
      0: Result := clBlack;
      1: Result := clRed;
      2: Result := clGreen;
      3: Result := clYellow;
      4: Result := clBlue;
      5: Result := clFuchsia;
      6: Result := clAqua;
      7: Result := clWhite;
    else
      Result := clWhite;
    end;
  end;

  procedure AddSpan(const AStart, ALength: Integer);
  var
    Span: TAnsiSpan;
  begin
    if ALength <= 0 then
      Exit;
    Span.Start := AStart;
    Span.Length := ALength;
    Span.Styles := Styles;
    Span.HasColor := HasForeground;
    if Reverse then
      Span.Color := Background
    else
      Span.Color := Foreground;
    SetLength(Spans, Length(Spans) + 1);
    Spans[High(Spans)] := Span;
  end;

  procedure ApplyCode(const Code: Integer);
  begin
    case Code of
      0:
        begin
          Foreground := FPConfig^.TerminalFontColor;
          Background := trmShowMail.Color;
          Styles := [];
          HasForeground := False;
          Reverse := False;
        end;
      1: Include(Styles, fsBold);
      4: Include(Styles, fsUnderline);
      7: Reverse := True;
      22: Exclude(Styles, fsBold);
      24: Exclude(Styles, fsUnderline);
      27: Reverse := False;
      30..37:
        begin
          Foreground := AnsiColor(Code - 30);
          HasForeground := True;
        end;
      39: HasForeground := False;
      40..47: Background := AnsiColor(Code - 40);
      49: Background := trmShowMail.Color;
    end;
  end;

begin
  MailText := CP437ToUTF8(Raw);
  PlainText := '';
  SetLength(Spans, 0);
  Foreground := FPConfig^.TerminalFontColor;
  Background := trmShowMail.Color;
  Styles := [];
  HasForeground := False;
  Reverse := False;
  I := 1;
  while I <= Length(MailText) do
  begin
    if (MailText[I] = #27) and (I < Length(MailText)) and (MailText[I + 1] = '[') then
    begin
      J := I + 2;
      while (J <= Length(MailText)) and (MailText[J] <> 'm') do
        Inc(J);
      if J <= Length(MailText) then
      begin
        Params := Copy(MailText, I + 2, J - I - 2);
        if Params = '' then
          ApplyCode(0)
        else
        begin
          CodeStart := 1;
          for K := 1 to Length(Params) + 1 do
            if (K > Length(Params)) or (Params[K] = ';') then
            begin
              if TryStrToInt(Copy(Params, CodeStart, K - CodeStart), CodeValue) then
                ApplyCode(CodeValue);
              CodeStart := K + 1;
            end;
        end;
        I := J + 1;
        Continue;
      end;
    end;

    SpanStart := UTF8Length(PlainText);
    J := I;
    while (J <= Length(MailText)) and not ((MailText[J] = #27) and
      (J < Length(MailText)) and (MailText[J + 1] = '[')) do
      Inc(J);
    PlainText := PlainText + Copy(MailText, I, J - I);
    AddSpan(SpanStart, UTF8Length(PlainText) - SpanStart);
    I := J;
  end;

  trmShowMail.Lines.Text := PlainText;
  for K := 0 to High(Spans) do
  begin
    if Spans[K].HasColor then
      trmShowMail.SetRangeColor(Spans[K].Start, Spans[K].Length,
        Spans[K].Color);
    if Spans[K].Styles <> [] then
      trmShowMail.SetRangeParams(Spans[K].Start, Spans[K].Length,
        [tmm_Styles], '', 0, 0, Spans[K].Styles, []);
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
  Line := '';
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
  try
    Regex.Expression := 'stop_7+...(?:\()?(\S+)\/.*';
    Regex.ModifierI := True;

    if Regex.Exec(Line) then
      Go7FileName := Regex.Match[1];
  finally
    Regex.Free;
  end;

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
  sgMailList.ColCount := 9;

  sgMailList.Cells[0, 0] := 'Nr';
  sgMailList.Cells[1, 0] := 'T';
  sgMailList.Cells[2, 0] := 'Date';
  sgMailList.Cells[3, 0] := 'Time';
  sgMailList.Cells[4, 0] := 'Subject';
  sgMailList.Cells[5, 0] := 'From';
  sgMailList.Cells[6, 0] := 'To';
  sgMailList.Cells[7, 0] := 'Size (Bytes)';
  sgMailList.Cells[8, 0] := 'Filename';

  Row := 1;

  if FindFirst(Path + DirectorySeparator + '*', faAnyFile and not faDirectory, SR) = 0 then
  begin
    repeat
      if (SR.Attr and faDirectory) = 0 then
      begin
        Header := ParseMessageHeader(Path + DirectorySeparator + SR.Name);

        sgMailList.RowCount := Row + 1;
        sgMailList.Cells[1, Row] := Header.MType;
        sgMailList.Cells[2, Row] := Header.DateStr;
        sgMailList.Cells[3, Row] := Header.TimeStr;
        sgMailList.Cells[4, Row] := Header.Subject;
        sgMailList.Cells[5, Row] := Header.FromCall;
        sgMailList.Cells[6, Row] := Header.ToCall;
        sgMailList.Cells[7, Row] := IntToStr(SR.Size);
        sgMailList.Cells[8, Row] := SR.Name;

        // Fallback
        if Length(Header.FromCall) <= 0 then
          sgMailList.Cells[5, Row] := Header.FromCall2;

        if Length(Header.ToCall) <= 0 then
          sgMailList.Cells[6, Row] := Header.ToCall2;

        Inc(Row);
      end;
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
  AutoSizeStringGridColumns;
end;

procedure TFListMails.SortGridByDate;
type
  TMailRow = record
    Cells: array[0..8] of String;
    SortDate: TDateTime;
  end;
var
  Rows: array of TMailRow;
  Row, Col, RowCount: Integer;

  procedure QuickSort(const Left, Right: Integer);
  var
    I, J: Integer;
    Pivot: TDateTime;
    Temp: TMailRow;
  begin
    I := Left;
    J := Right;
    Pivot := Rows[(Left + Right) div 2].SortDate;
    repeat
      while Rows[I].SortDate > Pivot do
        Inc(I);
      while Rows[J].SortDate < Pivot do
        Dec(J);
      if I <= J then
      begin
        Temp := Rows[I];
        Rows[I] := Rows[J];
        Rows[J] := Temp;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if Left < J then
      QuickSort(Left, J);
    if I < Right then
      QuickSort(I, Right);
  end;

begin
  RowCount := sgMailList.RowCount;
  if RowCount <= 2 then
  begin
    if RowCount = 2 then
      sgMailList.Cells[0, 1] := '1';
    Exit;
  end;

  SetLength(Rows, RowCount - 1);
  for Row := 1 to RowCount - 1 do
  begin
    for Col := 0 to sgMailList.ColCount - 1 do
      Rows[Row - 1].Cells[Col] := sgMailList.Cells[Col, Row];
    Rows[Row - 1].SortDate :=
      ParseDateTimeString(Rows[Row - 1].Cells[2] + ' ' + Rows[Row - 1].Cells[3]);
  end;

  QuickSort(0, High(Rows));
  for Row := 1 to RowCount - 1 do
  begin
    for Col := 0 to sgMailList.ColCount - 1 do
      sgMailList.Cells[Col, Row] := Rows[Row - 1].Cells[Col];
    sgMailList.Cells[0, Row] := IntToStr(Row);
  end;
end;

function TFListMails.ParseDateTimeString(const S: String): TDateTime;
var
  FS: TFormatSettings;
  CleanStr: string;
begin
  FS := DefaultFormatSettings;
  FS.DateSeparator := '.';
  FS.TimeSeparator := ':';
  FS.ShortDateFormat := 'dd.mm.yy';
  FS.ShortTimeFormat := 'hh:nn';

  CleanStr := Trim(StringReplace(S, 'z', '', [rfIgnoreCase]));
  if not TryStrToDateTime(CleanStr, Result, FS) then
    Result := EncodeDate(1970, 1, 1);
end;


function TFListMails.ParseMessageHeader(const FileName: String): TMessageHeader;
var Regex: TRegExpr;
    sl: TStringList;
    i, start: Integer;
    Line, tmp: String;
    parts: TStringArray;
begin
  FillChar(Result, SizeOf(Result), 0);
  sl := TStringList.Create;
  try
    sl.LoadFromFile(FileName);
    start := 0;

    if sl.Count > 0 then
    begin
      // search the bcm header
      Regex := TRegExpr.Create;
      try
      Regex.Expression := '^(\S+).*>.*(\S+).*(\d{2}\.\d{2}\.\d{2}) (\d{2}:\d{2}z) (\d+) Lines (\d+) Bytes.*@ (\S+)';
      Regex.ModifierI := True;
      for i := 0 to sl.Count - 1 do
      begin
        Line := sl[i];

        if Regex.Exec(Line) then
        begin
          if Regex.SubExprMatchCount >= 7 then
          begin
            Result.FromCall2 := Regex.Match[1];
            Result.ToCall2 := Regex.Match[7];
            Result.DateStr := Regex.Match[3];
            Result.TimeStr := Regex.Match[4];
            Result.Lines := StrToInt(Regex.Match[5]);
            Result.Bytes := StrToInt(Regex.Match[6]);
            start := i+1;
            break;
          end;
        end;
      end;
      finally
        Regex.Free;
      end;

      for i := start to sl.Count - 1 do
      begin
        Line := sl[i];

        // Header parsing stops at the first body line. This avoids scanning
        // and allocating for the complete mail body.
        if not IsMessageHeaderLine(Line) then
          Break;

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
          tmp := Trim(Copy(Line, 11, Length(Line)));
          parts := tmp.Split([' ']);
          Result.DateStr := parts[0];
          Result.TimeStr := parts[1];
          if Pos('Z', Result.TimeStr) > 0 then
             Delete(Result.TimeStr, Length(Result.TimeStr), 1);
        end;

        if Length(Result.MID) > 0 then
           Result.MType := 'M';
        if Length(Result.BID) > 0 then
           Result.MType := 'B';

      end;
    end;

  finally
    sl.Free;
  end;
end;

procedure TFListMails.sgMailListClick(Sender: TObject);
var
  Raw: RawByteString;
  FileName: String;
begin
  fileName := FPConfig^.DirectoryMail + DirectorySeparator + sgMailList.Cells[8, sgMailList.Row];

  if not FileExists(FileName) then
     Exit;

  raw := LoadFileAsRawByteString(fileName);
  DisplayMailText(raw);
end;

procedure TFListMails.sgMailListDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  with sgMailList do
  begin
    if ARow = 0 then
      Canvas.Font.Style := [fsBold]
    else
      Canvas.Font.Style := [];

    Canvas.FillRect(ARect);

    Canvas.TextRect(ARect, ARect.Left + 2, ARect.Top + 2, Cells[ACol, ARow]);
  end;
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
  sgMailList.ColWidths[8] := 0; // hide filename col
end;

end.

