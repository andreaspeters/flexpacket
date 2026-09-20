unit umheard;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Grids, RegExpr;

type
  TFMHeard = class(TForm)
    sgMHeard: TStringGrid;
    procedure FormCreate(Sender: TObject);
  private
    procedure ConfigureGrid;
    procedure AddHeader(const Header: String);
  public
    procedure AddMonitorData(const Data: String);
    function GetMHeardList: String;
  end;

var
  FMHeard: TFMHeard;

implementation

{$R *.lfm}

procedure TFMHeard.FormCreate(Sender: TObject);
begin
  ConfigureGrid;
end;

procedure TFMHeard.ConfigureGrid;
begin
  sgMHeard.FixedRows := 1;
  sgMHeard.ColCount := 5;
  sgMHeard.RowCount := 1;
  sgMHeard.Cells[0, 0] := 'Date';
  sgMHeard.Cells[1, 0] := 'Time';
  sgMHeard.Cells[2, 0] := 'Callsign from';
  sgMHeard.Cells[3, 0] := 'Callsign to';
  sgMHeard.Cells[4, 0] := 'Via';
  sgMHeard.ColWidths[0] := 90;
  sgMHeard.ColWidths[1] := 75;
  sgMHeard.ColWidths[2] := 110;
  sgMHeard.ColWidths[3] := 150;
  sgMHeard.ColWidths[4] := 110;
end;

procedure TFMHeard.AddHeader(const Header: String);
var
  Regex: TRegExpr;
  Callsign, Path, Via: String;
  Row, I: Integer;
  NowDate, NowTime: String;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*fm\s+(\S+)\s+to\s+(\S+)(?:\s+via\s+(.+?))?\s+ctl\s+';
    Regex.ModifierI := True;
    if not Regex.Exec(Header) then
      Exit;

    Callsign := UpperCase(Trim(Regex.Match[1]));
    Path := Trim(Regex.Match[2]);
    Via := '';
    if Regex.SubExprMatchCount >= 3 then
      Via := Trim(Regex.Match[3]);

    NowDate := FormatDateTime('yyyy-mm-dd', Now);
    NowTime := FormatDateTime('hh:nn:ss', Now);

    for I := 1 to sgMHeard.RowCount - 1 do
      if SameText(sgMHeard.Cells[2, I], Callsign) and
         SameText(sgMHeard.Cells[3, I], Path) and
         SameText(sgMHeard.Cells[4, I], Via) then
      begin
        sgMHeard.DeleteRow(I);
        Break;
      end;

    sgMHeard.InsertRowWithValues(1, [NowDate, NowTime, Callsign, Path, Via]);
    Row := sgMHeard.RowCount - 1;
    sgMHeard.Row := 1;
  finally
    Regex.Free;
  end;
end;

procedure TFMHeard.AddMonitorData(const Data: String);
var
  Lines: TStringList;
  I: Integer;
begin
  Lines := TStringList.Create;
  try
    Lines.Text := StringReplace(Data, #13, '', [rfReplaceAll]);
    for I := 0 to Lines.Count - 1 do
      if Pos(' fm ', ' ' + LowerCase(Lines[I])) > 0 then
        AddHeader(Trim(Lines[I]));
  finally
    Lines.Free;
  end;
end;

function TFMHeard.GetMHeardList: String;
var
  I: Integer;
  RowText: String;
begin
  Result := '';
  if not Assigned(sgMHeard) or (sgMHeard.RowCount <= 1) then
    Exit;

  // Header row - skip
  for I := 1 to sgMHeard.RowCount - 1 do
  begin
    RowText := sgMHeard.Cells[0, I] + ' ' +
               sgMHeard.Cells[1, I] + ' ' +
               sgMHeard.Cells[2, I] + ' ' +
               sgMHeard.Cells[3, I] + ' ' +
               sgMHeard.Cells[4, I];
    Result := Result + RowText + LineEnding;
  end;
end;

initialization
  RegisterClass(TFMHeard);

end.
