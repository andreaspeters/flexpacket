unit uCmdBoxCustom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCmdBox, Controls, Dialogs, ClipBrd, Graphics;

type
  TCmdBoxAccess = class(TCmdBox);
  TCmdBoxCustom = class(TCmdBox)
  private
    SelectActive: Boolean;
    SelectStartRow, SelectStartCol: Integer;
    SelectEndRow, SelectEndCol: Integer;
    StringBuffer: TStringList;
    WriteBuffer: String;
    procedure WMMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    function GetTextInRange(Lines: TStringList; StartRow, StartCol, EndRow, EndCol: Integer): string;
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
  public
    procedure Write(S: String);
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Custom', [TCmdBoxCustom]);
end;

{ TCmdBoxCustom }

constructor TCmdBoxCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseWheel := @WMMouseWheel;
  StringBuffer := TStringList.Create;
  VerticalScrollbarVisible := True;
end;


procedure TCmdBoxCustom.Paint;
var R: TRect;
   CharHeight: Integer;
begin
  inherited Paint;

  CharHeight := abs(Font.Height)+3;

  R.Left   := SelectStartCol * GraphicalCharacterWidth;
  R.Top    := SelectStartRow * CharHeight;
  R.Right  := SelectEndCol * GraphicalCharacterWidth;
  R.Bottom := (SelectEndRow + 1) * CharHeight;

  Canvas.Pen.Color := clRed;
  Canvas.Brush.Style := bsClear;
  Canvas.Rectangle(R);
end;

procedure TCmdBoxCustom.WMMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    if Self.TopLine = 0 then
      Self.TopLine := 0
    else
      Self.TopLine := Self.TopLine - 1
  else
    Self.TopLine := Self.TopLine + 1;
  Handled := True
end;


procedure TCmdBoxCustom.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CharHeight: Integer;
begin
  inherited MouseDown(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    CharHeight := abs(Font.Height)+3;
    SelectStartRow := Y div CharHeight;
    SelectStartCol := X div GraphicalCharacterWidth;

    SelectEndRow := SelectStartRow;
    SelectEndCol := SelectStartCol;
    SelectActive := True;
    Invalidate;
  end;
end;

procedure TCmdBoxCustom.MouseMove(Shift: TShiftState; X, Y: Integer);
var CharHeight: Integer;
begin
  inherited MouseMove(Shift, X, Y);
  if SelectActive then
  begin
    CharHeight := abs(Font.Height)+3;
    SelectEndRow := Y div CharHeight;
    SelectEndCol := X div GraphicalCharacterWidth;
    Invalidate;
  end;
end;

procedure TCmdBoxCustom.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if Button = mbLeft then
  begin
    SelectActive := False;
    Clipboard.AsText := GetTextInRange(StringBuffer, SelectStartRow + TopLine, SelectStartCol, SelectEndRow + TopLine, SelectEndCol);
    SelectStartRow := -1;
    SelectEndRow := -1;
    Invalidate;
  end;
end;

procedure TCmdBoxCustom.Write(S: string);
var
  Lines: TStringList;
  i: Integer;
  LastIsComplete: Boolean;
begin
  inherited Write(S);

  S := StringReplace(S, #13#10, #10, [rfReplaceAll]);

  // String buffer, for the case the last line was not finished
  if WriteBuffer <> '' then
    S := WriteBuffer + S;

  LastIsComplete := (S <> '') and (S[Length(S)] = #10);

  Lines := TStringList.Create;
  try
    Lines.Text := StringReplace(S, #10, sLineBreak, [rfReplaceAll]);
    for i := 0 to Lines.Count - 1 do
    begin
      // add the line only if it's ending
      if (i < Lines.Count - 1) or LastIsComplete then
        StringBuffer.Add(Lines[i]+#13#10);
    end;

    // if the line does not has an enb, buffer it.
    if not LastIsComplete then
      WriteBuffer := Lines[Lines.Count - 1]
    else
      WriteBuffer := '';
  finally
    Lines.Free;
  end;
end;


function TCmdBoxCustom.GetTextInRange(Lines: TStringList; StartRow, StartCol, EndRow, EndCol: Integer): String;
var Row: Integer;
    LineText: String;
begin
  Result := '';

  if not Assigned(Lines) then
    Exit;

  if (Lines.Count <= 0) or (EndRow > Lines.Count-1) then
    Exit;

  if (EndRow = StartRow) and (EndCol = StartCol) then
    Exit;

  for Row := StartRow to EndRow do
  begin
    if Row >= Lines.Count then
      Exit;
    if Row < 0 then
      Continue;

    LineText := Lines[Row];

    if Row = StartRow then
      LineText := Copy(Lines[Row], StartCol, Length(LineText));

    if Row = EndRow then
      LineText := Copy(Lines[Row], 0, EndCol+1);

    if (Row = StartRow) and (Row = EndRow) then
      LineText := Copy(Lines[Row], StartCol, EndCol+1);

    Result := Result + LineText;
  end;
end;

end.

