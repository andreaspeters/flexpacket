unit uCmdBoxCustom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCmdBox, Controls, LCLType, LMessages, Dialogs, ClipBrd,
  Graphics;

type
  TTerminalInputEvent = procedure(Sender: TObject;
    const Data: RawByteString) of object;

  TCmdBoxAccess = class(TCmdBox);
  TCmdBoxCustom = class(TCmdBox)
  private
    SelectActive: Boolean;
    SelectStartRow, SelectStartCol: Integer;
    SelectEndRow, SelectEndCol: Integer;
    StringBuffer: TStringList;
    WriteBuffer: String;
    FOnTerminalInput: TTerminalInputEvent;
    FAnsiMouseReported: Boolean;
    procedure AnsiMouseReport(ACmdBox: TCmdBox; const AReport: string);
    function GetTextInRange(Lines: TStringList; StartRow, StartCol, EndRow, EndCol: Integer): string;
  protected
    procedure CMWantSpecialKey(var Message: TCMWantSpecialKey);
      message CM_WANTSPECIALKEY;
    procedure WMGetDlgCode(var Message: TLMGetDlgCode); message LM_GETDLGCODE;
    procedure UTF8KeyPress(var Key: TUTF8Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
      MousePos: TPoint): Boolean; override;
    procedure Paint; override;
  public
    procedure Write(S: String);
    constructor Create(AOwner: TComponent); override;
    property OnTerminalInput: TTerminalInputEvent read FOnTerminalInput
      write FOnTerminalInput;
  end;

function TerminalKeySequence(Key: Word; Shift: TShiftState): RawByteString;
procedure Register;

implementation

function TerminalKeySequence(Key: Word; Shift: TShiftState): RawByteString;
var
  Letter: AnsiChar;
begin
  Result := '';

  if (ssAlt in Shift) and (Key >= Ord('A')) and (Key <= Ord('Z')) then
  begin
    Letter := AnsiChar(Key);
    if not (ssShift in Shift) then
      Letter := AnsiChar(Ord(Letter) + Ord('a') - Ord('A'));
    Exit(#27 + Letter);
  end;

  if (ssCtrl in Shift) and (Key >= Ord('A')) and (Key <= Ord('Z')) then
    Exit(AnsiChar(Key - Ord('A') + 1));

  case Key of
    VK_BACK: Result := #8;
    VK_TAB: Result := #9;
    VK_RETURN: Result := #13;
    VK_ESCAPE: Result := #27;
    VK_UP: Result := #27'[A';
    VK_DOWN: Result := #27'[B';
    VK_RIGHT: Result := #27'[C';
    VK_LEFT: Result := #27'[D';
    VK_HOME: Result := #27'[H';
    VK_END: Result := #27'[F';
    VK_INSERT: Result := #27'[2~';
    VK_DELETE: Result := #27'[3~';
    VK_PRIOR: Result := #27'[5~';
    VK_NEXT: Result := #27'[6~';
    VK_F1: Result := #27'OP';
    VK_F2: Result := #27'OQ';
    VK_F3: Result := #27'OR';
    VK_F4: Result := #27'OS';
    VK_F5: Result := #27'[15~';
    VK_F6: Result := #27'[17~';
    VK_F7: Result := #27'[18~';
    VK_F8: Result := #27'[19~';
    VK_F9: Result := #27'[20~';
    VK_F10: Result := #27'[21~';
    VK_F11: Result := #27'[23~';
    VK_F12: Result := #27'[24~';
  end;
end;

procedure Register;
begin
  RegisterComponents('Custom', [TCmdBoxCustom]);
end;

{ TCmdBoxCustom }

constructor TCmdBoxCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TabStop := True;
  OnAnsiMouseReport := @AnsiMouseReport;
  StringBuffer := TStringList.Create;
  VerticalScrollbarVisible := True;
end;

procedure TCmdBoxCustom.CMWantSpecialKey(var Message: TCMWantSpecialKey);
begin
  Message.Result := 1;
end;

procedure TCmdBoxCustom.WMGetDlgCode(var Message: TLMGetDlgCode);
begin
  inherited;
  Message.Result := Message.Result or DLGC_WANTARROWS or DLGC_WANTTAB or
    DLGC_WANTALLKEYS or DLGC_WANTCHARS;
end;

procedure TCmdBoxCustom.UTF8KeyPress(var Key: TUTF8Char);
begin
  if (Key <> '') and Assigned(FOnTerminalInput) then
    FOnTerminalInput(Self, RawByteString(Key));
  Key := '';
end;

procedure TCmdBoxCustom.KeyDown(var Key: Word; Shift: TShiftState);
var
  Data: RawByteString;
begin
  Data := TerminalKeySequence(Key, Shift);
  if Data <> '' then
  begin
    if Assigned(FOnTerminalInput) then
      FOnTerminalInput(Self, Data);
    Key := 0;
    Exit;
  end;
  inherited KeyDown(Key, Shift);
end;

procedure TCmdBoxCustom.AnsiMouseReport(ACmdBox: TCmdBox;
  const AReport: string);
begin
  FAnsiMouseReported := True;
  if Assigned(FOnTerminalInput) then
    FOnTerminalInput(Self, RawByteString(AReport));
end;

function TCmdBoxCustom.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer;
  MousePos: TPoint): Boolean;
begin
  FAnsiMouseReported := False;
  Result := inherited DoMouseWheel(Shift, WheelDelta, MousePos);
  if FAnsiMouseReported then
    Exit(True);

  if WheelDelta > 0 then
    if Self.TopLine = 0 then
      Self.TopLine := 0
    else
      Self.TopLine := Self.TopLine - 1
  else
    Self.TopLine := Self.TopLine + 1;
  VerticalScrollbarVisible := True;
  Result := True;
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

procedure TCmdBoxCustom.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var CharHeight: Integer;
begin
  FAnsiMouseReported := False;
  inherited MouseDown(Button, Shift, X, Y);
  if FAnsiMouseReported then
  begin
    SelectActive := False;
    Exit;
  end;
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
  FAnsiMouseReported := False;
  inherited MouseMove(Shift, X, Y);
  if FAnsiMouseReported then
    Exit;
  if SelectActive then
  begin
    CharHeight := abs(Font.Height)+3;
    SelectEndRow := Y div CharHeight;
    SelectEndCol := X div GraphicalCharacterWidth;
  end;
  Invalidate;
end;

procedure TCmdBoxCustom.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FAnsiMouseReported := False;
  inherited MouseUp(Button, Shift, X, Y);
  if FAnsiMouseReported then
    Exit;
  if (Button = mbLeft) and SelectActive then
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

