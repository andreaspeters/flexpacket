unit uansi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Graphics,
  StrUtils, RichMemo;


type
  TGraphicSegment = record
    Text: string;
    Color: TColor;
    TextFrom: Integer;
    TextLength: Integer;
  end;

  TGraphicArray = array of TGraphicSegment;
  function ApplyANSIColor(Text: string; MainColor: TColor): TGraphicArray;
  procedure DisplayANSITextInMemo(Memo: TRichMemo; Segments: TGraphicArray);


implementation

function ApplyANSIColor(Text: string; MainColor: TColor): TGraphicArray;
var
  StartPos, EndPos, ANSILen: Integer;
  CurrentColor: TColor;
  Segments: TGraphicArray;
  Segment: TGraphicSegment;
begin
  SetLength(Segments, 0);
  StartPos := 1;
  ANSILen := 0;
  CurrentColor := MainColor;

  while StartPos <= Length(Text) do
  begin
    if Text[StartPos] = #27 then
    begin
      case Copy(Text, StartPos, 5) of
        #27'[30m': // Schwarz
        begin
          CurrentColor := clBlack;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[31m': // Rot
        begin
          CurrentColor := clRed;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[32m': // Grün
        begin
          CurrentColor := clGreen;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[33m': // Gelb
        begin
          CurrentColor := clOlive;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[34m': // Blau
        begin
          CurrentColor := clBlue;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[35m': // Magenta
        begin
          CurrentColor := clFuchsia;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[36m': // Cyan
        begin
          CurrentColor := clAqua;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[37m': // Weiß
        begin
          CurrentColor := clWhite;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[90m': // Helles Schwarz (Grau)
        begin
          CurrentColor := clGray;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[91m': // Helles Rot
        begin
          CurrentColor := clRed;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[92m': // Helles Grün
        begin
          CurrentColor := clLime;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[93m': // Helles Gelb
        begin
          CurrentColor := clYellow;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[94m': // Helles Blau
        begin
          CurrentColor := clSkyBlue;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[95m': // Helles Magenta
        begin
          CurrentColor := clFuchsia;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[96m': // Helles Cyan
        begin
          CurrentColor := clAqua;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[97m': // Helles Weiß (fast Weiß)
        begin
          CurrentColor := clSilver;
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[39m': // NULL, this one is doing nothing. but we need it.
        begin
          ANSILen := 5;
          Inc(StartPos, ANSILen);
        end;
        #27'[0m':  // Reset
        begin
          CurrentColor := MainColor;
          ANSILen := 4;
          Inc(StartPos, ANSILen);
        end;
      else
        Inc(StartPos);  // Default-Fall, wenn der Escape-Code nicht erkannt wird
      end;
    end
    else
    begin
      EndPos := PosEx(#27'[', Text, StartPos);  // Sucht nach dem nächsten Escape Character

      if EndPos = 0 then
        EndPos := Length(Text) + 1;  // Wenn kein Escape Character mehr gefunden wird

      Segment.Text := Copy(Text, StartPos, EndPos - StartPos);
      Segment.Color := CurrentColor;
      Segment.TextFrom := StartPos - ANSILen - 1;
      Segment.TextLength := EndPos - StartPos;

      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Segment;

      StartPos := EndPos;
    end;
  end;

  Result := Segments;
end;


procedure DisplayANSITextInMemo(Memo: TRichMemo; Segments: TGraphicArray);
var
  i, Len, max, from: Integer;
  curColor: TColor;
begin
  curColor := Memo.Font.Color;
  max := High(Segments);
  if Assigned(Memo) then
  begin
    for i := 0 to max do
    begin
      Len := Memo.GetTextLen;
      if (Memo.Lines.Count > 0) and (Memo.Lines[Memo.Lines.Count - 1] <> '') then
      begin
        // if in the prev line a CR exist, add text in a new line
        if (Segments[i].Text <> '') and (Length(Segments[i].Text) > 0) and (Segments[i].Text[Length(Segments[i].Text)] = #13) then
        begin
          Memo.Lines.Add(Segments[i].Text);
        end
        else
        begin
          Memo.Lines[Memo.Lines.Count - 1] := Memo.Lines[Memo.Lines.Count - 1] + Segments[i].Text;
        end;
      end
      else
      begin
        Memo.Lines.Add(Segments[i].Text);
      end;

      from := Segments[i].TextFrom + Len;
      if from < 0 then
        from := 0;

      //writeln(from);
      //writeln(Segments[i].TextLength + 3);
      //writeln(Segments[i].Color);
      Memo.SetRangeColor(from, Segments[i].TextLength + 4, Segments[i].Color);
    end;
    Memo.Font.Color := curColor;
  end;
end;


end.

