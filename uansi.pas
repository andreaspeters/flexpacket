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
  StartPos, EndPos: Integer;
  CurrentColor: TColor;
  Segments: TGraphicArray;
  Segment: TGraphicSegment;
begin
  SetLength(Segments, 0);
  StartPos := 1;
  CurrentColor := MainColor;

  while StartPos <= Length(Text) do
  begin
    if Text[StartPos] = #27 then
    begin
      case Copy(Text, StartPos, 5) of
        #27'[31m': // Rot
        begin
          CurrentColor := clRed;
          Inc(StartPos, 5);
        end;
        #27'[32m': // Grün
        begin
          CurrentColor := clGreen;
          Inc(StartPos, 5);
        end;
        #27'[34m': // Blau
        begin
          CurrentColor := clBlue;
          Inc(StartPos, 5);
        end;
        #27'[96m': // Bright Cyan
        begin
          CurrentColor := clAqua;
          Inc(StartPos, 5);
        end;
        #27'[0m':  // Reset
        begin
          CurrentColor := MainColor;
          Inc(StartPos, 4);
        end;
      else  // Default-Fall, wenn der Escape-Code nicht erkannt wird
        Inc(StartPos);
      end;
    end
    else
    begin
      EndPos := PosEx(#27'[', Text, StartPos);  // Sucht nach dem nächsten Escape Character
      if EndPos = 0 then
        EndPos := Length(Text);  // Wenn kein Escape Character mehr gefunden wird

      Segment.Text := Copy(Text, StartPos, EndPos - StartPos);
      Segment.Color := CurrentColor;
      Segment.TextFrom := StartPos - 6;
      if Segment.TextFrom < 0 then
        Segment.TextFrom := 0;
      Segment.TextLength := EndPos - StartPos + 1;
      if Segment.TextLength < 0 then
        Segment.TextLength := 0;
      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Segment;

      StartPos := EndPos;
    end;
  end;

  if Length(Segments) > 0 then
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
        if (Segments[i].Text <> '') and (Segments[i].Text[Length(Segments[i].Text)] = #13) then
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
      if from > 1 then
        Dec(from);

      Memo.SetRangeColor(from, Segments[i].TextLength+1, Segments[i].Color);
    end;
    Memo.Font.Color := curColor;
  end;
end;



end.

