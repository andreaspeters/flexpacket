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
  function ApplyANSIColor(Text: string): TGraphicArray;
  procedure DisplayANSITextInMemo(Memo: TRichMemo; Segments: TGraphicArray);


implementation

function ApplyANSIColor(Text: string): TGraphicArray;
var
  StartPos, EndPos: Integer;
  CurrentColor: TColor;
  Segments: TGraphicArray;
  Segment: TGraphicSegment;
begin
  SetLength(Segments, 0);
  StartPos := 1;
  CurrentColor := clBlack;


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
        #27'[0m':  // Reset
        begin
          CurrentColor := clBlack;
          Inc(StartPos, 5);
        end;
      else  // Default-Fall, wenn der Escape-Code nicht erkannt wird
        Inc(StartPos);
      end;
    end
    else
    begin
      EndPos := PosEx(#27, Text, StartPos);  // Sucht nach dem nächsten Escape Character
      if EndPos = 0 then
        EndPos := Length(Text) + 1;  // Wenn kein Escape Character mehr gefunden wird

      Segment.Text := Copy(Text, StartPos, EndPos - StartPos);
      Segment.Color := CurrentColor;
      Segment.TextFrom := StartPos - 6;
      Segment.TextLength := EndPos - StartPos + 1;
      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Segment;

      StartPos := EndPos;
    end;
  end;


  Result := Segments;
end;


procedure DisplayANSITextInMemo(Memo: TRichMemo; Segments: TGraphicArray);
var
  I, Len: Integer;
begin
  for I := 0 to High(Segments) do
  begin
    Len := Memo.GetTextLen;
    Memo.Lines.Add(Segments[i].Text);  // Füge den Text hinzu
    Memo.SetRangeColor(Segments[i].TextFrom + Len, Segments[i].TextLength, Segments[i].Color);
  end;
  Memo.Font.Color := clBlack;
end;



end.

