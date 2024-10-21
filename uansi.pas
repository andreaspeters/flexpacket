unit uansi;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Dialogs, StdCtrls, ExtCtrls, Graphics, RichMemo;




type
  TGraphicSegment = record
    Text: string;
    Color: TColor;
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
      if Copy(Text, StartPos, 5) = #27'[31m' then
      begin
        CurrentColor := clRed;
        Inc(StartPos, 5);
      end
      else if Copy(Text, StartPos, 5) = #27'[32m' then
      begin
        CurrentColor := clGreen;
        Inc(StartPos, 5);
      end
      else if Copy(Text, StartPos, 5) = #27'[0m' then
      begin
        CurrentColor := clBlack;
        Inc(StartPos, 5);
      end;
    end
    else
    begin
      EndPos := StartPos;
      while (EndPos <= Length(Text)) and (Text[EndPos] <> #27) do
        Inc(EndPos);

      Segment.Text := Copy(Text, StartPos, EndPos - StartPos);
      Segment.Color := CurrentColor;
      SetLength(Segments, Length(Segments) + 1);
      Segments[High(Segments)] := Segment;

      StartPos := EndPos;
    end;
  end;

  Result := Segments;
end;


procedure DisplayANSITextInMemo(Memo: TRichMemo; Segments: TGraphicArray);
var
  I: Integer;
begin
  for I := 0 to High(Segments) do
  begin
    Memo.Font.Color := Segments[i].Color;  // Setze die Schriftfarbe#
    Memo.Lines.Add(Segments[i].Text);  // Füge den Text hinzu
  end;
  Memo.Font.Color := clBlack;
end;



end.

