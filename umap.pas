unit umap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BCSVGViewer, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Contnrs, uresize, utypes;

type

  { TTFMap }

  TTFMap = class(TForm)
    BCSVGMap: TBCSVGViewer;
    SBMap: TStatusBar;
    TMAPUpdate: TTimer;
    procedure BCSVGMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure ResizeForm(Sender: TObject);
    procedure TMAPUpdateTimer(Sender: TObject);
  private

  public
    APRSMessageList: TFPHashList;
    procedure DrawTextOnCanvas(const Message: String);
    function ConvertToDecimalDegrees(const Deg, Min: string; Direction: char): Double;
  end;

var
  TFMap: TTFMap;
  OrigWidth, OrigHeight: Integer;

implementation

{$R *.lfm}

{ TTFMap }

procedure TTFMap.FormShow(Sender: TObject);
begin
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  APRSMessageList := TFPHashList.Create;

  BCSVGMap.LoadFromFile('assets/images/world.svg');

  TMAPUpdate.Enabled := True;
end;

procedure TTFMap.BCSVGMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  OffsetY, SVGX, SVGY: Double;
  Longitude, Latitude: Double;
  ViewBoxX, ViewBoxY, ViewBoxWidth, ViewBoxHeight: Double;
  ControlWidth, ControlHeight: Double;
begin
  // Beispielwerte der ViewBox
  ViewBoxX := 0;         // Linke obere Ecke der ViewBox (x)
  ViewBoxY := 0;         // Linke obere Ecke der ViewBox (y)
  ViewBoxWidth := 360;   // Breite der ViewBox
  ViewBoxHeight := 180;  // Höhe der ViewBox

  // Maße des SVG-Viewers (Anzeigebereich in Pixel)
  ControlWidth := BCSVGMap.Width;
  ControlHeight := BCSVGMap.Height;

  // Umrechnung der Pixelkoordinaten (X, Y) auf SVG-Koordinaten
  SVGX := ViewBoxX + (X / ControlWidth) * ViewBoxWidth;
  SVGY := ViewBoxY + (Y / ControlHeight) * ViewBoxHeight;

  OffsetY := 5.0;

  // Umrechnung SVG-Koordinaten auf geografische Koordinaten
  Longitude := (SVGX / ViewBoxWidth) * 360.0 - 180.0;  // Longitude: -180° bis +180°
  Latitude := 90.0 - (SVGY / ViewBoxHeight) * 180.0 + OffsetY; // Latitude: +90° bis -90°
  SBMap.Panels[0].Text := Format('Mouse Position: X = %.2f, Y = %.2f', [Longitude, Latitude]);
end;

procedure TTFMap.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TMAPUpdate.Enabled := False;
end;

procedure TTFMap.ResizeForm(Sender: TObject);
var
  scaleFactorWidth, scaleFactorHeight, scaleFactor: Double;
  i: Integer;
begin
  scaleFactorWidth := Width / OrigWidth;
  scaleFactorHeight := Height / OrigHeight;
  scaleFactor := Min(scaleFactorWidth, scaleFactorHeight);

  for i := 0 to ControlCount - 1 do
    ResizeControl(Controls[i], scaleFactorWidth, scaleFactorHeight, scaleFactor);
end;

procedure TTFMap.TMAPUpdateTimer(Sender: TObject);
var i: Integer;
    msg: PAPRSMessage;
begin
  if APRSMessageList <> nil then
  begin
    for i := 0 to APRSMessageList.Count - 1 do
    begin
      msg := PAPRSMessage(APRSMessageList.Items[i]);
      Writeln(msg^.FromCall);
    end;
  end;
//  for i := 0 to APRSMessageList.Count - 1 do
//  begin
//    RetrievedMessage := TAPRSMessage(APRSMessageList[i]);
//  end;
end;

procedure TTFMap.DrawTextOnCanvas(const Message: String);
var X, Y: Integer;
begin
  X := 10;
  Y := 10;

  with BCSVGMap.Canvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 16;
    Font.Color := clBlack;
    TextOut(X, Y, Message);
  end;
end;

function TTFMap.ConvertToDecimalDegrees(const Deg, Min: string; Direction: char): Double;
var
  Degrees, Minutes: Double;
begin
  Degrees := StrToFloat(Deg);
  Minutes := StrToFloat(Min);
  Result := Degrees + (Minutes / 60.0);
  if Direction in ['S', 'W'] then
    Result := -Result;
end;

end.

