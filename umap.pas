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
    procedure GeoToSVGCoordinates(const Latitude, Longitude: Double; out X, Y: Integer);
    procedure DrawTextOnCanvas(const Message: String; const X, Y: Integer);
  public
    APRSMessageList: TFPHashList;
    function ConvertToDecimalDegrees(const Deg, Min: string; Direction: char): Double;
  end;

var
  TFMap: TTFMap;
  OrigWidth, OrigHeight: Integer;
  ViewBoxWidth, ViewBoxHeight: Double;

implementation

{$R *.lfm}

{ TTFMap }

procedure TTFMap.FormShow(Sender: TObject);
begin
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  ViewBoxWidth := 360;
  ViewBoxHeight := 180;

  APRSMessageList := TFPHashList.Create;

  BCSVGMap.LoadFromFile('assets/images/world.svg');
  BCSVGMap.Width := 360;
  BCSVGMap.Height := 180;

  TMAPUpdate.Enabled := True;
end;

{
  BCSVGMapMouseMove

  Convert the Mouse position into Latitude and Longitude.
}
procedure TTFMap.BCSVGMapMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  OffsetY, SVGX, SVGY: Double;
  Longitude, Latitude: Double;
  ViewBoxX, ViewBoxY: Double;
begin
  // Beispielwerte der ViewBox
  ViewBoxX := 0;         // Linke obere Ecke der ViewBox (x)
  ViewBoxY := 0;         // Linke obere Ecke der ViewBox (y)

  // Umrechnung der Pixelkoordinaten (X, Y) auf SVG-Koordinaten
  SVGX := ViewBoxX + (X / BCSVGMap.Width) * ViewBoxWidth;
  SVGY := ViewBoxY + (Y / BCSVGMap.Height) * ViewBoxHeight;

  OffsetY := 5.0;

  // Umrechnung SVG-Koordinaten auf geografische Koordinaten
  Longitude := (ViewBoxX + (X / BCSVGMap.Width) * ViewBoxWidth / ViewBoxWidth) * 360.0 - 180.0;  // Longitude: -180° bis +180°
  Latitude := 90.0 - (ViewBoxY + (Y / BCSVGMap.Height) * ViewBoxHeight / ViewBoxHeight) * 180.0 + OffsetY; // Latitude: +90° bis -90°
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

{
  TMAPUpdateTimer

  This procedure will update the map.
}
procedure TTFMap.TMAPUpdateTimer(Sender: TObject);
var i, x, y: Integer;
    msg: PAPRSMessage;
begin
  if APRSMessageList <> nil then
  begin
    for i := 0 to APRSMessageList.Count - 1 do
    begin
      msg := PAPRSMessage(APRSMessageList.Items[i]);
      GeoToSVGCoordinates(msg^.Latitude, msg^.Longitude, x, y);
      Writeln(FloatToStr(msg^.Longitude));
      Writeln(FloatToStr(msg^.Latitude));

      DrawTextOnCanvas(msg^.FromCall, x, y);
    end;
  end;
end;

procedure TTFMap.DrawTextOnCanvas(const Message: String; const x, y: Integer);
begin
  with BCSVGMap.Canvas do
  begin
    Font.Name := 'Arial';
    Font.Size := 16;
    Font.Color := clBlack;
    TextOut(Trunc(X), Trunc(Y), Message);
  end;
end;

{
  ConvertToDecimalDegrees

  Convert String "Deg" (Degree), "Min" (Minute) and the "Directon" (S,W) into
  Double therefore we can use it later.
}
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

{
  GeoToSVGCoordinates

  Convert NMEA "Latitude" and "Longitude" into X, Y Coordinates for the SVGViewer.
}
procedure TTFMap.GeoToSVGCoordinates(const Latitude, Longitude: Double; out X, Y: Integer);
var DecLat, DecLon: Double;
    ViewBoxX, ViewBoxY: Double;
begin
  ViewBoxX := 0;
  ViewBoxY := 0;

  DecLon := (Trunc((Longitude / 10) / 100) + ((Longitude / 10) - Trunc((Longitude / 10) / 100) * 100) / 60);
  DecLat := Trunc(Latitude / 100) + (Frac(Latitude / 100) * 100) / 60;

  X := Trunc((((DecLon + 180.0) / 360.0) - ViewBoxX) * BCSVGMap.Width);
  Y := Trunc((((-(DecLat - 90.0) / 180.0) - ViewBoxY) * BCSVGMap.Height));
end;


end.

