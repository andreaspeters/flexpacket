unit umap;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BCSVGViewer, Forms, Controls, Graphics, Dialogs, uresize;

type

  { TTFMap }

  TTFMap = class(TForm)
    BCSVGViewer1: TBCSVGViewer;
    procedure FormShow(Sender: TObject);
    procedure ResizeForm(Sender: TObject);
  private

  public

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

  BCSVGViewer1.LoadFromFile('assets/images/world.svg');
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



end.

