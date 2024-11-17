unit ufileupload;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ValEdit, StdCtrls,
  ButtonPanel, ExtCtrls, uresize;

type

  { TFFileUpload }

  TFFileUpload = class(TForm)
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    MFileView: TMemo;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    procedure SetFilename(FName: String);
  end;

var
  FFileUpload: TFFileUpload;
  FileName: String;
  OrigWidth, OrigHeight: Integer;

implementation

{$R *.lfm}

{ TFFileUpload }

procedure TFFileUpload.SetFilename(FName: String);
begin
  FileName := FName;
end;

procedure TFFileUpload.CancelButtonClick(Sender: TObject);
begin
  Close;
end;


procedure TFFileUpload.FormResize(Sender: TObject);
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

procedure TFFileUpload.FormShow(Sender: TObject);
begin
  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  MFileView.Lines.Add(FileName);
end;

end.

