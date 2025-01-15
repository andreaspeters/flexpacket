unit ukiss;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, ButtonPanel, utypes, uini;

type

  { TFKiss }

  PTFPConfig = ^TFPConfig;

  TFKiss = class(TForm)
    BPDefaultButtons: TButtonPanel;
    ECallsign: TLabeledEdit;
    LESocketPath: TLabeledEdit;
    ODSelectFile: TOpenDialog;
    SpeedButton1: TSpeedButton;
    procedure BBSocketPathClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
  private

  public

  end;

var
  FKiss: TFKiss;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TFKiss }

procedure TFKiss.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  LESocketPath.Text := FPConfig^.KISSPipe;
end;


procedure TFKiss.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFKiss.BBSocketPathClick(Sender: TObject);
begin
  if ODSelectFile.Execute then
    LESocketPath.Text := ODSelectFile.FileName;
end;

procedure TFKiss.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.KISSPipe := LESocketPath.Text;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;


end.

