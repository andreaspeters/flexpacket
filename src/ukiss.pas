unit ukiss;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, ButtonPanel, StdCtrls, Spin, utypes, uini;

type

  { TFKiss }

  PTFPConfig = ^TFPConfig;

  TFKiss = class(TForm)
    BPDefaultButtons: TButtonPanel;
    ECallsign: TLabeledEdit;
    Label1: TLabel;
    LESocketPath: TLabeledEdit;
    ODSelectFile: TOpenDialog;
    SpeedButton1: TSpeedButton;
    SPMaxChannels: TSpinEdit;
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
  SPMaxChannels.Value := FPConfig^.MaxChannels;
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
  FPConfig^.MaxChannels := SPMaxChannels.Value;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;


end.

