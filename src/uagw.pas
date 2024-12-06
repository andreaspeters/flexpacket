unit uagw;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ExtCtrls, utypes, uini;

type

  { TFAGW }

  PTFPConfig = ^TFPConfig;

  TFAGW = class(TForm)
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LEServerPassword: TLabeledEdit;
    LEServerUsername: TLabeledEdit;
    LEServerPort: TLabeledEdit;
    LEServerIP: TLabeledEdit;
    procedure CancelButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  FAGW: TFAGW;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TFAGW }


procedure TFAGW.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;

  LEServerIP.Text := FPConfig^.AGWServer;
  LEServerPort.Text := IntToStr(FPConfig^.AGWServerPort);
  LEServerUsername.Text := FPConfig^.AGWServerUsername;
  LEServerPassword.Text := FPConfig^.AGWServerPassword;
end;

procedure TFAGW.CancelButtonClick(Sender: TObject);
begin
  close;
end;

procedure TFAGW.OKButtonClick(Sender: TObject);
begin
  FPConfig^.AGWServer := LEServerIP.Text;
  FPConfig^.AGWServerPort := StrToInt(LEServerPort.Text);
  FPConfig^.AGWServerUsername := LEServerUsername.Text;
  FPConfig^.AGWServerPassword := LEServerPassword.Text;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;

end.

