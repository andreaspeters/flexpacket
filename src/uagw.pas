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
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  FAGW: TFAGW;
  FPConfig: PTFPConfig;
  OldWidth, OldHeight: Integer;

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

procedure TFAGW.FormCreate(Sender: TObject);
begin
  OldWidth := Width;
  OldHeight := Height;
end;

procedure TFAGW.FormShow(Sender: TObject);
begin
  // fix for wayland
  Height := OldHeight;
  Width := OldWidth;
end;

procedure TFAGW.OKButtonClick(Sender: TObject);
var
  ServerPort: Integer;
begin
  if not TryStrToInt(LEServerPort.Text, ServerPort) or
    (ServerPort < 1) or (ServerPort > 65535) then
  begin
    MessageDlg('Please enter a valid AGW server port.', mtError, [mbOK], 0);
    Exit;
  end;

  BeginConfigurationChange;
  FPConfig^.AGWServer := LEServerIP.Text;
  FPConfig^.AGWServerPort := ServerPort;
  FPConfig^.AGWServerUsername := LEServerUsername.Text;
  FPConfig^.AGWServerPassword := LEServerPassword.Text;
  ApplyConfiguration;
  Close;
end;

end.

