unit umycallsign;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs,
  Buttons, ExtCtrls, ButtonPanel, utypes, uini;

type

  { TTFMyCallsign }

  PTFPConfig = ^TFPConfig;

  TTFMyCallsign = class(TForm)
    BPDefaultButtons: TButtonPanel;
    ECallsign: TLabeledEdit;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
  private

  public

  end;

var
  TFMyCallsign: TTFMyCallsign;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TTFMyCallsign }

procedure TTFMyCallsign.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  ECallSign.Text := FPConfig^.Callsign;
end;


procedure TTFMyCallsign.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFMyCallsign.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.Callsign := ECallsign.Text;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;

end.

