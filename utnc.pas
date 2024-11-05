unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, utypes, ExtCtrls, ButtonPanel, Spin;

type

  { TTFTNC }

  PTFPConfig = ^TFPConfig;

  TTFTNC = class(TForm)
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    EComPort: TLabeledEdit;
    Label1: TLabel;
    RGComSpeed: TRadioGroup;
    SPMaxChannels: TSpinEdit;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
  private
  public
  end;

var
  TFTNC: TTFTNC;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TTFTNC }

procedure TTFTNC.SetConfig(Config: PTFPConfig);
var i: Byte;
begin
  FPConfig := Config;
  EComPort.Text := FPConfig^.ComPort;
  SPMaxChannels.Value := FPConfig^.MaxChannels;

  for i := 0 to RGComSpeed.Items.Count - 1 do
  begin
    if RGComSpeed.Items[i] = IntToStr(FPConfig^.ComSpeed) then
      RGComSpeed.ItemIndex := i;
  end;
end;

procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.ComPort := EComPort.Text;
  FPConfig^.ComSpeed := StrToInt(RGComSpeed.Items[RGComSpeed.ItemIndex]);
  FPConfig^.MaxChannels := SPMaxChannels.Value;
  Close;
end;


end.

