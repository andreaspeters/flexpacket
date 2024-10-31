unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, utypes, ExtCtrls;

type

  { TTFTNC }

  PTFPConfig = ^TFPConfig;

  TTFTNC = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    GroupBox1: TGroupBox;
    EComPort: TLabeledEdit;
    RGComSpeed: TRadioGroup;
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
begin
  FPConfig := Config;
  EComPort.Text := FPConfig^.ComPort;
end;

procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.ComPort := EComPort.Text;
  Close;
end;


end.

