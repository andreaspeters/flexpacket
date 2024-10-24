unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  utypes, uhostmode;

type

  { TTFTNC }

  PTFPConfig = ^TFPConfig;
  PTHostmode = ^THostmode;

  TTFTNC = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    EComPort: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    RGComSpeed: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
    procedure SetHelper(Helper: PTHostmode);
    procedure InitTNC;
  private
  public
  end;

var
  TFTNC: TTFTNC;
  FPConfig: PTFPConfig;
  Hostmode: PTHostmode;

implementation

{$R *.lfm}

{ TTFTNC }

procedure TTFTNC.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  EComPort.Text := FPConfig^.Com.Port;
end;

procedure TTFTNC.SetHelper(Helper: PTHostmode);
begin
  Hostmode := Helper;
end;


procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.Com.Port := EComPort.Text;
  Close;
end;

procedure TTFTNC.InitTNC;
begin
//  AX25^.SendCommand('S '+ IntToStr(i));
//  AX25^.SendCommand('I '+ Callsign)
end;

end.

