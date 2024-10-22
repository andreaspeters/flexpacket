unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ax25helper;

type

  { TTFTNC }

  PTAX25Config = ^TAX25Config;
  PTAX25Helper = ^TAX25Helper;

  TTFTNC = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    EComPort: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    RGComSpeed: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTAX25Config);
    procedure SetHelper(Helper: PTAX25Helper);
    procedure InitTNC;
  private
  public
  end;

var
  TFTNC: TTFTNC;
  AX25Config: PTAX25Config;
  AX25: PTAX25Helper;

implementation

{$R *.lfm}

{ TTFTNC }

procedure TTFTNC.SetConfig(Config: PTAX25Config);
begin
  AX25Config := Config;
  EComPort.Text := AX25Config^.Com.Port;
end;

procedure TTFTNC.SetHelper(Helper: PTAX25Helper);
begin
  AX25 := Helper;
end;


procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  AX25Config^.Com.Port := EComPort.Text;
  Close;
end;

procedure TTFTNC.InitTNC;
begin
//  AX25^.SendCommand('S '+ IntToStr(i));
//  AX25^.SendCommand('I '+ Callsign)
end;

end.

