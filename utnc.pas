unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, ax25helper;

type

  { TTFTNC }

  PTAX25Config = ^TAX25Config;

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
  private

  public

  end;

var
  TFTNC: TTFTNC;
  AX25Config: PTAX25Config;

implementation

{$R *.lfm}

{ TTFTNC }

procedure TTFTNC.SetConfig(Config: PTAX25Config);
begin
  AX25Config := Config;
  EComPort.Text := AX25Config^.Com.Port;
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


end.

