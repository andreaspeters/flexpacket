unit umycallsign;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ax25helper;

type

  { TTFMyCallsign }

  PTAX25Config = ^TAX25Config;

  TTFMyCallsign = class(TForm)
    BtnSave: TButton;
    BtnCancel: TButton;
    ECallsign: TEdit;
    GroupBox1: TGroupBox;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTAX25Config);
  private

  public

  end;

var
  TFMyCallsign: TTFMyCallsign;
  AX25Config: PTAX25Config;

implementation

{$R *.lfm}

{ TTFMyCallsign }

procedure TTFMyCallsign.SetConfig(Config: PTAX25Config);
begin
  AX25Config := Config;
  ECallSign.Text := AX25Config^.Callsign;
end;


procedure TTFMyCallsign.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFMyCallsign.BtnSaveClick(Sender: TObject);
begin
  AX25Config^.Callsign := ECallsign.Text;
  Close;
end;

end.

