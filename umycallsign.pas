unit umycallsign;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, utypes;

type

  { TTFMyCallsign }

  PTFPConfig = ^TFPConfig;

  TTFMyCallsign = class(TForm)
    BtnSave: TButton;
    BtnCancel: TButton;
    ECallsign: TEdit;
    GroupBox1: TGroupBox;
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
  Close;
end;

end.

