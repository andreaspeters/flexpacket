unit umycallsign;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TTFMyCallsign }

  TTFMyCallsign = class(TForm)
    BtnSave: TButton;
    BtnCancel: TButton;
    ECallsign: TEdit;
    GroupBox1: TGroupBox;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure GroupBox1Click(Sender: TObject);
  private

  public

  end;

var
  TFMyCallsign: TTFMyCallsign;

implementation

{$R *.lfm}

{ TTFMyCallsign }

procedure TTFMyCallsign.GroupBox1Click(Sender: TObject);
begin

end;

procedure TTFMyCallsign.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFMyCallsign.BtnSaveClick(Sender: TObject);
begin
  Close;
end;

end.

