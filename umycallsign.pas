unit umycallsign;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons;

type

  { TForm2 }

  TForm2 = class(TForm)
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
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.GroupBox1Click(Sender: TObject);
begin

end;

procedure TForm2.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.BtnSaveClick(Sender: TObject);
begin
  Close;
end;

end.

