unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TTFTNC }

  TTFTNC = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    EComPort: TEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    RGComSpeed: TRadioGroup;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private

  public

  end;

var
  TFTNC: TTFTNC;

implementation

{$R *.lfm}

{ TTFTNC }


procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  Close;
end;

end.

