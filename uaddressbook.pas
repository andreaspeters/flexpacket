unit uaddressbook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  ValEdit, ExtCtrls;

type

  { TTFAdressbook }

  TTFAdressbook = class(TForm)
    ButtonPanel1: TButtonPanel;
    GroupBox1: TGroupBox;
    LEConnectVia: TLabeledEdit;
    LECallSign: TLabeledEdit;
    LBCallsignes: TListBox;
    Panel1: TPanel;
    procedure ButtonPanel1Click(Sender: TObject);
  private

  public

  end;

var
  TFAdressbook: TTFAdressbook;

implementation

{$R *.lfm}

{ TTFAdressbook }

procedure TTFAdressbook.ButtonPanel1Click(Sender: TObject);
begin
  Close;
end;

end.

