unit uinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel;

type

  { TTFInfo }

  TTFInfo = class(TForm)
    BPDefaultButtons: TButtonPanel;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Memo1: TMemo;
    procedure CloseInfo(Sender: TObject);
  private

  public

  end;

var
  TFInfo: TTFInfo;

implementation

{$R *.lfm}

{ TTFInfo }

procedure TTFInfo.CloseInfo(Sender: TObject);
begin
  Close;
end;

end.

