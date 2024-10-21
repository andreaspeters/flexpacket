unit uchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  SynEdit, SynPluginSyncroEdit;

type

  { TFChannel }

  TFChannel = class(TForm)
    MMChannel: TMainMenu;
    MRx: TMemo;
    MTx: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure SendMessage(Sender: TObject);
  private

  public

  end;

var
  FChannel: TFChannel;

implementation

{$R *.lfm}

{ TFChannel }

procedure TFChannel.SendMessage(Sender: TObject);
begin

end;

procedure TFChannel.FormCreate(Sender: TObject);
begin

end;

end.

