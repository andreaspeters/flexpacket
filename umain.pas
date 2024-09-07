unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, uchannel;

type

  { TFMain }

  TFMain = class(TForm)
    BBChannelOne: TBitBtn;
    BBChannelTwo: TBitBtn;
    BBChannelThree: TBitBtn;
    BBChannelFour: TBitBtn;
    BMonitor: TButton;
    BEditor: TButton;
    MainMenuItemFile: TMenuItem;
    MainMenuItemWindow: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MainMenuItemInfo: TMenuItem;
    MMainMenu: TMainMenu;
    procedure BBChannelOneClick(Sender: TObject);
  private

  public

  end;

var
  FMain: TFMain;
  FChannel: TFChannel;


implementation

{$R *.lfm}

{ TFMain }


procedure TFMain.BBChannelOneClick(Sender: TObject);
begin
  FChannel := TFChannel.Create(Self);
  FChannel.Show;
end;


end.

