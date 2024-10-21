unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, uchannel, AX25Helper, BaseUnix, sockets;

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
    procedure FMainInit(Sender: TObject);
    procedure StartPacketReceiving();
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
  StartPacketReceiving;
end;

procedure TFMain.FMainInit(Sender: TObject);
begin

end;

procedure TFMain.StartPacketReceiving();
var
  ReceiverThread: TFAX25Helper;
begin
  ReceiverThread := TFAX25Helper.Create(UChannel.FChannel.MRx);
  ReceiverThread.FreeOnTerminate := True; // Thread automatisch freigeben
  ReceiverThread.Start;
end;

end.

