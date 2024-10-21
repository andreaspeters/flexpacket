unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, uchannel, AX25Helper, BaseUnix, sockets, umonitor, umycallsign;

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
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    procedure BBChannelOneClick(Sender: TObject);
    procedure BMonitorClick(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
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

procedure TFMain.BMonitorClick(Sender: TObject);
begin
  TFMonitor.Show;
end;

procedure TFMain.FMainInit(Sender: TObject);
begin

end;

procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.Show;
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

