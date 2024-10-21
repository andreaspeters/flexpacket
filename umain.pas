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
  private
    procedure ReadAX25Packets;
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

procedure TFMain.FMainInit(Sender: TObject);
begin
  ReadAX25Packets;
end;


procedure TFMain.ReadAX25Packets;
var
  AX25Thread: TReadAX25PacketsThread;
begin
  // Starte den Thread zur Paketüberwachung und übergebe die Memo-Komponente
  try
    AX25Thread := TReadAX25PacketsThread.Create(False, 'ax0', UChannel.FChannel.MRx);
    UChannel.FChannel.MRx.Lines.Add('Lausche auf AX.25 Pakete auf ax0');
  except
    on E: Exception do
      UChannel.FChannel.MRx.Lines.Add('Fehler beim Starten des AX.25 Threads: ' + E.Message);
  end;
end;

end.

