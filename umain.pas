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
  sock: Integer;
  packet: TAX25Packet;
  recvBytes: ssize_t;
begin
  // Öffne ein AX.25-Socket auf dem Interface 'ax0'
  sock := OpenAX25Socket('ax0');
  if sock < 0 then
  begin
    UChannel.FChannel.MRx.Lines.Add('Fehler beim Erstellen des AX.25 Sockets.');
    Exit;
  end;
  UChannel.FChannel.MRx.Lines.Add('Lausche auf AX.25 Pakete auf ax0');

  while True do
  begin
    // Lese das empfangene AX.25 Paket
    recvBytes := FpRecv(sock, @packet, SizeOf(packet), 0);

    // Überprüfe, ob das empfangene Paket für das Zielrufzeichen bestimmt ist
    if (recvBytes > 0) and IsTargetCallsign(packet) then
    begin
      UChannel.FChannel.MRx.Lines.Add('Paket empfangen von: ' + AX25CallsignToString(packet.source));
      UChannel.FChannel.MRx.Lines.Add('Paketinhalt: ' + PChar(@packet.info[0]));
    end;
  end;

  // Schließe das Socket, wenn fertig
  FpClose(sock);
end;

end.

