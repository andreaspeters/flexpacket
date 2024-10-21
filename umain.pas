unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, RichMemo, SynEdit, synhighlighterunixshellscript,
  SynHighlighterAny, AX25Helper, umycallsign, utnc;

type

  { TFMain }

  TFMain = class(TForm)
    BBChannelOne: TBitBtn;
    BBChannelTwo: TBitBtn;
    BBChannelThree: TBitBtn;
    BBChannelFour: TBitBtn;
    BMonitor: TButton;
    BEditor: TButton;
    BtnSend: TButton;
    MainMenuItemFile: TMenuItem;
    MainMenuItemWindow: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MainMenuItemInfo: TMenuItem;
    MTx: TMemo;
    MITNC: TMenuItem;
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    RMRx: TRichMemo;
    procedure BtnSendClick(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendCommand(Sender: TObject; var Key: char);
  private
  public

  end;

var
  FMain: TFMain;
  AX25: TAX25Helper;

implementation

{$R *.lfm}


{ TFMain }

procedure TFMain.BtnSendClick(Sender: TObject);
begin
  AX25.TriggerSend;
end;

procedure TFMain.FMainInit(Sender: TObject);
begin
  AX25 := TAX25Helper.Create(RMRx, MTx, '/dev/ttyUSB0');
end;

procedure TFMain.OpenTNCSettings(Sender: TObject);
begin
  TFTNC.Show;
end;

procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.Show;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  AX25.Terminate;
  AX25.WaitFor; // Warten, bis der Thread beendet ist
  FreeAndNil(AX25);
end;


procedure TFMain.SendCommand(Sender: TObject; var Key: char);
begin
  if key = #27 then
  begin
    AX25.SendEscape;
  end;

end;

end.

