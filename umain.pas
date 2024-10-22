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
    BBChannel1: TBitBtn;
    BBChannel2: TBitBtn;
    BBChannel3: TBitBtn;
    BBChannel4: TBitBtn;
    BBChannel0: TButton;
    BEditor: TButton;
    BtnSend: TButton;
    LMonitor1: TLabel;
    LMonitor2: TLabel;
    LMonitor3: TLabel;
    LMonitor4: TLabel;
    MainMenuItemFile: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MenuItem1: TMenuItem;
    MTx: TMemo;
    MITNC: TMenuItem;
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    StatusBar1: TStatusBar;
    procedure BBChannel4Click(Sender: TObject);
    procedure BBChannel1Click(Sender: TObject);
    procedure BBChannel3Click(Sender: TObject);
    procedure BBChannel2Click(Sender: TObject);
    procedure BBChannel0Click(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure MMenuExitOnClick(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendCommand(Sender: TObject; var Key: char);
  private
    procedure ShowChannelMemo(channel: byte);
    procedure SetChannelButtonBold(channel: byte);
    procedure LoadConfigFromFile(const FileName: string; var Config: TAX25Config);
  public
    procedure SaveConfigToFile(const FileName: string; var Config: TAX25Config);
  end;

var
  FMain: TFMain;
  AX25: TAX25Helper;
  AX25Config: TAX25Config;
  CurrentChannel: byte;

implementation

{$R *.lfm}


{ TFMain }

procedure TFMain.SetChannelButtonBold(channel: byte);
var i: Byte;
    Btn: TBitBtn;
begin
  for i := 0 to 4 do
  begin
    Btn := TBitBtn(Self.FindComponent('BBChannel'+IntToStr(i)));
    if Assigned(Btn) then
    begin
      Btn.Font.Style := [];
      if i = channel then
      begin
        Btn.Font.Style := [fsBold];
      end;
    end;
  end;
end;

procedure TFMain.BtnSendClick(Sender: TObject);
begin
  AX25.TriggerSend;
end;

procedure TFMain.ShowChannelMemo(channel: byte);
var i: Byte;
begin
  for i := 0 to 4 do
  begin
    AX25Config.Channel[i].Visible := False;
  end;
  AX25Config.Channel[channel].Visible := True;
end;

procedure TFMain.BBChannel1Click(Sender: TObject);
begin
  CurrentChannel := 1;
  AX25.SetChannel(1, @AX25Config.Channel[1]);
  ShowChannelMemo(1);
  SetChannelButtonBold(1);
end;

procedure TFMain.BBChannel2Click(Sender: TObject);
begin
  CurrentChannel := 2;
  AX25.SetChannel(2, @AX25Config.Channel[2]);
  ShowChannelMemo(2);
  SetChannelButtonBold(2);
end;

procedure TFMain.BBChannel3Click(Sender: TObject);
begin
  CurrentChannel := 3;
  AX25.SetChannel(3, @AX25Config.Channel[3]);
  ShowChannelMemo(3);
  SetChannelButtonBold(3);
end;

procedure TFMain.BBChannel4Click(Sender: TObject);
begin
  CurrentChannel := 4;
  AX25.SetChannel(4, @AX25Config.Channel[4]);
  ShowChannelMemo(4);
  SetChannelButtonBold(4);
end;

procedure TFMain.BBChannel0Click(Sender: TObject);
begin
  CurrentChannel := 0;
  AX25.SetChannel(0, @AX25Config.Channel[0]);
  ShowChannelMemo(0);
  SetChannelButtonBold(0);
end;


procedure TFMain.FMainInit(Sender: TObject);
var i: Byte;
begin
  for i := 0 to 4 do
  begin
    AX25Config.Channel[i] := TRichMemo.Create(Self);
    AX25Config.Channel[i].Parent := Self;
    AX25Config.Channel[i].Left := 8;
    AX25Config.Channel[i].Top := 155;
    AX25Config.Channel[i].Width := 1126;
    AX25Config.Channel[i].Height := 455;
    AX25Config.Channel[i].Font.Color := clBlack;
    AX25Config.Channel[i].Font.Pitch := fpFixed;
    AX25Config.Channel[i].Rtf := '';
    AX25Config.Channel[i].Visible := False;
  end;
  LoadConfigFromFile('/tmp/flexpaket', AX25Config);
  AX25 := TAX25Helper.Create(@AX25Config.Channel, MTx, AX25Config.Com.Port);
end;

procedure TFMain.MMenuExitOnClick(Sender: TObject);
begin
  Close;
end;

procedure TFMain.OpenTNCSettings(Sender: TObject);
begin
  TFTNC.Show;
  TFTNC.SetConfig(@AX25Config);
end;

procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.Show;
  TFMyCallsign.SetConfig(@AX25Config);
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  SaveConfigToFile('/tmp/flexpaket', AX25Config);
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


procedure TFMain.SaveConfigToFile(const FileName: string; var Config: TAX25Config);
var
  FileHandle: TextFile;
  i: byte;
begin
  AssignFile(FileHandle, FileName);
  Rewrite(FileHandle);
  try
    WriteLn(FileHandle, Config.Com.Port);
    WriteLn(FileHandle, IntToStr(Config.Com.Speed));
    WriteLn(FileHandle, Config.Callsign);
  finally
    CloseFile(FileHandle);
  end;
end;

procedure TFMain.LoadConfigFromFile(const FileName: string; var Config: TAX25Config);
var
  FileHandle: TextFile;
begin
  if not FileExists(FileName) then Exit;

  AssignFile(FileHandle, FileName);
  Reset(FileHandle);
  try
    ReadLn(FileHandle, Config.Com.Port);
    ReadLn(FileHandle, Config.Com.Speed);
    ReadLn(FileHandle, Config.Callsign);
  finally
    CloseFile(FileHandle);
  end;
end;

end.

