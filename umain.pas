unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, RichMemo, SynEdit, synhighlighterunixshellscript,
  SynHighlighterAny, uhostmode, umycallsign, utnc, uansi;

type

    TCom = record
      Port: string;
      Speed: integer;
    end;

    TFPConfig = record
      Channel: array[0..4] of TRichMemo;
      ChannelBuffer: array[0..4] of string;
      Com: TCom;
      Callsign: string;
    end;

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
    SBStatus: TStatusBar;
    TMain: TTimer;
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
    procedure TMainTimer(Sender: TObject);
  private
    procedure ShowChannelMemo(channel: byte);
    procedure SetChannelButtonBold(channel: byte);
    procedure LoadConfigFromFile(const FileName: string; var Config: TFPConfig);
    procedure AddTextToMemo(Memo: TRichMemo; Data: string);
  public
    procedure SaveConfigToFile(const FileName: string; var Config: TFPConfig);
  end;

var
  FMain: TFMain;
  Hostmode: THostmode;
  FPConfig: TFPConfig;
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

  SBStatus.Panels[0].Text := 'Channel: ' + IntToStr(channel);
end;

procedure TFMain.BtnSendClick(Sender: TObject);
begin
  Hostmode.TriggerSend;
end;

procedure TFMain.ShowChannelMemo(channel: byte);
var i: Byte;
begin
  for i := 0 to 4 do
  begin
    FPConfig.Channel[i].Visible := False;
  end;
  FPConfig.Channel[channel].Visible := True;
end;

procedure TFMain.BBChannel1Click(Sender: TObject);
begin
  CurrentChannel := 1;
  Hostmode.SetChannel(1, @FPConfig.Channel[1]);
  ShowChannelMemo(1);
  SetChannelButtonBold(1);
end;

procedure TFMain.BBChannel2Click(Sender: TObject);
begin
  CurrentChannel := 2;
  Hostmode.SetChannel(2, @FPConfig.Channel[2]);
  ShowChannelMemo(2);
  SetChannelButtonBold(2);
end;

procedure TFMain.BBChannel3Click(Sender: TObject);
begin
  CurrentChannel := 3;
  Hostmode.SetChannel(3, @FPConfig.Channel[3]);
  ShowChannelMemo(3);
  SetChannelButtonBold(3);
end;

procedure TFMain.BBChannel4Click(Sender: TObject);
begin
  CurrentChannel := 4;
  Hostmode.SetChannel(4, @FPConfig.Channel[4]);
  ShowChannelMemo(4);
  SetChannelButtonBold(4);
end;

procedure TFMain.BBChannel0Click(Sender: TObject);
begin
  CurrentChannel := 0;
  Hostmode.SetChannel(0, @FPConfig.Channel[0]);
  ShowChannelMemo(0);
  SetChannelButtonBold(0);
end;

procedure TFMain.FMainInit(Sender: TObject);
var i: Byte;
begin
  for i := 0 to 4 do
  begin
    FPConfig.Channel[i] := TRichMemo.Create(Self);
    FPConfig.Channel[i].Parent := Self;
    FPConfig.Channel[i].Left := 8;
    FPConfig.Channel[i].Top := 155;
    FPConfig.Channel[i].Width := 1126;
    FPConfig.Channel[i].Height := 455;
    FPConfig.Channel[i].Font.Color := clBlack;
    FPConfig.Channel[i].Font.Pitch := fpFixed;
    FPConfig.Channel[i].Rtf := '';
    FPConfig.Channel[i].Visible := False;
  end;
  LoadConfigFromFile('/tmp/flexpaket', FPConfig);
  Hostmode := THostmode.Create(MTx, FPConfig.Com.Port);

  TFTNC.SetHelper(@Hostmode);
  TFTNC.InitTNC;
  TMain.Enabled := True;
end;

procedure TFMain.MMenuExitOnClick(Sender: TObject);
begin
  Close;
end;

procedure TFMain.OpenTNCSettings(Sender: TObject);
begin
  TFTNC.Show;
end;

procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.SetConfig(@FPConfig);
  TFMyCallsign.Show;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  SaveConfigToFile('/tmp/flexpaket', FPConfig);
  Hostmode.Terminate;
  Hostmode.WaitFor; // Warten, bis der Thread beendet ist
  FreeAndNil(Hostmode);
end;


procedure TFMain.SendCommand(Sender: TObject; var Key: char);
begin
  if key = #27 then
  begin
    Hostmode.SendEscape;
  end;
end;

procedure TFMain.TMainTimer(Sender: TObject);
var Data: string;
begin
  Data := Hostmode.ReadChannelBuffer(0);
  AddTextToMemo(FPConfig.Channel[0], Data);
end;


procedure TFMain.AddTextToMemo(Memo: TRichMemo; Data: string);
var Segments: uansi.TGraphicArray;
begin
  Segments := uansi.ApplyANSIColor(Data);
  DisplayANSITextInMemo(Memo, Segments);
  if Memo.Visible then
  begin
    Memo.SelStart := Memo.GetTextLen;
    Memo.ScrollBy(0, Memo.Lines.Count);
    Memo.Refresh;
  end;
end;

procedure TFMain.SaveConfigToFile(const FileName: string; var Config: TFPConfig);
var
  FileHandle: TextFile;
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

procedure TFMain.LoadConfigFromFile(const FileName: string; var Config: TFPConfig);
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


