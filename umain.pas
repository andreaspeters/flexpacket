unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, RichMemo, SynEdit, synhighlighterunixshellscript,
  SynHighlighterAny, uhostmode, umycallsign, utnc, uansi, utypes;

type


  { TFMain }

  TFMain = class(TForm)
    BBChannel1: TBitBtn;
    BBChannel2: TBitBtn;
    BBChannel3: TBitBtn;
    BBChannel4: TBitBtn;
    LMonitor1: TLabel;
    LMonitor2: TLabel;
    LMonitor3: TLabel;
    LMonitor4: TLabel;
    MainMenuItemFile: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MenuItem1: TMenuItem;
    MITNC: TMenuItem;
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    MTx: TMemo;
    Panel1: TPanel;
    SBStatus: TStatusBar;
    TMain: TTimer;
    ToolBar1: TToolBar;
    ToolButton2: TToolButton;
    procedure BBChannel4Click(Sender: TObject);
    procedure BBChannel1Click(Sender: TObject);
    procedure BBChannel3Click(Sender: TObject);
    procedure BBChannel2Click(Sender: TObject);
    procedure BtnSendClick(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure MMenuExitOnClick(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure TMainTimer(Sender: TObject);
    procedure SetChannelButtonLabel(channel: byte; LabCap: string);
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
  IsCommand: Boolean;
  HomeDir: string;

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
var i, x, y: Integer;
begin
  y := CurrentChannel;
  x := MTx.Lines.Count;
  i := 0;
  while i <= x do
  begin
    FPConfig.Channel[y].Lines.Add(MTx.Lines[i]);

    if IsCommand then
      Hostmode.SendByteCommand(y,1,MTx.Lines[i])
    else
      Hostmode.SendByteCommand(y,0,MTx.Lines[i]);
    inc(i);
  end;
  MTx.Clear;
  IsCommand := False;
end;

procedure TFMain.ShowChannelMemo(channel: byte);
var i: Byte;
begin
  for i := 1 to 4 do
  begin
    FPConfig.Channel[i].Visible := False;
  end;
  FPConfig.Channel[channel].Visible := True;
  MTx.Visible := True;
end;

procedure TFMain.BBChannel1Click(Sender: TObject);
begin
  CurrentChannel := 1;
  ShowChannelMemo(1);
  SetChannelButtonBold(1);
  SBStatus.Visible := True;
end;

procedure TFMain.BBChannel2Click(Sender: TObject);
begin
  CurrentChannel := 2;
  ShowChannelMemo(2);
  SetChannelButtonBold(2);
  SBStatus.Visible := True;
end;

procedure TFMain.BBChannel3Click(Sender: TObject);
begin
  CurrentChannel := 3;
  ShowChannelMemo(3);
  SetChannelButtonBold(3);
  SBStatus.Visible := True;
end;

procedure TFMain.BBChannel4Click(Sender: TObject);
begin
  CurrentChannel := 4;
  ShowChannelMemo(4);
  SetChannelButtonBold(4);
  SBStatus.Visible := True;
end;

procedure TFMain.FMainInit(Sender: TObject);
var i: Byte;
begin
  //Self.Width := 1140;
  //Self.Height := 715;

  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config';
  {$ELSE}
  HomeDir := GetEnvironmentVariable('USERPROFILE');
  {$ENDIF}

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
    FPConfig.Channel[i].Font.Name := 'Courier New';
    FPConfig.Channel[i].Rtf := '';
    FPConfig.Channel[i].Visible := False;
    FPConfig.Channel[i].ReadOnly := True;
  end;

  // change some parameters only for the monitor
  FPConfig.Channel[0].Left := 744;
  FPConfig.Channel[0].Top := 47;
  FPConfig.Channel[0].Width := 390;
  FPConfig.Channel[0].Height := 90;
  FPConfig.Channel[0].Visible := True;
  FPConfig.Channel[0].Font.Size := 9;
  FPConfig.Channel[0].Font.Color := clGreen;
  FPConfig.Channel[0].Rtf := 'Monitor';

  // by default show channel 1
  BBChannel1.Click;

  LoadConfigFromFile(HomeDir + '/flexpacket', FPConfig);
  Hostmode := THostmode.Create(@FPConfig);

  TMain.Enabled := True; // Enable Read Buffer Timer
  IsCommand := False;
end;

procedure TFMain.MMenuExitOnClick(Sender: TObject);
begin
  Close;
end;

procedure TFMain.OpenTNCSettings(Sender: TObject);
begin
  TFTNC.SetConfig(@FPConfig);
  TFTNC.Show;
end;

procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.SetConfig(@FPConfig);
  TFMyCallsign.Show;
end;


procedure TFMain.FormDestroy(Sender: TObject);
begin
  SaveConfigToFile(HomeDir + '/flexpacket', FPConfig);
  Hostmode.Terminate;
  Hostmode.WaitFor; // Warten, bis der Thread beendet ist
  FreeAndNil(Hostmode);
end;


procedure TFMain.SendCommand(Sender: TObject; var Key: char);
var y, x, i: Integer;
begin
  if key = #27 then
  begin
    IsCommand := True;
  end;
  if key = #13 then
  begin
    y := CurrentChannel;
    x := MTx.CaretPos.Y; // current cursor position
    if Length(MTx.Lines[x]) > 0 then
    begin
      if IsCommand then
      begin
        AddTextToMemo(FPConfig.Channel[y], #27'[34m' + MTx.Lines[x] + #13#27'[0m');
        Hostmode.SendByteCommand(y,1,MTx.Lines[x])
      end
      else
      begin
        AddTextToMemo(FPConfig.Channel[y], #27'[32m' + MTx.Lines[x] + #13#27'[0m');
        Hostmode.SendByteCommand(y,0,MTx.Lines[x]);
      end;
    end;
    IsCommand := False;
  end;
end;

procedure TFMain.TMainTimer(Sender: TObject);
var i: Integer;
    Data: string;
    Status: TStatusLine;
begin
  for i:= 0 to 4 do
  begin
    Data := '';
    Data := Hostmode.ReadChannelBuffer(i);
    if Length(Data) > 0 then
    begin
      AddTextToMemo(FPConfig.Channel[i], Data);
    end;
    Status := Hostmode.GetStatus(i);
    // 0 = Number of link status messages not yet displayed)
    // 1 = Number of receive frames not yet displayed
    // 2 = Number of send frames not yet transmitted
    // 3 = Number of transmitted frames not yet acknowledged
    // 4 = Number of tries on current operation
    // 5 = Link state
    // 6 = Status Text (CONNECTED, DISCONNECTED, etc
    // 7 = The CALL of the other station
    // 8 = call of the digipeater

    SBStatus.Panels[1].Text := 'UnDisp: ' + Status[0];
    SBStatus.Panels[2].Text := 'UnSent: ' + Status[2];
    SBStatus.Panels[3].Text := 'UnAck: ' + Status[3];
    SBStatus.Panels[4].Text := 'Retry: ' + Status[4];
    SBStatus.Panels[5].Text := 'ConTo: ' + Status[7];
    SBStatus.Panels[6].Text := 'ConVia: ' + Status[8];

    if Length(Status[7]) > 0 then
      SetChannelButtonLabel(i,Status[7])
    else
      SetChannelButtonLabel(i,'Disc');
  end;
end;

procedure TFMain.SetChannelButtonLabel(channel: byte; LabCap: string);
var i: Byte;
    Lab: TLabel;
begin
  for i := 1 to 4 do
  begin
    Lab := TLabel(Self.FindComponent('LMonitor'+IntToStr(i)));
    if Assigned(Lab) then
      Lab.Caption := LabCap;
  end;
end;

procedure TFMain.AddTextToMemo(Memo: TRichMemo; Data: string);
var Segments: uansi.TGraphicArray;
begin
  Segments := uansi.ApplyANSIColor(Data);
  uansi.DisplayANSITextInMemo(Memo, Segments);
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
    WriteLn(FileHandle, Config.ComPort);
    WriteLn(FileHandle, IntToStr(Config.ComSpeed));
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
    ReadLn(FileHandle, Config.ComPort);
    ReadLn(FileHandle, Config.ComSpeed);
    ReadLn(FileHandle, Config.Callsign);
  finally
    CloseFile(FileHandle);
  end;
end;

end.


