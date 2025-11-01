unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, ActnList, LazSerial, uCmdBoxCustom, uCmdBox,
  uhostmode, umycallsign, utnc, utypes, uinfo, uterminalsettings,
  uresize, uini, uaddressbook, uagwpeclient, uagw, ufileupload, System.UITypes,
  u7plus, LCLIntf, RegExpr, Process, upipes, LCLType, PairSplitter, ukissmode,
  ukiss, MD5, ulistmails, LConvEncoding, ueditor, uconvers;

type


  { TFMain }

  TFMain = class(TForm)
    actAGWSettings: TAction;
    actInitTNC: TAction;
    actEnableTNC: TAction;
    actEnableTFKISS: TAction;
    actEnableAGW: TAction;
    actGet7Plus: TAction;
    actGetAPRSMap: TAction;
    actGetTFKISS: TAction;
    actInfo: TAction;
    actGetBayComPassword: TAction;
    actCmdSendEscape: TAction;
    actCmdSendReturn: TAction;
    actDonate: TAction;
    actHamradiotech: TAction;
    actBymeacoffee: TAction;
    actEditor: TAction;
    actMainShowHide: TAction;
    actOpenConvers: TAction;
    actQuickConnect: TAction;
    actListMails: TAction;
    actToggleIconSize: TAction;
    actTerminalSettings: TAction;
    actSetCallSign: TAction;
    actTFKISSSettings: TAction;
    actTNCSettings: TAction;
    actFileRestart: TAction;
    actFileExit: TAction;
    ActionList1: TActionList;
    AOpenAddressbook: TAction;
    ALShortCuts: TActionList;
    ILImagesBig: TImageList;
    ILImagesSmall: TImageList;
    Image2: TImage;
    ILApplicationIcons: TImageList;
    MainMenuItemFile: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    MIToolbarSize: TMenuItem;
    MIKissSettings: TMenuItem;
    MIGetTFKISS: TMenuItem;
    MIEnableKISS: TMenuItem;
    MIGetAPRSMap: TMenuItem;
    MIExitButton: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MIExit: TMenuItem;
    MIShowHide: TMenuItem;
    MIGet7Plus: TMenuItem;
    MIAGWSettings: TMenuItem;
    MIEnableTNC: TMenuItem;
    MIEnableAGW: TMenuItem;
    MITNC: TMenuItem;
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    ODFileUpload: TOpenDialog;
    pmCmdBox: TPopupMenu;
    PSChannelSplitter: TPairSplitter;
    PSSChannel: TPairSplitterSide;
    PSSMTx: TPairSplitterSide;
    PMTrayIcon: TPopupMenu;
    SBStatus: TStatusBar;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    Separator3: TMenuItem;
    Separator4: TMenuItem;
    Separator5: TMenuItem;
    TBMap: TToolButton;
    TMain: TTimer;
    ToolBar1: TToolBar;
    TBAdressbook: TToolButton;
    TBFormular: TToolButton;
    TBFileUpload: TToolButton;
    TB7Plus: TToolButton;
    TBPassword: TToolButton;
    TBMessages: TToolButton;
    TBEditor: TToolButton;
    ToolButton1: TToolButton;
    TrayIcon: TTrayIcon;
    procedure actBymeacoffeeExecute(Sender: TObject);
    procedure actCmdSendEscapeExecute(Sender: TObject);
    procedure actCmdSendReturnExecute(Sender: TObject);
    procedure actDonateExecute(Sender: TObject);
    procedure actEditorExecute(Sender: TObject);
    procedure actFileExitExecute(Sender: TObject);
    procedure actGetBayComPasswordExecute(Sender: TObject);
    procedure actHamradiotechExecute(Sender: TObject);
    procedure actMainShowHideExecute(Sender: TObject);
    procedure actListMailsExecute(Sender: TObject);
    procedure actOpenConversExecute(Sender: TObject);
    procedure actQuickConnectExecute(Sender: TObject);
    procedure actToggleIconSizeExecute(Sender: TObject);
    procedure AOpenAddressbookExecute(Sender: TObject);
    procedure CmdBox1MouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FMainInit(Sender: TObject);
    procedure BtnReInitTNCOnClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure EnableTNCClick(Sender: TObject);
    procedure EnableAGWClick(Sender: TObject);
    procedure EnableKISSClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MIGetAPRSMapClick(Sender: TObject);
    procedure MIGetTFKISSClick(Sender: TObject);
    procedure MIKissSettingsClick(Sender: TObject);
    procedure MIShowHideClick(Sender: TObject);
    procedure MIGet7PlusClick(Sender: TObject);
    procedure MIAGWSettingsClick(Sender: TObject);
    procedure MovePairSplitter(Sender: TObject);
    procedure OpenTerminalSettings(Sender: TObject);
    procedure ResizeForm(Sender: TObject);
    procedure actFileRestartExecute(Sender: TObject);
    procedure ShowInfo(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure TB7PlusClick(Sender: TObject);
    procedure TBAdressbookClick(Sender: TObject);
    procedure TBFileUploadClick(Sender: TObject);
    procedure TBMapClick(Sender: TObject);
    procedure TMainTimer(Sender: TObject);
    procedure SetChannelButtonLabel(channel: byte; LabCap: string);
    procedure HostmodeThreadTerminated(Sender: TObject);
    procedure AGWThreadTerminated(Sender: TObject);
    procedure ChangeCommandMode(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure GetBayCom( const Channel: Byte; const Data: String);
    procedure StoreMail( const Channel: Byte; const Data: String);
  private
    procedure ShowChannelMemo(const channel: byte);
    procedure ShowMTxMemo(const channel: byte);
    procedure ShowPTxPanel(const channel: byte);
    procedure SetChannelButtonBold(const Channel: Byte);
    procedure AddTextToMemo(Const Channel: Byte; const Data: AnsiString);
    procedure BBChannelClick(Sender: TObject);
    Procedure UploadFile(Sender: TObject);
    procedure GetStatus(const Channel: Byte);
    procedure GetAutoBin(const Channel: Byte; const Data: String);
    procedure GetAPRSMessage(const Data: String);
    procedure CheckConnected(const Channel: Byte; const Data: String);
    procedure CheckDisconnected(const Channel: Byte; const Data: String);
    procedure SetIconSize(const big: Boolean);
    function ReadChannelBuffer(const Channel: byte):string;
    function ReadDataBuffer(const Channel: Byte):TBytes;
  public
    CurrentChannel: byte;
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    procedure SendStringCommand(const Channel, Code: byte; const Command: string);
  end;

var
  FMain: TFMain;
  Hostmode: THostmode;
  KISSmode: TKissmode;
  AGWClient: TAGWPEClient;
  FPConfig: TFPConfig;
  HomeDir: string;
  OrigWidth, OrigHeight: Integer;
  BBChannel: TBChannel;
  LMChannel: TLChannel;
  APRSHeader: String;

implementation

{$R *.lfm}


{ TFMain }

{
  SetChannelButtonBold

  Change the Button caption to Bold.
}
procedure TFMain.SetChannelButtonBold(const channel: byte);
var i, x: Byte;
    Btn: TBitBtn;
begin
  for i := 0 to FPConfig.MaxChannels do
  begin
    Btn := TBitBtn(Self.FindComponent('BBChannel'+IntToStr(i)));
    if Assigned(Btn) then
    begin
      for x:= 0 to SBStatus.Panels.Count - 1 do
        SBStatus.Panels[x].Text := '';
      Btn.Font.Style := [];
      if i = channel then
      begin
        Btn.Font.Style := [fsBold];
      end;
    end;
  end;

  SBStatus.Panels[0].Text := 'Channel: ' + IntToStr(channel);
end;

{
  ShowMTxMemo

  Hide all channel TX memos except the one of the current channel
}
procedure TFMain.ShowMTxMemo(const channel: byte);
var i: Byte;
begin
  for i := 0 to FPConfig.MaxChannels do
    if not FPConfig.IsConvers[i] then
      FPConfig.MTx[i].Visible := False;

  FPConfig.MTx[channel].Visible := True;

  if Assigned(FPConfig.MTx[channel]) and FPConfig.MTx[channel].CanSetFocus then
    FPConfig.MTx[channel].SetFocus;
end;

{
  ShowPTxPanel

  Hide all tx pannels (the red line as indication of sending command) except
  the one of the current channel
}
procedure TFMain.ShowPTxPanel(const channel: byte);
var i: Byte;
begin
  for i := 0 to FPConfig.MaxChannels do
    if not FPConfig.IsConvers[i] then
      FPConfig.PTx[i].Visible := False;

  FPConfig.PTx[channel].Visible := True;
end;

{
  ShowChannelMemo

  Hide all RX memos except the one of the current channel
}
procedure TFMain.ShowChannelMemo(const channel: byte);
var i: Byte;
begin
  for i := 0 to FPConfig.MaxChannels do
    if not FPConfig.IsConvers[i] then
      FPConfig.Channel[i].Visible := False;

  FPConfig.Channel[channel].Visible := True;
end;

{
  FMainInit

  Initialize Controls, Records and Threads.
}
procedure TFMain.FMainInit(Sender: TObject);
var i: Byte;
    FontSize, nextBtnLeft: Integer;
begin
  Self.Width := 1137;
  Self.Height := 716;

  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  LoadConfigFromFile(@FPConfig);

  FontSize := 11;
  if FPConfig.TerminalFontSize > 0 then
    FontSize := FPConfig.TerminalFontSize;

  MIToolbarSize.Checked := FPConfig.TerminalToolbarBig;
  SetIconSize(MIToolbarSize.Checked);

  if (FPConfig.MainWidth > 0) and (FPConfig.MainHeight > 0) and (FPConfig.TerminalHeight > 0) then
  begin
    Self.Width := FPConfig.MainWidth;
    Self.Height := FPConfig.MainHeight;
    OrigWidth := Self.Width;
    OrigHeight := Self.Height;
    PSChannelSplitter.Position := FPConfig.TerminalHeight;
  end;

  // init channel RX
  // 0 is monitoring channel
  // MaxChannel is Convers Channel (only visible in convers window)
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i] := TCmdBoxCustom.Create(Self);
    FPConfig.Channel[i].EscapeCodeType := esctAnsi;
    FPConfig.Channel[i].Parent := PSSChannel;
    FPConfig.Channel[i].Left := 4;
    FPConfig.Channel[i].Width := PSChannelSplitter.Width;
    FPConfig.Channel[i].Font.Color := FPConfig.TerminalFontColor;
    FPConfig.Channel[i].Font.Pitch := fpFixed;
    FPConfig.Channel[i].Font.Name := FPConfig.TerminalFontName;
    FPConfig.Channel[i].Font.Style := [fsBold];
    FPConfig.Channel[i].Font.Size := FontSize;
    FPConfig.Channel[i].BackGroundColor := FPConfig.TerminalBGColor;
    FPConfig.Channel[i].TextBackground(FPConfig.TerminalBGColor);
    FPConfig.Channel[i].TextColor(FPConfig.TerminalFontColor);
    FPConfig.Channel[i].Visible := False;
    FPConfig.Channel[i].Enabled := True;
    FPConfig.Channel[i].InputSelBackGround := clRed;
    FPConfig.Channel[i].Anchors := [akLeft,akRight,akTop,akBottom];
    FPConfig.Channel[i].PopupMenu := pmCmdBox;

    FPConfig.Connected[i] := False;
    FPConfig.Download[i] := FFileUpload.Default;
    FPConfig.IsConvers[i] := False;
  end;

  // change some parameters only for the monitor
  FPConfig.Channel[0].Font.Color := clGreen;
  FPConfig.Channel[0].TextBackground(clWhite);
  FPConfig.Channel[0].TextColor(clGreen);
  FPConfig.Channel[0].BackGroundColor := clWhite;

  // init MTx TX
  // 0 is monitoring channel
  // MaxChannel is Convers Channel (only visible in convers window)
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.MTx[i] := TMemo.Create(Self);
    FPConfig.MTx[i].Parent := PSSMTx;
    FPConfig.MTx[i].Left := 4;
    FPConfig.MTx[i].Top := 5;
    FPConfig.MTx[i].Width := PSChannelSplitter.Width;
    FPConfig.MTx[i].Height := PSSMTx.Height - 10;
    FPConfig.MTx[i].Font.Color := clBlack;
    FPConfig.MTx[i].Font.Pitch := fpFixed;
    FPConfig.MTx[i].Font.Name := 'Courier New';
    FPConfig.MTx[i].Font.Size := FontSize - 1;
    FPConfig.MTx[i].Color := clDefault;
    FPConfig.MTx[i].Text := '';
    FPConfig.MTx[i].Visible := False;
    FPConfig.MTx[i].ScrollBars := ssAutoVertical;
    FPConfig.MTx[i].Anchors := [akLeft,akRight,akTop];
    FPConfig.MTx[i].OnKeyPress := @SendCommand;
  end;

  // init PTx Statusbar
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.PTx[i] := TPanel.Create(Self);
    FPConfig.PTx[i].Parent := PSSMTx;
    FPConfig.PTx[i].Left := 4;
    FPConfig.PTx[i].Top := 0;
    FPConfig.PTx[i].Width := PSChannelSplitter.Width;
    FPConfig.PTx[i].Height := 2;
    FPConfig.PTx[i].Color := clDefault;
    FPConfig.PTx[i].Visible := False;
    FPConfig.PTx[i].BevelOuter := bvNone;
    FPConfig.PTx[i].BevelInner := bvRaised;
    FPConfig.PTx[i].BevelColor := clForm;
    FPConfig.PTx[i].Anchors := [akLeft,akRight,akTop];

    FPConfig.IsCommand[i] := False;
  end;

  MIEnableTNC.Checked := FPConfig.EnableTNC;
  MIEnableAGW.Checked := FPConfig.EnableAGW;
  MIEnableKISS.Checked := FPConfig.EnableKISS;


  if Length(FPConfig.Callsign) > 0 then
     FMain.Caption :=  FMain.Caption + ' - ' + FPConfig.Callsign;

  Hostmode := nil;
  KISSmode := nil;
  AGWClient := nil;

  if MIEnableKISS.Checked then
  begin
    KISSmode := TKISSmode.Create(@FPConfig);
    KISSmode.Start;
//    KISSmode.OnTerminate := @KISSmodeThreadTerminated;
  end;

  if MIEnableTNC.Checked then
  begin
    if Length(FPConfig.ComPort) <= 0 then
      ShowMessage('Please configure the TNC Com Port');

    Hostmode := THostmode.Create(@FPConfig);
    Hostmode.Start;
//    Hostmode.OnTerminate := @HostmodeThreadTerminated;
  end;

  if MIEnableAGW.Checked then
  begin
    AGWClient := TAGWPEClient.Create(@FPConfig);
    AGWClient.OnTerminate := @AGWThreadTerminated;
    AGWClient.Start;
    FPConfig.MaxChannels := 1;
    TBFileUpload.Enabled := False;
  end;

  // init Channel Buttons and labels
  nextBtnLeft := 0;
  for i := 0 to FPConfig.MaxChannels do
  begin
    BBChannel[i] := TBitBtn.Create(Self);
    BBChannel[i].Parent := FMain;
    BBChannel[i].Left := 4 + nextBtnLeft;
    BBChannel[i].Top := Toolbar1.Top + Toolbar1.Height + 10;
    BBChannel[i].Height := 48;
    BBChannel[i].Width := 56;
    BBChannel[i].Caption := '&' + IntToStr(i);
    BBChannel[i].onClick := @BBChannelClick;
    BBChannel[i].Name := 'BBChannel'+IntToStr(i);

    nextBtnLeft := nextBtnLeft + BBChannel[i].Width + 5;

    LMChannel[i] := TLabel.Create(Self);
    LMChannel[i].Parent := FMain;
    LMChannel[i].Top := BBChannel[i].Top + BBChannel[i].Height + 10;
    LMChannel[i].Width := 56;
    LMChannel[i].Font.Size := 8;
    LMChannel[i].Font.Style := [fsBold];
    LMChannel[i].Name := 'LMonitor'+IntToStr(i);
    LMChannel[i].Anchors := [akLeft,akTop];
    SetChannelButtonLabel(i,'Disc');
  end;

  SetChannelButtonLabel(0,'Monitor');

  // by default show channel 0
  BBChannel[0].Click;

  TMain.Enabled := True; // Enable Read Buffer Timer

  // Save size and possition of all elements to make window resize possible
  SetLength(ControlInfoList, 0);

  StoreOriginalSizes(Self);
  FFileUpload.SetConfig(@FPConfig);
end;

procedure TFMain.AOpenAddressbookExecute(Sender: TObject);
begin
  TBAdressbookClick(Sender);
end;

procedure TFMain.CmdBox1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin

end;

{
  FileExitExecute

  Action procedure for the Exit button in the Main Menu.
}
procedure TFMain.actFileExitExecute(Sender: TObject);
begin
  Close;
end;

procedure TFMain.actDonateExecute(Sender: TObject);
begin
  if not OpenURL('https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ') then
    ShowMessage('Could not open URL: https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ');
end;

procedure TFMain.actEditorExecute(Sender: TObject);
begin
  TFEditor.SetConfig(@FPConfig);
  TFEditor.Show;
end;

procedure TFMain.actCmdSendEscapeExecute(Sender: TObject);
begin
  SendStringCommand(CurrentChannel,0,#27)
end;

procedure TFMain.actBymeacoffeeExecute(Sender: TObject);
begin
  if not OpenURL('https://buymeacoffee.com/hamradiotech') then
    ShowMessage('Could not open URL: https://buymeacoffee.com/hamradiotech');
end;

procedure TFMain.actCmdSendReturnExecute(Sender: TObject);
begin
  SendStringCommand(CurrentChannel,0,#13)
end;

{
  ChangeCommandMode

  Will switch between command and info mode.
}
procedure TFMain.ChangeCommandMode(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = VK_Escape then
  begin
    if FPConfig.IsCommand[CurrentChannel] then
    begin
      FPConfig.IsCommand[CurrentChannel] := False;
      FPConfig.PTx[CurrentChannel].BevelColor := clForm;
    end
    else
    begin
      FPConfig.IsCommand[CurrentChannel] := True;
      FPConfig.PTx[CurrentChannel].BevelColor := clRed;
      FPConfig.MTx[CurrentChannel].SetFocus;
    end;
  end;
end;

procedure TFMain.HostmodeThreadTerminated(Sender: TObject);
begin
  Hostmode := THostmode.Create(@FPConfig);
end;

procedure TFMain.AGWThreadTerminated(Sender: TObject);
begin
  AGWClient := TAGWPEClient.Create(@FPConfig);
end;
{
  BBChannelClick

  If the user clicked the channel button, we have to change pannels and button style
}
procedure TFMain.BBChannelClick(Sender: TObject);
var btn: TBitBtn;
begin
  if Sender is TBitBtn then
  begin
     btn := TBitBtn(Sender);
     CurrentChannel := StrToInt(btn.Caption);
     ShowChannelMemo(CurrentChannel);
     ShowMTxMemo(CurrentChannel);
     ShowPTxPanel(CurrentChannel);
     SetChannelButtonBold(CurrentChannel);
     SBStatus.Visible := True;
  end;
end;

{
  BtnReInitTNCOnClick

  Reinitialize TNC
}
procedure TFMain.BtnReInitTNCOnClick(Sender: TObject);
begin
  if MIEnableTNC.Checked then
  begin
    Hostmode.Terminate;
    Hostmode.WaitFor;
    Hostmode.Free;
    Hostmode := THostmode.Create(@FPConfig);
    Hostmode.Start;
//    Hostmode.OnTerminate := @HostmodeThreadTerminated;

    Hostmode.LoadTNCInit;
    Hostmode.SetCallsign;
  end;
end;

procedure TFMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FPConfig.MainX := FMain.Left;
  FPConfig.MainY := FMain.Top;
  FPConfig.ConversX := TFConvers.Left;
  FPConfig.ConversY := TFConvers.Top;
  FPConfig.MailX := FListMails.Left;
  FPConfig.MailY := FListMails.Top;


  SaveConfigToFile(@FPConfig);
  try
    ClosePipe('flexpacketaprspipe');
  except
  end;

  if MIEnableTNC.Checked then
  begin
    Hostmode.Terminate;
    Hostmode.WaitFor;
    Hostmode.Free;
    Hostmode := nil;
  end;
end;

procedure TFMain.FormHide(Sender: TObject);
begin
  FPConfig.MainX := FMain.Left;
  FPConfig.MainY := FMain.Top;
end;


{
  FormPaint

  because of a strange behaviore we have to recalculate the position
  of the label after Form repaint
}
procedure TFMain.FormPaint(Sender: TObject);
var i: Byte;
    Lab: TLabel;
    Btn: TBitBtn;
    TextWidth: Integer;
begin

  for i := 0 to FPConfig.MaxChannels do
  begin
    Lab := TLabel(Self.FindComponent('LMonitor'+IntToStr(i)));
    Btn := TBitBtn(Self.FindComponent('BBChannel'+IntToStr(i)));

    if (Assigned(Btn)) and (Assigned(Lab)) then
    begin
      TextWidth := Lab.Canvas.TextWidth(Lab.Caption);
      Lab.Left := Btn.Left + (Btn.Width - TextWidth) div 2;
    end;
  end;
end;

{
  EnableTNCClick

  Set FlexPacket into Hostmode for HW TNC's
}
procedure TFMain.EnableTNCClick(Sender: TObject);
begin
  FPCOnfig.EnableKISS := False;
  FPConfig.EnableAGW := False;
  FPConfig.EnableTNC := True;
  TBFileUpload.Enabled := True;
  FPConfig.MaxChannels := 4;
  SaveConfigToFile(@FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
end;

{
  EnableAGWClick

  Set FlexPacket into the AGW Mode
}
procedure TFMain.EnableAGWClick(Sender: TObject);
begin
  FPConfig.EnableTNC := False;
  FPCOnfig.EnableKISS := False;
  FPConfig.EnableAGW := True;
  TBFileUpload.Enabled := False;
  SaveConfigToFile(@FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
end;

procedure TFMain.EnableKISSClick(Sender: TObject);
begin
  FPConfig.EnableTNC := False;
  FPConfig.EnableAGW := False;
  FPCOnfig.EnableKISS := True;
  SaveConfigToFile(@FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
end;

procedure TFMain.FormShow(Sender: TObject);
begin
  if (FPConfig.MainX > 0) and (FPConfig.MainY > 0) then
  begin
    Left := FPConfig.MainX;
    Top := FPConfig.MainY;
  end;

  if (FPConfig.ConversX > 0) and (FPConfig.ConversY > 0) then
  begin
     TFConvers.Left := FPConfig.ConversX;
     TFConvers.Top := FPConfig.ConversY;
  end;

  if (FPConfig.MailX > 0) and (FPConfig.MailY > 0) then
  begin
     FListMails.Left := FPConfig.ConversX;
     FListMails.Top := FPConfig.ConversY;
  end;
end;

{
  ShowHideClick

  Try Icon Menu to show and hide the PR Window
}
procedure TFMain.MIShowHideClick(Sender: TObject);
begin
  if FMain.WindowState = wsMinimized then
  begin
    FMain.WindowState := wsNormal;
    FMain.Show
  end
  else
  begin
    FMain.WindowState := wsMinimized;
    FMain.Hide;
  end;
end;

{
  MIGetAPRSMapClick

  Open then URL where the user can download APRS Map
}
procedure TFMain.MIGetAPRSMapClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/aprsmap') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/aprsmap');
end;

{
  MIGetTFKISSClick

  Open then URL where the user can download TFKISS
}
procedure TFMain.MIGetTFKISSClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/tfkiss') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/tfkiss')
end;

{
  MIGet7PlusClick

  Open then URL where the user can download 7plus
}
procedure TFMain.MIGet7PlusClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/7plus') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/7plus');
end;

{
  MIKissSettingsClick

  Open the TFKISS Settings Window
}
procedure TFMain.MIKissSettingsClick(Sender: TObject);
begin
  FKiss.SetConfig(@FPConfig);
  FKiss.Show;
end;

{
  MIAGWSettingsClick

  Open the AGW Settings Window
}
procedure TFMain.MIAGWSettingsClick(Sender: TObject);
begin
  FAGW.SetConfig(@FPConfig);
  FAGW.Show;
end;

{
  MovePairSplitter

  Save the new PairSplitter Position and change the MTx and PTx Size.
}
procedure TFMain.MovePairSplitter(Sender: TObject);
var i: Byte;
begin
  FPConfig.TerminalHeight := PSChannelSplitter.Position;

  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i].Height := PSSChannel.Height;
    FPConfig.Channel[i].Width := PSChannelSplitter.Width - 8;
    FPConfig.MTx[i].Height := PSSMTx.Height - 10;
    FPConfig.MTx[i].Width := PSChannelSplitter.Width - 8;
    FPConfig.PTx[i].Top := 0;
    FPConfig.PTx[i].Width := PSChannelSplitter.Width - 8;
  end;
end;

{
  OpenTerminalSettings

  Action function to open the Terminal Settings.
}
procedure TFMain.OpenTerminalSettings(Sender: TObject);
begin
  TFTerminalSettings.SetConfig(@FPConfig);
  TFTerminalSettings.Show;
end;

{
  ResizeForm

  This procedure will calculate the current scale factor for the resize
  procedure in uresize.pas. It's used to resize all lazarus controll at a
  form.
}
procedure TFMain.ResizeForm(Sender: TObject);
var i: Integer;
begin
  FPConfig.MainWidth := Width;
  FPConfig.MainHeight := Height;
  FPConfig.TerminalHeight := PSChannelSplitter.Position;

  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i].Height := PSSChannel.Height;
    FPConfig.Channel[i].Width := PSChannelSplitter.Width - 8;
    FPConfig.MTx[i].Height := PSSMTx.Height - 10;
    FPConfig.MTx[i].Width := PSChannelSplitter.Width - 8;
    FPConfig.PTx[i].Top := 0;
    FPConfig.PTx[i].Width := PSChannelSplitter.Width - 8;

    BBChannel[i].Top := Toolbar1.Top + Toolbar1.Height + 10;
    LMChannel[i].Top := BBChannel[i].Top + BBChannel[i].Height + 10;
  end;
end;

{
  actFileRestart

  Action procedure for the actFileRestart button in the Main Menu.
}
procedure TFMain.actFileRestartExecute(Sender: TObject);
begin
  SaveConfigToFile(@FPConfig);
  RestartApplication;
end;


procedure TFMain.ShowInfo(Sender: TObject);
begin
  TFInfo.Show;
end;

{
  OpenTNCSettings

  Action procedure to open the TNC settings.
}
procedure TFMain.OpenTNCSettings(Sender: TObject);
begin
  TFTNC.SetConfig(@FPConfig);
  TFTNC.Show;
end;

{
  OpenMyCallsign

  Action procedure to open the callsign settings.
}
procedure TFMain.OpenMyCallsign(Sender: TObject);
begin
  TFMyCallsign.SetConfig(@FPConfig);
  TFMyCallsign.Show;
end;

{
  SendCommand

  Action procedure for the TX Memo. If user hit the ESC
  key the line will be send as Command and not Data.
  If the user hit ENTER the current Memo line will be send.
}
procedure TFMain.SendCommand(Sender: TObject; var Key: char);
var y, x: Integer;
begin
  if key = #13 then
  begin
    y := CurrentChannel;
    x := FPConfig.MTx[CurrentChannel].CaretPos.Y; // current cursor position
    if Length(FPConfig.MTx[CurrentChannel].Lines[x]) > 0 then
    begin
      if FPConfig.IsCommand[CurrentChannel] then
        SendStringCommand(y,1,FPConfig.MTx[CurrentChannel].Lines[x])
      else
        SendStringCommand(y,0,FPConfig.MTx[CurrentChannel].Lines[x])
    end;
    FPConfig.IsCommand[CurrentChannel] := False;
    FPConfig.PTx[CurrentChannel].BevelColor := clForm;
  end;
end;

procedure TFMain.TB7PlusClick(Sender: TObject);
begin
  F7Plus.SetConfig(@FPConfig);
  F7Plus.Show;
end;

{
  TBAdressbookClick

  Toolbarbutton to open the Addressbook. The the QuickConnect property for
  the quickconnect button.
}
procedure TFMain.TBAdressbookClick(Sender: TObject);
begin
  TFAdressbook.Show;
end;

{
  UploadFile

  Get the AutoBin string (ufileupload.pas) and send it to the other
  PR client.
}
procedure TFMain.UploadFile(Sender: TObject);
var FileUpload: TFFileUpload;
begin
  if CurrentChannel = 0 then
  begin
    ShowMessage('No fileupload at the monitoring channel.');
    Exit;
  end;

  FileUpload := TFFileUpload(Sender);
  if Assigned(FileUpload) then
  begin
    if Length(FileUpload.AutoBin) > 0 then
    begin
      SendStringCommand(CurrentChannel, 0, FileUpload.AutoBin);
      FPConfig.Upload[CurrentChannel].Enabled := True;
      FPConfig.Upload[CurrentChannel].FileName := FileUpload.FileName;
    end;
  end;
end;

{
  TBFileUploadClick

  Toolbarbutton to show the File Upload Dialog and set the FileName.
}
procedure TFMain.TBFileUploadClick(Sender: TObject);
begin
  if ODFileUpload.Execute then
  begin
    FFileUpload.OnUpload := @UploadFile;
    FFileUpload.Filename := ODFileUpload.FileName;
    FFileUpload.Show;
  end;
end;

procedure TFMain.TBMapClick(Sender: TObject);
var run: TProcess;
begin
  CreatePipe('flexpacketaprspipe');

  run := TProcess.Create(nil);
  try
    if not FileExists(FPConfig.ExecutableAPRSMap) then
    begin
      ShowMessage('APRS Map Executable does not exist. Please configure under Teminal Settings.');
      Exit;
    end;

    run.Executable := FPConfig.ExecutableAPRSMap;
    run.Options := run.Options;
    run.Execute;
  finally
    run.Free;
  end;
end;


{
  TMainTimer

  Loop to get updates from TNC/AGW.
}
procedure TFMain.TMainTimer(Sender: TObject);
var i: Integer;
    Data: AnsiString;
begin
  for i:= 0 to FPConfig.MaxChannels do
  begin
    // handle status information
    GetStatus(i);

    if MIEnableKISS.Checked then
      if not KISSmode.Connected then
        Exit;

    if MIEnableTNC.Checked then
      if not Hostmode.Connected then
        Exit;

    if MIEnableAGW.Checked then
      if not AGWClient.Connected then
        Exit;

    // if upload is activated for this channel and the upload is not Go7
    // then download the file.
    //
    if (i > 0) and (FPConfig.Download[i].Enabled) and (FPConfig.Download[i].Autobin) then
      FFileUpload.FileDownload(ReadDataBuffer(i), i);

    // Read data from channel buffer
    Data := ReadChannelBuffer(i);

    // handle autobin messages
    GetAutoBin(i, Data);

    if i > 0 then
    begin
      // Check Connection State
      CheckConnected(i, Data);
      CheckDisconnected(i, Data);

      // Check if BayCom Password string was send
      GetBayCom(i, Data);

      // Check it's a mail
      StoreMail(i, Data);

      if (FPConfig.Download[i].Enabled) and (FPConfig.Download[i].Mail) then
        FFileUpload.FileDownload(Data, i);
    end;

    // handle aprs messages. APRS Messages can only be at the Monitoring Channel.
    if i = 0 then
      GetAPRSMessage(Data);

    // colorize text if channel is in convers mode
    if FPConfig.IsConvers[i] then
      Data := TFConvers.Convers(Data);

    if Length(Data) > 0 then
      AddTextToMemo(i, Data);
  end;
end;

{
  SetChannelButtonLabel

  This function will set the caption of the channel button labels. The caption
  will be centered under the buttons.
}
procedure TFMain.SetChannelButtonLabel(channel: byte; LabCap: string);
var i: Byte;
    Lab: TLabel;
    Btn: TBitBtn;
    TextWidth: Integer;
begin
  for i := 0 to FPConfig.MaxChannels do
  begin
    if i = channel then
    begin
      Lab := TLabel(Self.FindComponent('LMonitor'+IntToStr(i)));
      Btn := TBitBtn(Self.FindComponent('BBChannel'+IntToStr(i)));

      if (Assigned(Btn)) and (Assigned(Lab)) then
      begin
        Lab.Caption := LabCap;
        TextWidth := Lab.Canvas.TextWidth(Lab.Caption);
        Lab.Left := Btn.Left + (Btn.Width - TextWidth) div 2;
      end;
    end;
  end;
end;

{
  AddTextToMemo

  Replace basic ANSI Codes into TColor, and display it at the "Memo".
}
procedure TFMain.AddTextToMemo(const Channel: Byte; const Data: AnsiString);
var Memo: TCmdBoxCustom;
    Line: String;
    BGColor: TColor;
begin
  Memo := FPConfig.Channel[Channel];

  Line := StringReplace(Data, #13#10, #10, [rfReplaceAll]);  // Windows → Unix
  Line := StringReplace(Line, #13, #10, [rfReplaceAll]);     // Mac Classic → Unix
  Line := StringReplace(Line, #10, #13#10, [rfReplaceAll]);  // Unix → systemabhängig

  Memo.Write(CP437ToUTF8(Line));
end;

{
  SendByteCommand

  Send "Data" as array of Byte into "Channel". "Code" will define if it's data
  or command for the TNC.
  Bye transfer is not supported in AGW.
}
procedure TFMain.SendByteCommand(const Channel, Code: byte; const Data: TBytes);
begin
  if (MIEnableTNC.Checked) and (Length(Data) > 0) then
    Hostmode.SendByteCommand(Channel, Code, Data);
end;

{
  SendStringCommand

  Send "Command" as String into "Channel". "Code" will define if it's data or
  a command for the TNC.
  In AGW Mode, Channel can only be 0.
}
procedure TFMain.SendStringCommand(const Channel, Code: byte; const Command: string);
var cmd: String;
begin
  cmd := Command;
  if Code = 1 then
    cmd := UpperCase(Command);

  case Code of
    1: AddTextToMemo(Channel, #27'[96m' + cmd + #13#10#27'[0m');
    0: AddTextToMemo(Channel, #27'[32m' + cmd + #13#10#27'[0m');
  end;

  if (MIEnableKISS.Checked) and (Length(cmd) > 0) then
    KISSmode.SendStringCommand(Channel, Code, cmd);

  if (MIEnableTNC.Checked) and (Length(cmd) > 0) then
    Hostmode.SendStringCommand(Channel, Code, cmd);

  if (MIEnableAGW.Checked) and (Length(cmd) > 0) then
    AGWClient.SendStringCommand(0, Code, cmd);
end;

{
  ReadChannelBuffer

  Return the String of "Channel". These Buffer will hold all AX25 ASCII data
  that we want to display.
}
function TFMain.ReadChannelBuffer(const Channel: Byte):AnsiString;
begin
  Result := '';

  if MIEnableKISS.Checked then
  begin
    Result := KISSmode.ChannelBuffer[Channel];
    KISSmode.ChannelBuffer[Channel] := '';
  end;
  if MIEnableTNC.Checked then
  begin
    Result := Hostmode.ChannelBuffer[Channel];
    Hostmode.ChannelBuffer[Channel] := '';
  end;
  if MIEnableAGW.Checked then
  begin
    Result := AGWClient.ChannelBuffer[Channel];
    AGWClient.ChannelBuffer[Channel] := '';
  end;
end;


{
  ReadDataBuffer

  Return the Byte Buffer of "Channel" as Byte Array. These is only important
  for fileuploads.
}
function TFMain.ReadDataBuffer(const Channel: Byte):TBytes;
begin
  Result := TBytes.Create;
  SetLength(Result, 0);
  if MIEnableTNC.Checked then
  begin
    Result := Hostmode.ChannelByteData[Channel];
    SetLength(Hostmode.ChannelByteData[Channel],0);
  end;
end;

{
  GetAutoBin

  Check if "Data" in "Channel" is an AutoBin message. If it's so, prepare
  downloading or uploading.
}
procedure TFMain.GetAutoBin(const Channel: Byte; const Data: String);
var AutoBin: TStrings;
begin
  if (Length(Data) = 0) or (Channel = 0) then
    Exit;

  if not Assigned(FFileUpload) then
    Exit;

  // Check if the message is an AutoBin command
  AutoBin := FFileUpload.IsAutobin(Trim(Data));
  case AutoBin[0] of
    'BIN': // Someone want to send a file to me
    begin
      if MessageDlg('Do you want to accept the file upload '+AutoBin[4]+' ?', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
      begin
        SendStringCommand(Channel, 0, '#OK#');
        FPConfig.Download[Channel].Enabled := True;
        FPConfig.Download[Channel].FileSize := StrToInt(AutoBin[1]);
        FPConfig.Download[Channel].FileCRC := StrToInt(AutoBin[2]);
        FPConfig.Download[Channel].FileName := AutoBin[4];
        FPConfig.Download[Channel].TempFileName := GetTempFileName(FPConfig.DirectoryAutoBin, 'part');

        FPConfig.Download[Channel].AutoBin := True;
      end
      else
        SendStringCommand(Channel, 0, '#ABORT#')
    end;
    'OK': // Got OK, we can send the file
    begin
      if FPConfig.Upload[Channel].Enabled then
        if MIEnableTNC.Checked then
        begin
          Hostmode.SendFile(Channel);
          FPConfig.Upload[Channel].Enabled := False;
        end;
    end;
  end;
end;

{
  StoreMail

  Check if "Data" in "Channel" is a mail message. If it's so, store it
  for later reading
}
procedure TFMain.StoreMail(const Channel: Byte; const Data: AnsiString);
var Regex: TRegExpr;
    AText, FName: AnsiString;
begin
  if (Length(Data) = 0) or (Channel = 0) then
    Exit;

  AText := RemoveNonPrintable(Data);

  // For OpenBCM BBS
  Regex := TRegExpr.Create;
  Regex.Expression := '^(\S+).*>.*(\S+).*(\d{2}\.\d{2}\.\d{2}) (\d{2}:\d{2}z) (\d+) Lines (\d+) Bytes.*';
  Regex.ModifierI := True;

  if Regex.Exec(AText) then
  begin
    // if the download is already enabled, then the prev download
    // has lesser lines as the header said. maybe the file is unfinished.
    // We move that one and start the new download.
    // The User can clean it up in the mail overview.
    if FPConfig.Download[Channel].Enabled then
    begin
      FName := FPConfig.DirectoryMail + DirectorySeparator + FPConfig.Download[Channel].FileName;
      RenameFile(FPConfig.Download[Channel].TempFileName, FName);
    end;

    FPConfig.Download[Channel] := FFileUpload.Default;
    FPConfig.Download[Channel].Enabled := True;
    FPConfig.Download[Channel].Mail := True;
    FPConfig.Download[Channel].FileSize := StrToInt(Regex.Match[6]);
    FPConfig.Download[Channel].Lines := StrToInt(Regex.Match[5]);
    FPConfig.Download[Channel].TempFileName := GetTempFileName(FPConfig.DirectoryMail, 'part');
    FPConfig.Download[Channel].OpenBCM := True;
    FPConfig.Download[Channel].LinBPQ := False;
    FPConfig.Download[Channel].FileName :=
      md5print(md5string(
        Regex.Match[1] +
        Regex.Match[2] +
        Regex.Match[3] +
        Regex.Match[4] +
        Regex.Match[5] +
        Regex.Match[6] +
        TimeToStr(Now)
      ));
    Exit;
  end;

  // For LinBPQ BBS
  if (Pos('From:', AText) > 0) and (not FPConfig.Download[Channel].OpenBCM) then
  begin
    // if the download is already enabled, then the prev download
    // has lesser lines as the header said. maybe the file is unfinished.
    // We move that one and start the new download.
    // The User can clean it up in the mail overview.
    if FPConfig.Download[Channel].Enabled then
    begin
      FName := FPConfig.DirectoryMail + DirectorySeparator + FPConfig.Download[Channel].FileName;
      RenameFile(FPConfig.Download[Channel].TempFileName, FName);
    end;

    FPConfig.Download[Channel] := FFileUpload.Default;
    FPConfig.Download[Channel].Enabled := True;
    FPConfig.Download[Channel].Mail := True;
    FPConfig.Download[Channel].TempFileName := GetTempFileName(FPConfig.DirectoryMail, 'part');
    FPConfig.Download[Channel].OpenBCM := False;
    FPConfig.Download[Channel].LinBPQ := True;
    FPConfig.Download[Channel].FileName :=
      md5print(md5string(
        TimeToStr(Now)
      ));
    Exit;
  end;
end;

{
  GetAPRSMessage

  Check if "Data" is an APRS Message. If it so, then split it to get the information.
}
procedure TFMain.GetAPRSMessage(const Data: String);
var Regex: TRegExpr;
    APRSMsg: String;
begin
  if (Length(Data) = 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    if MIEnableAGW.Checked then
    begin
      Regex.Expression := '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? <UI pid=F0.*(?:\[(\d{2}:\d{2}:\d{2})\]){1}(.*)';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
        if Regex.SubExprMatchCount >= 5 then
        begin
          APRSMsg := Regex.Match[1]+'|'+Regex.Match[2]+'|'+Regex.Match[3]+'|'+Regex.Match[5];
          APRSMsg := StringReplace(APRSMsg, #13, ' ', [rfReplaceAll]);
          WriteToPipe('flexpacketaprspipe', APRSMsg);
        end;
    end;

    if MIEnableTNC.Checked or MIEnableKISS.Checked then
    begin
      Regex.Expression := '^.*?fm\s(\S+)\sto\s(\S+)\s(?:via\s(.*))? ctl UI(?:(\S){1})? pid F0?';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
        if Regex.SubExprMatchCount >= 3 then
          APRSHeader := Regex.Match[1]+'|'+Regex.Match[2]+'|'+Regex.Match[3];

      Regex.Expression := '^([!=\/@;#*)_:>]{1})(.*)$';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
      begin
        APRSMsg := APRSHeader + '|' + Data;
        APRSMsg := StringReplace(APRSMsg, #13, ' ', [rfReplaceAll]);
        WriteToPipe('flexpacketaprspipe', APRSMsg);
        APRSHeader := '';
      end;
    end;

  finally
    Regex.Free;
  end;
end;

procedure TFMain.actGetBayComPasswordExecute(Sender: TObject);
var Callsign, Pass, Password: String;
    i: Integer;

begin
  Pass := FPConfig.BayCom[CurrentChannel];

  if Length(Pass) > 0 then
  begin
    i := FPConfig.DestCallsign[CurrentChannel].Count;
    Callsign := FPConfig.DestCallsign[CurrentChannel][i-1];
    Password := TFAdressbook.GetPassword(Callsign, Pass);
    if Length(Password) > 0 then
    begin
      FPConfig.MTx[CurrentChannel].Lines.Add(Password);
      SendStringCommand(CurrentChannel,0,Password)
    end;
  end;

end;

procedure TFMain.actHamradiotechExecute(Sender: TObject);
begin
  if not OpenURL('https://www.hamradiotech.de') then
    ShowMessage('Could not open URL: https://www.hamradiotech.de');
end;

{
  ShowHideClick

  Try Icon Menu to show and hide the PR Window
}
procedure TFMain.actMainShowHideExecute(Sender: TObject);
begin
  if FMain.WindowState = wsMinimized then
  begin
    FMain.WindowState := wsNormal;
    FMain.Show
  end
  else
  begin
    FMain.WindowState := wsMinimized;
    FMain.Hide;
  end;
end;

procedure TFMain.actListMailsExecute(Sender: TObject);
begin
  FListMails.SetConfig(@FPConfig);
  FListMails.Show;
end;

procedure TFMain.actOpenConversExecute(Sender: TObject);
begin
  TFConvers.SetConfig(@FPConfig);
  TFConvers.Show;
end;

procedure TFMain.actQuickConnectExecute(Sender: TObject);
var Callsign: String;
    i, Channel: Byte;
begin
  Channel := CurrentChannel;

  // search next free channel if the current channel is 0
  if Channel = 0 then
  begin
    for i := 1 to FPConfig.MaxChannels do
      if not FPConfig.Connected[i] then
      begin
         Channel := i;
         break;
      end
  end;

  if Channel = 0 then
  begin
    ShowMessage('No quickconnect at the monitoring channel.');
    Exit;
  end;

  Callsign := TFAdressbook.GetCallsign;
  if Length(Callsign) > 0 then
  begin
    if MIEnableTNC.Checked or MIEnableKISS.Checked  then
      SendStringCommand(Channel, 1, 'C ' + Callsign);
    if MIEnableAGW.Checked then
      SendStringCommand(Channel, 1, 'c ' + Callsign)
  end;
  TFAdressbook.Close;
end;

procedure TFMain.actToggleIconSizeExecute(Sender: TObject);
begin
  MIToolbarSize.Checked := not MIToolbarSize.Checked;
  FPConfig.TerminalToolbarBig := MIToolbarSize.Checked;

  SetIconSize(MIToolbarSize.Checked);
  ResizeForm(Sender);
end;

procedure TFMain.SetIconSize(const big: Boolean);
var i: Integer;
    Ctrl: TControl;
begin
  if big then
  begin
    Toolbar1.Height := 47;
    Toolbar1.ImagesWidth := 45;
    Toolbar1.Images := ILImagesBig;
    for i := 0 to Toolbar1.ControlCount - 1 do
    begin
      Ctrl := Toolbar1.Controls[i];

      if Ctrl is TToolButton then
      begin
        Ctrl.Width := 45;
        Ctrl.Height := 45;
      end;
    end;
  end
  else
  begin
    Toolbar1.Images := ILImagesSmall;
    Toolbar1.Height := 26;
    Toolbar1.ImagesWidth := 18;
    for i := 0 to Toolbar1.ControlCount - 1 do
    begin
      Ctrl := Toolbar1.Controls[i];

      if Ctrl is TToolButton then
      begin
        Ctrl.Width := 26;
        Ctrl.Height := 26;
      end;
    end;
  end;
end;

procedure TFMain.GetBayCom(const Channel: Byte; const Data: String);
var Regex: TRegExpr;
begin
  if (Length(Data) = 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*(\d{1} \d{1} \d{1} \d{1} \d{1})';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      FPConfig.BayCom[Channel] := Regex.Match[1];
  finally
    Regex.Free;
  end;
end;

{
  CheckConnected

  Check if the Data string of the Channel is a connection message.
  If it so, add the Callsign to the DestCallsign list.
}
procedure TFMain.CheckConnected(const Channel: Byte; const Data: String);
var Regex: TRegExpr;
begin
  if (Length(Data) = 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*Connected to (?:[A-Z]{0,6}\:)?([A-Z0-9]{1,6}-[0-9]).*';
    Regex.ModifierI := True;
    if Regex.Exec(Data) then
    begin
      if not Assigned(FPConfig.DestCallsign[Channel]) then
        FPConfig.DestCallsign[Channel] := TStringList.Create;
      SetChannelButtonLabel(Channel,Trim(Regex.Match[1]));
      FPConfig.DestCallsign[Channel].Add(Trim(Regex.Match[1]));
    end;
  finally
    Regex.Free;
  end;
end;

{
  CheckDisconnected

  Check if the Data string of the Channel is a disconnection message.
  If it so, remove the last Callsign of DestCallsign.
}
procedure TFMain.CheckDisconnected(const Channel: Byte; const Data: String);
var Regex: TRegExpr;
    i: Integer;
begin
  if (Length(Data) = 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '^.*Disconnected from (?:[A-Z]{0,6}\:)?([A-Z0-9]{1,6}-[0-9]).*';
    Regex.ModifierI := True;
    if Regex.Exec(Data) then
    begin
      // delete the last one
      i := FPConfig.DestCallsign[CurrentChannel].Count;
      if (i - 1) > 0 then
      begin
        SetChannelButtonLabel(Channel,Trim(FPConfig.DestCallsign[Channel][i-2]));
        FPConfig.DestCallsign[Channel].Delete(i-1);
      end;
    end;
  finally
    Regex.Free;
  end;
end;

procedure TFMain.GetStatus(const Channel: Byte);
var Status: TStatusLine;
begin
  // 0 = Number of link status messages not yet displayed)
  // 1 = Number of receive frames not yet displayed
  // 2 = Number of send frames not yet transmitted
  // 3 = Number of transmitted frames not yet acknowledged
  // 4 = Number of tries on current operation
  // 5 = Link state
  // 6 = Status Text (CONNECTED, DISCONNECTED, etc
  // 7 = The CALL of the other station
  // 8 = call of the digipeater
  // 9 = Free Status Text

  Status := Default(TStatusLine);

  if MIEnableTNC.Checked or MIEnableKISS.Checked then
  begin
    if MIEnableTNC.Checked then
      Status := Hostmode.ChannelStatus[Channel]
    else
      Status := KISSmode.ChannelStatus[Channel];

    SBStatus.Panels[1].Text := Status[9];
    SBStatus.Panels[2].Text := 'UnDisp: ' + Status[0];
    SBStatus.Panels[3].Text := 'UnSent: ' + Status[2];
    SBStatus.Panels[4].Text := 'UnAck: ' + Status[3];
    SBStatus.Panels[5].Text := 'Retry: ' + Status[4];

    SBStatus.Repaint;
  end;

  if MIEnableAGW.Checked then
    Status := AGWClient.ChannelStatus[Channel];

  if (Status[6] = 'DISCONNECTED') or (Status[5] = Chr(0)) then
    SetChannelButtonLabel(Channel,'Disc');
end;

end.


