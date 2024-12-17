unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, ActnList, RichMemo, LazSerial, uhostmode,
  umycallsign, utnc, uansi, utypes, uinfo, uterminalsettings, uresize, uini,
  uaddressbook, uagwpeclient, uagw, ufileupload, System.UITypes, u7plus,
  LCLIntf, RegExpr, Process, upipes, LCLType;

type


  { TFMain }

  TFMain = class(TForm)
    AOpenAddressbook: TAction;
    ALShortCuts: TActionList;
    ILImages: TImageList;
    Image2: TImage;
    MainMenuItemFile: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MenuItem1: TMenuItem;
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
    PMTrayIcon: TPopupMenu;
    SBStatus: TStatusBar;
    Separator1: TMenuItem;
    Separator2: TMenuItem;
    TBMap: TToolButton;
    TMain: TTimer;
    ToolBar1: TToolBar;
    TBAdressbook: TToolButton;
    TBFormular: TToolButton;
    TBFileUpload: TToolButton;
    TB7Plus: TToolButton;
    TrayIcon: TTrayIcon;
    procedure AOpenAddressbookExecute(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure BtnReInitTNCOnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure EnableTNCClick(Sender: TObject);
    procedure EnableAGWClick(Sender: TObject);
    procedure MIShowHideClick(Sender: TObject);
    procedure MIGet7PlusClick(Sender: TObject);
    procedure MIAGWSettingsClick(Sender: TObject);
    procedure OpenTerminalSettings(Sender: TObject);
    procedure ResizeForm(Sender: TObject);
    procedure Restart(Sender: TObject);
    procedure ShowInfo(Sender: TObject);
    procedure MMenuExitOnClick(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
  private
    procedure ShowChannelMemo(const channel: byte);
    procedure ShowMTxMemo(const channel: byte);
    procedure ShowPTxPanel(const channel: byte);
    procedure SetChannelButtonBold(const Channel: Byte);
    procedure AddTextToMemo(Const Channel: Byte; const Data: string);
    procedure BBChannelClick(Sender: TObject);
    Procedure UploadFile(Sender: TObject);
    procedure QuickConnect(Sender: TObject);
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    procedure SendStringCommand(const Channel, Code: byte; const Command: string);
    procedure GetStatus(const Channel: Byte);
    procedure GetAutoBin(const Channel: Byte; const Data: String);
    procedure GetAPRSMessage(const Data: String);
    function ReadChannelBuffer(const Channel: byte):string;
    function ReadDataBuffer(const Channel: Byte):TBytes;
  public

  end;

var
  FMain: TFMain;
  Hostmode: THostmode;
  AGWClient: TAGWPEClient;
  FPConfig: TFPConfig;
  CurrentChannel: byte;
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
    FPConfig.MTx[i].Visible := False;

  FPConfig.MTx[channel].Visible := True;
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

  FPConfig.MaxChannels := 10;

  LoadConfigFromFile(@FPConfig);

  FontSize := 11;
  if FPConfig.TerminalFontSize > 0 then
    FontSize := FPConfig.TerminalFontSize;

  // init channel TRichMemo
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i] := TRichMemo.Create(Self);
    FPConfig.Channel[i].Parent := FMain;
    FPConfig.Channel[i].Left := 4;
    FPConfig.Channel[i].Top := 155;
    FPConfig.Channel[i].Width := FMain.Width - 8;
    FPConfig.Channel[i].Height := FMain.Height - SBStatus.Height - 231;
    FPConfig.Channel[i].Font.Color := clWhite;
    FPConfig.Channel[i].Font.Pitch := fpFixed;
    FPConfig.Channel[i].Font.Name := 'Courier New';
    FPConfig.Channel[i].Font.Style := [fsBold];
    FPConfig.Channel[i].Font.Size := FontSize;
    FPConfig.Channel[i].Color := clBlack;
    FPConfig.Channel[i].Rtf := '';
    FPConfig.Channel[i].Visible := False;
    FPConfig.Channel[i].ReadOnly := True;
    FPConfig.Channel[i].ScrollBars := ssAutoVertical;
    FPConfig.Channel[i].Anchors := [akLeft,akRight,akTop];

    FPConfig.Connected[i] := False;
    FPConfig.Download[i].Enabled := False;
  end;

  // workaround!!
  // When TRichMemo receives text for the first time, the TRichMemo is displayed
  // automatically even if Visible=False. This behavior is annoying during
  // operation. Therefore we force this behavior when starting the software so
  // that it does not annoy us later.
  for i := FPConfig.MaxChannels downto 0 do
    AddTextToMemo(i, #13);

  // change some parameters only for the monitor
  FPConfig.Channel[0].Font.Color := clGreen;
  FPConfig.Channel[0].Color := clWhite;

  // init MTx Memo
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.MTx[i] := TMemo.Create(Self);
    FPConfig.MTx[i].Parent := FMain;
    FPConfig.MTx[i].Left := 4;
    FPConfig.MTx[i].Top := FPConfig.Channel[i].Height + FPConfig.Channel[i].Top + 5;
    FPConfig.MTx[i].Width := FMain.Width - 8;
    FPConfig.MTx[i].Height := 67;
    FPConfig.MTx[i].Font.Color := clBlack;
    FPConfig.MTx[i].Font.Pitch := fpFixed;
    FPConfig.MTx[i].Font.Name := 'Courier New';
    FPConfig.MTx[i].Font.Size := 10;
    FPConfig.MTx[i].Color := clDefault;
    FPConfig.MTx[i].Text := '';
    FPConfig.MTx[i].Visible := False;
    FPConfig.MTx[i].ScrollBars := ssAutoVertical;
    FPConfig.MTx[i].Anchors := [akLeft,akRight,akBottom];
    FPConfig.MTx[i].OnKeyPress := @SendCommand;
  end;

  // init PTx Panel (visual Command indication)
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.PTx[i] := TPanel.Create(Self);
    FPConfig.PTx[i].Parent := FMain;
    FPConfig.PTx[i].Left := 4;
    FPConfig.PTx[i].Top := FPConfig.Channel[i].Height + FPConfig.Channel[i].Top + 2;
    FPConfig.PTx[i].Width := FMain.Width - 8;
    FPConfig.PTx[i].Height := 2;
    FPConfig.PTx[i].Color := clDefault;
    FPConfig.PTx[i].Visible := False;
    FPConfig.PTx[i].BevelOuter := bvNone;
    FPConfig.PTx[i].BevelInner := bvRaised;
    FPConfig.PTx[i].BevelColor := clForm;
    FPConfig.PTx[i].Anchors := [akLeft,akRight,akBottom];

    FPConfig.IsCommand[i] := False;
  end;

  MIEnableTNC.Checked := FPConfig.EnableTNC;
  MIEnableAGW.Checked := FPConfig.EnableAGW;


  Hostmode := nil;
  AGWClient := nil;
  if MIEnableTNC.Checked then
  begin
    MIEnableTNC.Checked := True;
    Hostmode := THostmode.Create(@FPConfig);
    Hostmode.Start;
    Hostmode.OnTerminate := @HostmodeThreadTerminated;
  end;

  if MIEnableAGW.Checked then
  begin
    MIEnableAGW.Checked := True;
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
    BBChannel[i].Top := 66;
    BBChannel[i].Height := 48;
    BBChannel[i].Width := 56;
    BBChannel[i].Caption := '&' + IntToStr(i);
    BBChannel[i].onClick := @BBChannelClick;
    BBChannel[i].Name := 'BBChannel'+IntToStr(i);

    nextBtnLeft := nextBtnLeft + BBChannel[i].Width + 5;

    LMChannel[i] := TLabel.Create(Self);
    LMChannel[i].Parent := FMain;
    LMChannel[i].Top := 120;
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
  ShowMessage('Unknown Serial Communication Error');
  Hostmode := THostmode.Create(@FPConfig);
end;

procedure TFMain.AGWThreadTerminated(Sender: TObject);
begin
  ShowMessage('Unknown AGW Communication Error');
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
    Hostmode.LoadTNCInit;
    Hostmode.SetCallsign;
  end;
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
  FPConfig.EnableTNC := True;
  FPConfig.EnableAGW := False;
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
  FPConfig.EnableAGW := True;
  TBFileUpload.Enabled := False;
  SaveConfigToFile(@FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
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
  MIGet7PlusClick

  Open then URL where the user can download 7plus
}
procedure TFMain.MIGet7PlusClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/7plus') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/7plus');
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
var
  scaleFactorWidth, scaleFactorHeight, scaleFactor: Double;
  i: Integer;
begin
  scaleFactorWidth := Width / OrigWidth;
  scaleFactorHeight := Height / OrigHeight;
  scaleFactor := Min(scaleFactorWidth, scaleFactorHeight);

  for i := 0 to ControlCount - 1 do
    ResizeControl(Controls[i], scaleFactorWidth, scaleFactorHeight, scaleFactor);
end;

{
  Restart

  Action procedure for the Restart button in the Main Menu.
}
procedure TFMain.Restart(Sender: TObject);
begin
  RestartApplication;
end;


procedure TFMain.ShowInfo(Sender: TObject);
begin
  TFInfo.Show;
end;

{
  MMenuExitOnClick

  Action procedure for the Exit button in the Main Menu.
}
procedure TFMain.MMenuExitOnClick(Sender: TObject);
begin
  if MIEnableTNC.Checked then
  begin
    Hostmode.Terminate;
    Hostmode := nil;
  end;
  Close;
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


procedure TFMain.FormDestroy(Sender: TObject);
begin
  try
    ClosePipe('flexpacketaprspipe');

    if MIEnableTNC.Checked then
    begin
      Hostmode.Connected := False;
      Hostmode.Terminate;
    end;
    SaveConfigToFile(@FPConfig);
  finally
  end;
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
  QuickConnect

  Callback procedure for the QuickConnect button (uadressbook.pas).
}
procedure TFMain.QuickConnect(Sender: TObject);
var Callsign: String;
begin
  if CurrentChannel = 0 then
  begin
    ShowMessage('No quickconnect at the monitoring channel.');
    Exit;
  end;

  Callsign := TFAdressbook.GetCallsign;
  if Length(Callsign) > 0 then
  begin
    if MIEnableTNC.Checked then
      SendStringCommand(CurrentChannel, 1, 'C ' + Callsign);
    if MIEnableAGW.Checked then
    SendStringCommand(CurrentChannel, 1, 'c ' + Callsign)
  end;
end;

{
  TBAdressbookClick

  Toolbarbutton to open the Addressbook. The the QuickConnect property for
  the quickconnect button.
}
procedure TFMain.TBAdressbookClick(Sender: TObject);
begin
  TFAdressbook.OnQuickConnect := @QuickConnect;
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
    FFileUpload.SetFilename(ODFileUpload.FileName);
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
    Data: string;
begin
  if MIEnableTNC.Checked then
    if not Hostmode.Connected then
      Exit;

  if MIEnableAGW.Checked then
    if not AGWClient.Connected then
      Exit;

  for i:= 0 to FPConfig.MaxChannels do
  begin
    // handle status information except monitor channel
    if i > 0 then
      GetStatus(i);

    // if upload is activated for this channel, download the file.
    if (i > 0) and (FPConfig.Download[i].Enabled) then
      FFileUpload.FileDownload(ReadDataBuffer(i), i);

    // Read data from channel buffer
    Data := ReadChannelBuffer(i);

    // handle autobin messages
    GetAutoBin(i, Data);

    // handle aprs messages. APRS Messages can only be at the Monitoring Channel.
    GetAPRSMessage(Data);

    if (Length(Data) > 0) then
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
procedure TFMain.AddTextToMemo(const Channel: Byte; const Data: String);
var Segments: uansi.TGraphicArray;
    Memo: TRichMemo;
begin
  Memo := FPConfig.Channel[Channel];
  Segments := uansi.ApplyANSIColor(Data, Memo.Font.Color);
  uansi.DisplayANSITextInMemo(Memo, Segments);
  if (Memo.Visible) and (Channel = CurrentChannel) then
  begin
    Memo.SelStart := Memo.GetTextLen;
    Memo.ScrollBy(0, Memo.Lines.Count);
    Memo.Refresh;
  end;
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
begin
  case Code of
    1: AddTextToMemo(Channel, #27'[96m' + Command + #13#27'[0m');
    0: AddTextToMemo(Channel, #27'[32m' + Command + #13#27'[0m');
  end;

  if (MIEnableTNC.Checked) and (Length(Command) > 0) then
    Hostmode.SendStringCommand(Channel, Code, Command);

  if (MIEnableAGW.Checked) and (Length(Command) > 0) then
    AGWClient.SendStringCommand(0, Code, Command);
end;

{
  ReadChannelBuffer

  Return the String of "Channel". These Buffer will hold all AX25 ASCII data
  that we want to display.
}
function TFMain.ReadChannelBuffer(const Channel: Byte):String;
begin
  Result := '';

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
  GetAPRSMessage

  Check if "Data" is an APRS Message. If it so, then split it to get the information.
}
procedure TFMain.GetAPRSMessage(const Data: String);
var
  Regex: TRegExpr;
  APRSMsg: String;
begin
  if (Length(Data) = 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    if FPConfig.EnableAGW then
    begin
      Regex.Expression := '^.*?Fm\s(\S+)\sTo\s(\S+)\s(?:Via\s(\S+))? <UI pid=F0.*';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
        WriteToPipe('flexpacketaprspipe', Data);
    end;

    if FPConfig.EnableTNC then
    begin
      Regex.Expression := '^.*?fm\s(\S+)\sto\s(\S+)\s(?:via\s(\S+))? ctl UIv pid F0?';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
        APRSHeader := Data;

      Regex.Expression := '^.*([!|=|\/@|z|h]{1})(\d{4}\.\d{2}[N|S])\/(\d{5}\.\d{2}[E|W])(\w)(.+).*$';
      Regex.ModifierI := False;
      if Regex.Exec(Data) then
      begin
        APRSMsg := APRSHeader + ' ' + Data;
        WriteToPipe('flexpacketaprspipe', StringReplace(APRSMsg, #13, '', [rfReplaceAll]));
        APRSHeader := '';
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

  Status := Default(TStatusLine);

  if MIEnableTNC.Checked then
  begin
    Status := Hostmode.ChannelStatus[Channel];
    SBStatus.Panels[1].Text := 'UnDisp: ' + Status[0];
    SBStatus.Panels[2].Text := 'UnSent: ' + Status[2];
    SBStatus.Panels[3].Text := 'UnAck: ' + Status[3];
    SBStatus.Panels[4].Text := 'Retry: ' + Status[4];
    SBStatus.Repaint;
  end;

  if MIEnableAGW.Checked then
    Status := AGWClient.ChannelStatus[Channel];

  if Status[6] = 'CONNECTED' then
    SetChannelButtonLabel(Channel,Status[7]);

  if (Status[6] = 'DISCONNECTED') or (Status[5] = Chr(0)) then
    SetChannelButtonLabel(Channel,'Disc');
end;

end.


