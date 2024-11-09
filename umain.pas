unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  StdCtrls, Buttons, ExtCtrls, RichMemo, uhostmode, umycallsign,
  utnc, uansi, utypes, uinfo, uterminalsettings, uresize, uini, uaddressbook;

type


  { TFMain }

  TFMain = class(TForm)
    ILImages: TImageList;
    Image1: TImage;
    MainMenuItemFile: TMenuItem;
    MainMenuItemSettings: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MITNC: TMenuItem;
    MISettings: TMenuItem;
    MMainMenu: TMainMenu;
    MTx: TMemo;
    PTx: TPanel;
    SBStatus: TStatusBar;
    TMain: TTimer;
    ToolBar1: TToolBar;
    TBPacketRadio: TToolButton;
    TBAdressbook: TToolButton;
    TBFormular: TToolButton;
    procedure BtnSendClick(Sender: TObject);
    procedure FMainInit(Sender: TObject);
    procedure BtnReInitTNCOnClick(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure OpenTerminalSettings(Sender: TObject);
    procedure ResizeForm(Sender: TObject);
    procedure ShowInfo(Sender: TObject);
    procedure MMenuExitOnClick(Sender: TObject);
    procedure OpenTNCSettings(Sender: TObject);
    procedure OpenMyCallsign(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure TBAdressbookClick(Sender: TObject);
    procedure TMainTimer(Sender: TObject);
    procedure SetChannelButtonLabel(channel: byte; LabCap: string);
  private
    procedure ShowChannelMemo(channel: byte);
    procedure SetChannelButtonBold(channel: byte);
    procedure AddTextToMemo(Memo: TRichMemo; Data: string);
    procedure SetToolButtonDown(Sender: TObject);
    procedure BBChannelClick(Sender: TObject);
    function Min(a, b: Double): Double;
  public

  end;

var
  FMain: TFMain;
  Hostmode: THostmode;
  FPConfig: TFPConfig;
  CurrentChannel: byte;
  IsCommand: Boolean;
  HomeDir: string;
  OrigWidth, OrigHeight: Integer;
  BBChannel: TBChannel;
  LMChannel: TLChannel;


implementation

{$R *.lfm}


{ TFMain }

procedure TFMain.SetChannelButtonBold(channel: byte);
var i: Byte;
    Btn: TBitBtn;
begin
  for i := 0 to FPConfig.MaxChannels do
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
  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i].Visible := False;
  end;
  FPConfig.Channel[channel].Visible := True;
  MTx.Visible := True;
end;

procedure TFMain.FMainInit(Sender: TObject);
var i: Byte;
    FontSize, nextBtnLeft, nextLabelLeft: Integer;
begin
  Self.Width := 1137;
  Self.Height := 716;

  OrigWidth := Self.Width;
  OrigHeight := Self.Height;

  FontSize := 11;
  if FPConfig.TerminalFontSize > 0 then
    FontSize := FPConfig.TerminalFontSize;

  LoadConfigFromFile(@FPConfig);

  for i := 0 to FPConfig.MaxChannels do
  begin
    FPConfig.Channel[i] := TRichMemo.Create(Self);
    FPConfig.Channel[i].Parent := FMain;
    FPConfig.Channel[i].Left := 4;
    FPConfig.Channel[i].Top := 155;
    FPConfig.Channel[i].Width := FMain.Width - 8;
    FPConfig.Channel[i].Height := FMain.Height - SBStatus.Height - 233;
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
    FPConfig.Channel[i].Anchors := [akLeft,akRight,akBottom];

    // set the channel to be inactive and not connected
    FPConfig.Active[i] := False;
    FPConfig.Connected[i] := False;
  end;

  // change some parameters only for the monitor
  FPConfig.Channel[0].Font.Color := clGreen;
  FPConfig.Channel[0].Color := clWhite;
  // set the monitor channel to be active
  FPConfig.Active[0] := True;

  Hostmode := THostmode.Create(@FPConfig);

  nextBtnLeft := 0;
  nextLabelLeft := 0;
  for i := 0 to FPConfig.MaxChannels do
  begin
    BBChannel[i] := TBitBtn.Create(Self);
    BBChannel[i].Parent := FMain;
    BBChannel[i].Left := 4 + nextBtnLeft;
    BBChannel[i].Top := 66;
    BBChannel[i].Height := 48;
    BBChannel[i].Width := 56;
    BBChannel[i].Caption := IntToStr(i);
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
  IsCommand := False;

  // Save size and possition of all elements to make window resize possible
  SetLength(ControlInfoList, 0);
  StoreOriginalSizes(Self);
end;

procedure TFMain.BBChannelClick(Sender: TObject);
var btn: TBitBtn;
begin
  if Sender is TBitBtn then
  begin
     btn := TBitBtn(Sender);
     CurrentChannel := StrToInt(btn.Caption);
     ShowChannelMemo(CurrentChannel);
     SetChannelButtonBold(CurrentChannel);
     SBStatus.Visible := True;
  end;
end;

procedure TFMain.BtnReInitTNCOnClick(Sender: TObject);
begin
  Hostmode.LoadTNCInit;
end;

procedure TFMain.FormPaint(Sender: TObject);
var i: Byte;
    Lab: TLabel;
    Btn: TBitBtn;
    TextWidth: Integer;
begin
  // because of a strange behavor, we have to recalculate the position
  // of the label after Form repaint
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

procedure TFMain.OpenTerminalSettings(Sender: TObject);
begin
  TFTerminalSettings.SetConfig(@FPConfig);
  TFTerminalSettings.Show;
end;

function TFMain.Min(a, b: Double): Double;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;


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


procedure TFMain.ShowInfo(Sender: TObject);
begin
  TFInfo.Show;
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
  SaveConfigToFile(@FPConfig);
  Hostmode.Terminate;
  Hostmode.WaitFor; // Warten, bis der Thread beendet ist
  FreeAndNil(Hostmode);
end;


procedure TFMain.SendCommand(Sender: TObject; var Key: char);
var y, x: Integer;
begin
  // set the channel active
  FPConfig.Active[CurrentChannel] := True;

  if key = #27 then
  begin
    if IsCommand then
    begin
      IsCommand := False;
      PTx.BevelColor := clForm;
    end
    else
    begin
      IsCommand := True;
      PTx.BevelColor := clRed;
    end;
  end;
  if key = #13 then
  begin
    y := CurrentChannel;
    x := MTx.CaretPos.Y; // current cursor position
    if Length(MTx.Lines[x]) > 0 then
    begin
      if IsCommand then
      begin
        AddTextToMemo(FPConfig.Channel[y], #27'[96m' + MTx.Lines[x] + #13#27'[0m');
        Hostmode.SendByteCommand(y,1,MTx.Lines[x])
      end
      else
      begin
        AddTextToMemo(FPConfig.Channel[y], #27'[32m' + MTx.Lines[x] + #13#27'[0m');
        Hostmode.SendByteCommand(y,0,MTx.Lines[x]);
      end;
    end;
    IsCommand := False;
    PTx.BevelColor := clForm;
  end;
end;

procedure TFMain.TBAdressbookClick(Sender: TObject);
begin
  TFAdressbook.Show;
end;

procedure TFMain.SetToolButtonDown(Sender: TObject);
var
  i: Integer;
  ClickedButton: TToolButton;
  ToolBar: TToolBar;
begin
  if not (Sender is TToolButton) then Exit;

  ClickedButton := TToolButton(Sender);
  ToolBar := TToolBar(ClickedButton.Parent);

  for i := 0 to ToolBar.ButtonCount - 1 do
  begin
    if ToolBar.Buttons[i] = ClickedButton then
      ToolBar.Buttons[i].Down := True
    else
      ToolBar.Buttons[i].Down := False;
  end;
end;

procedure TFMain.TMainTimer(Sender: TObject);
var i: Integer;
    Data: string;
    Status: TStatusLine;
begin
  for i:= 0 to FPConfig.MaxChannels do
  begin
    Data := '';
    Data := Hostmode.ReadChannelBuffer(i);
    if (Length(Data) > 0) then
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
    // 6 = Status Text CONNECTED, DISCONNECTED, etc
    // 7 = The CALL of the other station
    // 8 = call of the digipeater

    SBStatus.Panels[1].Text := 'UnDisp: ' + Status[0];
    SBStatus.Panels[2].Text := 'UnSent: ' + Status[2];
    SBStatus.Panels[3].Text := 'UnAck: ' + Status[3];
    SBStatus.Panels[4].Text := 'Retry: ' + Status[4];
    SBStatus.Repaint;

    if Status[6] = 'CONNECTED' then
      SetChannelButtonLabel(i,Status[7]);

    if Status[6] = 'DISCONNECTED' then
      SetChannelButtonLabel(i,'Disc');
  end;
end;

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

procedure TFMain.AddTextToMemo(Memo: TRichMemo; Data: string);
var Segments: uansi.TGraphicArray;
begin
  Segments := uansi.ApplyANSIColor(Data, Memo.Font.Color);
  uansi.DisplayANSITextInMemo(Memo, Segments);
  if Memo.Visible then
  begin
    Memo.SelStart := Memo.GetTextLen;
    Memo.ScrollBy(0, Memo.Lines.Count);
    Memo.Refresh;
  end;
end;

end.


