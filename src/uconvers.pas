unit uconvers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, Menus, ExtCtrls, PairSplitter, ColorBox, utypes, uCmdBoxCustom,
  uCmdBox, uAddressbook, RegExpr;

type

  { TTFConvers }

  TTFConvers = class(TForm)
    actClose: TAction;
    actConnect: TAction;
    actDisconnect: TAction;
    ActionList1: TActionList;
    lbCallsigns: TListBox;
    MenuItem1: TMenuItem;
    PairSplitter1: TPairSplitter;
    PairSplitter2: TPairSplitter;
    PairSplitterSide2: TPairSplitterSide;
    PMConnect: TPopupMenu;
    PSSChannel: TPairSplitterSide;
    PSSMTx: TPairSplitterSide;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    ToolBar1: TToolBar;
    TBConnect: TToolButton;
    ToolButton1: TToolButton;
    ToolButton7: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PMConnectOnClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    ChatWindow: TCmdBoxCustom;
    MessageWindow: TMemo;
    Message: Boolean;
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure AddBuddies(const Callsign: AnsiString);
    procedure CheckLeft(const Data: AnsiString);
    procedure CheckJoined(const Data: AnsiString);
    function Colorerize(const Data: AnsiString): AnsiString;
    function GetCallsign(const Data: AnsiString): AnsiString;
    function IsInBuddieList(Callsign: AnsiString): Boolean;
    function GetUsername(const Data: AnsiString): AnsiString;
  public
    procedure SetConfig(Config: PTFPConfig);
    function Convers(const Data: AnsiString): AnsiString;
  end;

var
  TFConvers: TTFConvers;
  FPConfig: PTFPConfig;

implementation

uses umain;

{$R *.lfm}

procedure TTFConvers.FormShow(Sender: TObject);
begin
  FMain.SetChannelButtonLabel(FPConfig^.MaxChannels,'Convers');
  // Get Convers Channel
  if Assigned(FPConfig^.Channel[FPConfig^.MaxChannels]) then
  begin
    ChatWindow := FPConfig^.Channel[FPConfig^.MaxChannels];
    ChatWindow.Parent := PairSplitterSide2;
    ChatWindow.Left := 4;
    ChatWindow.Enabled := True;
    ChatWindow.Visible := True;
    ChatWindow.Align := alClient;
    ChatWindow.BackGroundColor := FPConfig^.ConversBGColor;
    ChatWindow.TextBackground(FPConfig^.ConversBGColor);
    ChatWindow.TextColor(FPConfig^.ConversFontColor);
    ChatWindow.Font.Size := FPConfig^.ConversFontSize;
    ChatWindow.Font.Name := FPConfig^.ConversFontName;
    FPConfig^.IsConvers[FPConfig^.MaxChannels] := True;
  end;

  if Assigned(FPConfig^.MTx[FPConfig^.MaxChannels]) then
  begin
    MessageWindow := FPConfig^.MTx[FPConfig^.MaxChannels];
    MessageWindow.Parent := PSSMTx;
    MessageWindow.Left := 4;
    MessageWindow.Enabled := True;
    MessageWindow.Visible := True;
    MessageWindow.Align := alClient;
    MessageWindow.OnKeyPress := @SendCommand;
  end;

  // If a message was send by a user, it will be true
  Message := False;

  if (FPConfig^.ConversX > 0) and (FPConfig^.ConversY > 0) then
  begin
    Left := FPConfig^.ConversX;
    Top := FPConfig^.ConversY;
  end;
end;

procedure TTFConvers.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

procedure TTFConvers.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TTFConvers.actConnectExecute(Sender: TObject);
var Pt: TPoint;
    Item: TMenuItem;
    i: Integer;
    Callsigns: TStringList;
begin
  PMConnect.Items.Clear;

  Callsigns := TFAdressbook.GetAllCallsigns('where type = "Convers"');

  for i := 0 to Callsigns.Count - 1 do
  begin
    Item := TMenuItem.Create(PMConnect);
    Item.Caption := Callsigns[i];
    Item.OnClick := @FMain.actQuickConnectExecute;
    PMConnect.Items.Add(Item);
  end;

  Pt := TBConnect.ClientToScreen(Point(0, TBConnect.Height));
  PMConnect.PopUp(Pt.X, Pt.Y);
end;

procedure TTFConvers.actDisconnectExecute(Sender: TObject);
begin
  FMain.SendStringCommand(FPConfig^.MaxChannels, 1, 'D');
end;

procedure TTFConvers.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FMain.SetChannelButtonLabel(FPConfig^.MaxChannels,'Disc');
  FPConfig^.IsConvers[FPConfig^.MaxChannels] := False;
  FPConfig^.ConversX := Left;
  FPConfig^.ConversY := Top;
  MessageWindow.Parent := FMain.PSSMTx;
  ChatWindow.Parent := FMain.PSSChannel;
  ChatWindow.BackGroundColor := FPConfig^.TerminalBGColor;
  ChatWindow.TextBackground(FPConfig^.TerminalBGColor);
  ChatWindow.Font.Size := FPConfig^.TerminalFontSize;
  ChatWindow.Font.Color := FPConfig^.TerminalFontColor;
end;


procedure TTFConvers.FormResize(Sender: TObject);
begin
  PairSplitter2.Position := TFConvers.Width - 124;
end;

procedure TTFConvers.PMConnectOnClick(Sender: TObject);
var btn: TMenuItem;
    Callsign: String;
begin
  if Sender is TMenuItem then
  begin
    btn := TMenuItem(Sender);
    Callsign := btn.Caption;

    if (FPConfig^.Connected[FPConfig^.MaxChannels]) then
      Exit;

    if Length(Callsign) > 0 then
    begin
      if FPConfig^.EnableTNC then
        FMain.SendStringCommand(FPConfig^.MaxChannels, 1, 'C ' + Callsign);
      if FPConfig^.EnableAGW then
        FMain.SendStringCommand(FPConfig^.MaxChannels, 1, 'c ' + Callsign);
    end;
  end;
end;

procedure TTFConvers.Timer1Timer(Sender: TObject);
begin
  if not Assigned(FPConfig) then
    Exit;

  // keepalive
  try
    if FPConfig^.Connected[FPConfig^.MaxChannels] then
      FMain.SendStringCommand(FPConfig^.MaxChannels,0,#0);
  except
  end;
end;

procedure TTFConvers.SendCommand(Sender: TObject; var Key: char);
var y, x: Integer;
begin
  if key = #13 then
  begin
    y := FPConfig^.MaxChannels;
    x := FPConfig^.MTx[y].CaretPos.Y; // current cursor position
    if Length(FPConfig^.MTx[y].Lines[x]) > 0 then
      FMain.SendStringCommand(y,0,FPConfig^.MTx[y].Lines[x])
  end;
end;

function TTFConvers.Colorerize(const Data: AnsiString): AnsiString;
var Regex: TRegExpr;
    msg, clock, Callsign: AnsiString;
begin
  Result := Data;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '(?:(\d{2}\s*:\s*\d{2})\s+)?([A-Z0-9]+)\s*:\s*(.*)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 3 then
      begin
        clock := Regex.Match[1];
        callsign := Regex.Match[2];
        msg := Regex.Match[3];
        if clock = '' then
          Result := Format(#27'[36m %-7s : '#27'[0m%s', [callsign, msg])
        else
          Result := Format(#27'[36m %-5s %-7s : '#27'[0m%s', [clock, callsign, msg]);
      end;
  finally
    Regex.Free;
  end;

//  if Message then
//  begin
//    if Pos(#13, Data) > 0 then
//      Result := StringReplace(Result, #13, #13 + ESC+'[36m', [rfReplaceAll]);
//    if Pos(':', Data) > 0 then
//      Result := StringReplace(Result, ':', ESC+'[0m :', [rfReplaceAll]);
//  end;
end;

procedure TTFConvers.AddBuddies(const Callsign: AnsiString);
begin
  if (Length(Callsign) <= 0) then
    Exit;

  if Length(Callsign) > 0 then
    if not IsInBuddieList(Callsign) then
      lbCallsigns.Items.Add(Callsign);
end;

function TTFConvers.IsInBuddieList(Callsign: AnsiString): Boolean;
var i: Integer;
begin
  Result := False;
  for i:= 0 to lbCallsigns.Items.Count - 1 do
    if Pos(lbCallsigns.Items[i], Callsign) > 0 then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TTFConvers.CheckLeft(const Data: AnsiString);
var Callsign: AnsiString;
    i: Integer;
begin
  if (Length(Data) <= 0) then
    Exit;

  if Pos('*** Left', Data) > 0 then
  begin
    Callsign := GetCallsign(Data);
    if Length(Callsign) > 0 then
    begin
      for i:= 0 to lbCallsigns.Items.Count - 1 do
        if SameText(lbCallsigns.Items[i], Callsign) then
        begin
          lbCallsigns.Items.Delete(i);
          Exit;
        end;
    end;
  end;
end;

procedure TTFConvers.CheckJoined(const Data: AnsiString);
var Callsign: AnsiString;
    i: Integer;
begin
  if (Length(Data) <= 0) then
    Exit;

  if Pos('*** Joined', Data) > 0 then
  begin
    Callsign := GetCallsign(Data);
    if Length(Callsign) > 0 then
    begin
      for i:= 0 to lbCallsigns.Items.Count - 1 do
        if SameText(lbCallsigns.Items[i], Callsign) then
          Exit;
      AddBuddies(Format('%-7s (%s)',[Callsign, GetUsername(Data)]));
    end;
  end;
end;

function TTFConvers.GetUsername(const Data: AnsiString): AnsiString;
var Regex: TRegExpr;
begin
  Regex := TRegExpr.Create;
  try
    Regex.Expression := '.*\s:\s(.*)(?:\*\*\* Joined Chat).*';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 1 then
        Result := Trim(Regex.Match[1]);
  finally
    Regex.Free;
  end;
end;

function TTFConvers.GetCallsign(const Data: AnsiString): AnsiString;
var Regex: TRegExpr;
begin
  Result := '';

  if (Length(Data) <= 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '((?:\d{1})?[A-Z]{1,3}\d{1,3}[A-Z]{0,4}(?:-\d{1,2})?)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 1 then
        Result := Trim(Regex.Match[1]);
  finally
    Regex.Free;
  end;
end;

function TTFConvers.Convers(const Data: AnsiString): AnsiString;
begin
  CheckLeft(Data);
  CheckJoined(Data);
  Result := Colorerize(Data);
end;

end.


