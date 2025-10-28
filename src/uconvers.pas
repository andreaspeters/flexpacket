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
    PSSChannel: TPairSplitterSide;
    PSSMTx: TPairSplitterSide;
    PMConnect: TPopupMenu;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    TBConnect: TToolButton;
    TBDisconnect: TToolButton;
    ToolButton7: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure PMConnectOnClick(Sender: TObject);
  private
    ChatWindow: TCmdBoxCustom;
    MessageWindow: TMemo;
    Message: Boolean;
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure GetBuddies(const Data: AnsiString);
    procedure CheckLeft(const Data: AnsiString);
    function Colorerize(const Data: AnsiString): AnsiString;
    function GetCallsign(const Data: AnsiString): AnsiString;
    function IsInBuddieList(Callsign: AnsiString): Boolean;
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
    ChatWindow.Parent := PSSChannel;
    ChatWindow.Left := 4;
    ChatWindow.Enabled := True;
    ChatWindow.Visible := True;
    ChatWindow.Align := alClient;
    ChatWindow.BackGroundColor := FPConfig^.ConversBGColor;
    ChatWindow.TextBackground(FPConfig^.ConversBGColor);
    ChatWindow.Font.Size := FPConfig^.TerminalFontSize - 1;
    ChatWindow.Font.Color := FPConfig^.ConversFontColor;
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

  TFAdressbook.OpenDatabase;
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
    Callsign: String;
begin
  PMConnect.Items.Clear;
  TFAdressbook.SQLQuery.Close;
  TFAdressbook.SQLQuery.SQL.Text := 'SELECT callsign FROM "ADR" where type = "Convers"';
  TFAdressbook.SQLQuery.Open;
  TFAdressbook.SQLQuery.First;
  while not TFAdressbook.SQLQuery.EOF do
  begin
    Callsign := TFAdressbook.SQLQuery.FieldByName('callsign').AsString;
    if Length(Callsign) <= 0 then
     TFAdressbook.SQLQuery.Next;

    Item := TMenuItem.Create(PMConnect);
    Item.Caption := Callsign;
    Item.OnClick := @PMConnectOnClick;
    PMConnect.Items.Add(Item);

    TFAdressbook.SQLQuery.Next;
  end;


  Pt := TBConnect.ClientToScreen(Point(0, TBConnect.Height));
  PMConnect.PopUp(Pt.X, Pt.Y);
end;

procedure TTFConvers.actDisconnectExecute(Sender: TObject);
begin
  FMain.SendStringCommand(FPConfig^.MaxChannels, 1, 'D');
  TBDisconnect.Enabled := False;
end;

procedure TTFConvers.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FMain.SetChannelButtonLabel(FPConfig^.MaxChannels,'Disc');
  FPConfig^.IsConvers[FPConfig^.MaxChannels] := False;
  MessageWindow.Parent := FMain.PSSMTx;
  ChatWindow.Parent := FMain.PSSChannel;
  ChatWindow.BackGroundColor := FPConfig^.TerminalBGColor;
  ChatWindow.TextBackground(FPConfig^.TerminalBGColor);
  ChatWindow.Font.Size := FPConfig^.TerminalFontSize;
  ChatWindow.Font.Color := FPConfig^.TerminalFontColor;
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
    Msg: AnsiString;
begin
  Result := Data;

  if (Length(Data) <= 0) then
    Exit;

  if Pos(':', Data) > 0 then
    Message := True;

  if Message then
  begin
    if Pos(#13, Data) > 0 then
      Result := StringReplace(Result, #13, #13 + ColorToAnsi(clNavy, False), [rfReplaceAll]);
    if Pos(':', Data) > 0 then
      Result := StringReplace(Result, ':', ESC+'[0m :', [rfReplaceAll]);
  end;
end;

procedure TTFConvers.GetBuddies(const Data: AnsiString);
var Callsign: AnsiString;
begin
  if (Length(Data) <= 0) then
    Exit;

  Callsign := GetCallsign(Data);

  if Length(Callsign) > 0 then
    if not IsInBuddieList(Callsign) then
      lbCallsigns.Items.Add(Callsign);
end;

function TTFConvers.IsInBuddieList(Callsign: AnsiString): Boolean;
var i: Integer;
begin
  Result := False;
  for i:= 0 to lbCallsigns.Items.Count - 1 do
    if SameText(lbCallsigns.Items[i], Callsign) then
    begin
      Result := True;
      Exit;
    end;
end;

procedure TTFConvers.CheckLeft(const Data: AnsiString);
var Regex: TRegExpr;
    Callsign: AnsiString;
    i: Integer;
begin
  if (Length(Data) <= 0) then
    Exit;

  if Pos(Data, '*** Left') > 0 then
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

function TTFConvers.GetCallsign(const Data: AnsiString): AnsiString;
var Regex: TRegExpr;
begin
  Result := '';

  if (Length(Data) <= 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    Regex.Expression := '((?:\d{1})?[A-Z]{1,3}\d{1,3}[A-Z]{0,3}(?:-\d{1,2})?)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 1 then
        Result := Regex.Match[1];
  finally
    Regex.Free;
  end;
end;

function TTFConvers.Convers(const Data: AnsiString): AnsiString;
begin
  if (FPConfig^.Connected[FPConfig^.MaxChannels]) then
    TBDisconnect.Enabled := True;

  GetBuddies(Data);
  CheckLeft(Data);
  Result := Colorerize(Data);
end;

end.


