unit uconvers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, StdCtrls, Controls, Graphics, Dialogs, ComCtrls,
  ActnList, Menus, ExtCtrls, PairSplitter, utypes, uCmdBoxCustom,
  uCmdBox, uAddressbook, RegExpr;

type

  { TTFConvers }

  TTFConvers = class(TForm)
    actClose: TAction;
    actConnect: TAction;
    actDisconnect: TAction;
    actReconnect: TAction;
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
    ToolBar1: TToolBar;
    TBConnect: TToolButton;
    ToolButton1: TToolButton;
    tbReconnect: TToolButton;
    ToolButton7: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure actConnectExecute(Sender: TObject);
    procedure actDisconnectExecute(Sender: TObject);
    procedure actReconnectExecute(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure PMConnectOnClick(Sender: TObject);
  private
    ChatWindow: TCmdBoxCustom;
    MessageWindow: TMemo;
    Message: Boolean;
    Reconnect: Boolean;
    procedure SendCommand(Sender: TObject; var Key: char);
    procedure AddBuddies(const Callsign: AnsiString);
    procedure CheckLeft(const Data: AnsiString);
    procedure CheckJoined(const Data: AnsiString);
    procedure AlreadyConnectedUsers(const Data: AnsiString);
    procedure UserNotification(const Data: AnsiString);
    procedure CheckDisconnected(const Data: AnsiString);
    procedure ReconnectConvers;
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

  // Attach Convers window at the main window
  if FMain.WindowState <> wsMinimized then
  begin
    Left := FMain.Left+FMain.Width+1;
    Top := FMain.Top;
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

procedure TTFConvers.actReconnectExecute(Sender: TObject);
begin
  Reconnect := tbReconnect.Down;
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

  // Cleanup userlist
  lbCallsigns.Clear;
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
    p: Byte;
begin
  Result := Data;

  Regex := TRegExpr.Create;
  try
    // Try LinBPQ
    Regex.Expression := '(?:.*?(\d{2}:\d{2})\s+)?([A-Z0-9]+)\s*:\s*(.*)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 2 then
      begin
        if Regex.SubExprMatchCount = 2 then
        begin
          callsign := Regex.Match[1];
          msg := Regex.Match[2];
          Result := Format(#27'[36m %-7s : '#27'[0m%s', [callsign, msg]);
        end;

        if Regex.SubExprMatchCount = 3 then
        begin
          clock := Regex.Match[1];
          callsign := Regex.Match[2];
          msg := Regex.Match[3];
          Result := Format(#27'[36m %-5s %-7s : '#27'[0m%s', [clock, callsign, msg]);
        end;
        Exit;
      end;

    // Try UROnode
    Regex.Expression := '(<\S+>):(.*)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 2 then
      begin
        callsign := Regex.Match[1];
        msg := Regex.Match[2];
        Result := Format(#27'[36m %-7s : '#27'[0m%s', [callsign, msg]);
        Exit;
      end;

    // Color Notification Callsign
    Callsign := UpperCase(FPConfig^.Callsign);
    // Remove SID
    p := Pos('-', Callsign);
    if p > 0 then
      Callsign := Copy(Callsign, 1, p - 1);

    if Pos('@'+Callsign, UpperCase(Data)) > 0 then
    begin
      Result := StringReplace(Result, '@'+UpperCase(Callsign), Format(#27'[101 @%s '#27'[0m', [Callsign]), [rfReplaceAll]);
      Result := StringReplace(Result, '@'+LowerCase(Callsign), Format(#27'[101 @%s '#27'[0m', [Callsign]), [rfReplaceAll]);
    end;
  finally
    Regex.Free;
  end;
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
  if (Pos('*** Left', Data) > 0) or (Pos('left channel', Data) > 0) then
  begin
    Callsign := GetCallsign(Data);
    if Length(Callsign) > 0 then
    begin
      for i:= 0 to lbCallsigns.Items.Count - 1 do
        if Pos(Callsign, lbCallsigns.Items[i]) > 0 then
        begin
          lbCallsigns.Items.Delete(i);
          Exit;
        end;
    end;
  end;
end;

procedure TTFConvers.CheckJoined(const Data: AnsiString);
var Callsign: AnsiString;
begin
  if (Pos('*** Joined', Data) > 0) or (Pos('joined channel', Data) > 0) then
  begin
    Callsign := GetCallsign(Data);
    if Length(Callsign) > 0 then
    begin
      if IsInBuddieList(Callsign) then
        Exit;
      AddBuddies(Format('%-7s (%s)',[Callsign, GetUsername(Data)]));
    end;
  end;
end;

// check already conneced
procedure TTFConvers.AlreadyConnectedUsers(const Data: AnsiString);
var Callsign, UserName: AnsiString;
    Regex: TRegExpr;
begin
  if (Length(Data) <= 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    // Try LinBPQ
    // 9A6BCDE  at  TSTCHT Name, City
    // ABC123   at  TSTCHT Name, City
    Regex.Expression := '([A-Za-z]{1,3}\d[A-Za-z0-9]{1,4}(?:-\d{1,2})?)\s+at\s+(?:\S)+\s+([^,]+).*';
    Regex.ModifierI := True;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 2 then
      begin
        Callsign := Trim(Regex.Match[1]);
        UserName := Trim(Regex.Match[2]);
        if Length(Callsign) > 0 then
        begin
          if IsInBuddieList(Callsign) then
            Exit;
          AddBuddies(Format('%-7s (%s)',[Callsign, UserName]));
        end;
      end;
  finally
    Regex.Free;
  end;
end;

function TTFConvers.GetUsername(const Data: AnsiString): AnsiString;
var Regex: TRegExpr;
begin
  if (Length(Data) <= 0) then
    Exit;

  Regex := TRegExpr.Create;
  try
    // Try LinBPQ
    Regex.Expression := '\s:\s(.*)(?:\*\*\* Joined Chat)';
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
    // Try LinBPQ
    Regex.Expression := '((?:\d{1})?[A-Z]{1,3}\d{1,3}[A-Z]{0,4}(?:-\d{1,2})?)';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 1 then
      begin
        Result := Trim(Regex.Match[1]);
        Exit;
      end;

    // Try UROnode
    Regex.Expression := '\*\*\*\s*(?:\(\d{1,2}:\d{2}\)\s*)?(\S+?)(?:@\S+)?\s+joined channel\s';
    Regex.ModifierI := False;
    if Regex.Exec(Data) then
      if Regex.SubExprMatchCount >= 1 then
      begin
        Result := Trim(Regex.Match[1]);
        Exit;
      end;

  finally
    Regex.Free;
  end;
end;

procedure TTFConvers.ReconnectConvers;
var callsign: String;
    i: Integer;
begin
  if not Assigned(FPConfig^.DestCallsign[FPConfig^.MaxChannels]) then
    Exit;

  if Reconnect then
  begin
    i := FPConfig^.DestCallsign[FPConfig^.MaxChannels].Count;
    if i > 0 then
    begin
      callsign := FPConfig^.DestCallsign[FPConfig^.MaxChannels][i-1];
      FMain.ConnectExecute(callsign, FPConfig^.MaxChannels);
    end;
  end;
end;

procedure TTFConvers.CheckDisconnected(const Data: AnsiString);
begin
  if (Length(Data) <= 0) then
    Exit;

  if (Pos('disconnected', LowerCase(Data)) > 0) then
    ReconnectConvers;
end;

procedure TTFConvers.UserNotification(const Data: AnsiString);
var Callsign: AnsiString;
    p: Integer;
begin
  Callsign := UpperCase(FPConfig^.Callsign);

  // Remove SID
  p := Pos('-', Callsign);
  if p > 0 then
    Callsign := Copy(Callsign, 1, p - 1);

  if Pos('@'+Callsign, UpperCase(Data)) > 0 then
  begin
    FMain.TrayIcon.BalloonTitle := 'FlexPacket';
    FMain.TrayIcon.BalloonHint := 'You go a message in Convers';
    FMain.TrayIcon.BalloonFlags := bfInfo;
    FMain.TrayIcon.ShowBalloonHint;
  end;
end;

function TTFConvers.Convers(const Data: AnsiString): AnsiString;
var Buffer, Line: AnsiString;
    p: Integer;
begin
  Result := '';

  if (Length(Data) <= 0) then
    Exit;

  Buffer := Data;

  repeat
    p := Pos(#13#10, Buffer);

    if p > 0 then
    begin
      Line := Copy(Buffer, 1, p - 1);

      CheckLeft(Line);
      CheckJoined(Line);
      AlreadyConnectedUsers(Line);
      CheckDisconnected(Line);
      UserNotification(Line);
      Result := Result + Colorerize(Line) + #13#10;

      Delete(Buffer, 1, p + 1);
    end
    else
    begin
      if Buffer <> '' then
      begin
        CheckLeft(Buffer);
        CheckJoined(Buffer);
        AlreadyConnectedUsers(Buffer);
        CheckDisconnected(Buffer);
        UserNotification(Buffer);
        Result := Result + Colorerize(Buffer);
      end;

      Buffer := ''; // Loop beenden
    end;

  until Buffer = '';
end;

end.


