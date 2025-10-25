unit uconvers;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ActnList,
  PairSplitter, utypes, uCmdBoxCustom, uCmdBox;

type

  { TTFConvers }

  TTFConvers = class(TForm)
    actClose: TAction;
    ActionList1: TActionList;
    StatusBar1: TStatusBar;
    ToolBar1: TToolBar;
    ToolButton7: TToolButton;
    procedure actCloseExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    ChatWindow: TCmdBoxCustom;
  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  TFConvers: TTFConvers;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

procedure TTFConvers.actCloseExecute(Sender: TObject);
begin
  Close;
end;

procedure TTFConvers.FormShow(Sender: TObject);
begin
  ChatWindow := TCmdBoxCustom.Create(Self);
  ChatWindow.EscapeCodeType := esctAnsi;
  ChatWindow.Parent := Self;
  ChatWindow.Left := 4;
  ChatWindow.Width := Width;
  ChatWindow.Font.Color := FPConfig^.TerminalFontColor;
  ChatWindow.Font.Pitch := fpFixed;
  ChatWindow.Font.Name := FPConfig^.TerminalFontName;
  ChatWindow.Font.Style := [fsBold];
  ChatWindow.Font.Size := FPConfig^.TerminalFontSize;
  ChatWindow.BackGroundColor := FPConfig^.TerminalBGColor;
  ChatWindow.TextColor(FPConfig^.TerminalFontColor);
  ChatWindow.TextBackground(FPConfig^.TerminalBGColor);
  ChatWindow.Enabled := True;
  ChatWindow.Visible := True;
  ChatWindow.InputSelBackGround := clRed;
  ChatWindow.Align := alClient;
end;

procedure TTFConvers.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

end.

