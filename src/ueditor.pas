unit ueditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, Buttons,
  ComCtrls, ActnList, ExtCtrls, SynEdit, utypes, SynEditKeyCmds, SynEditTypes,
  SynBeautifier;

type

  { TTFEditor }

  TTFEditor = class(TForm)
    actSignature: TAction;
    actOpenFile: TAction;
    actSaveAs: TAction;
    actSend: TAction;
    actNew: TAction;
    ActionList1: TActionList;
    BPDefaultButtons: TButtonPanel;
    imgListSmall: TImageList;
    odOpenFile: TOpenDialog;
    sdSaveFile: TSaveDialog;
    SEMessage: TSynEdit;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    ToolButton6: TToolButton;
    procedure actNewExecute(Sender: TObject);
    procedure actOpenFileExecute(Sender: TObject);
    procedure actSaveAsExecute(Sender: TObject);
    procedure actSignatureExecute(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  TFEditor: TTFEditor;
  FPConfig: PTFPConfig;

implementation

uses umain;

{$R *.lfm}

procedure TTFEditor.OKButtonClick(Sender: TObject);
var i: Integer;
begin
  if SEMessage.Lines.Count > 0 then
    for i := 0 to SEMessage.Lines.Count do
      FMain.SendStringCommand(FMain.CurrentChannel,0, SEMessage.Lines[i]);
end;

procedure TTFEditor.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TTFEditor.actNewExecute(Sender: TObject);
begin
  SEMessage.Text := #10#10#10#10#10 + FPConfig^.TerminalSignature;
end;

procedure TTFEditor.actOpenFileExecute(Sender: TObject);
begin
  if odOpenFile.Execute then
  begin
    SEMessage.Lines.LoadFromFile(odOpenFile.FileName);
    SEMessage.Text := SEMessage.Text;
  end;
end;

procedure TTFEditor.actSaveAsExecute(Sender: TObject);
begin

  if sdSaveFile.Execute then
    SEMessage.Lines.SaveToFile(sdSaveFile.FileName);
end;

procedure TTFEditor.actSignatureExecute(Sender: TObject);
begin
  SEMessage.Text := SEMessage.Text + #10#10#10#10#10 + FPConfig^.TerminalSignature;
end;

procedure TTFEditor.SetConfig(Config: PTFPConfig);
var Beauty: TSynBeautifier;
begin
  FPConfig := Config;

  Beauty := TSynBeautifier.Create(Self);
  Beauty.IndentType := sbitConvertToTabOnly;
  Beauty.AutoIndent := True;

  SEMessage.Beautifier := Beauty;
  SEMessage.BlockIndent := 0;
  SEMessage.BlockTabIndent := 0;
  SEMessage.DoubleBuffered := DoubleBuffered;
  SEMessage.Options := [eoBracketHighlight,eoGroupUndo,eoScrollPastEol,eoTrimTrailingSpaces];
  SEMessage.Options2 := [eoFoldedCopyPaste,eoOverwriteBlock,eoAcceptDragDropEditing];

  SEMessage.Text := #10#10#10#10#10 + FPConfig^.TerminalSignature;
end;

end.

