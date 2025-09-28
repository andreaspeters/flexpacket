unit ueditor;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ButtonPanel, SynEdit,
  utypes, SynEditKeyCmds, SynEditTypes, SynBeautifier;

type

  { TTFEditor }

  TTFEditor = class(TForm)
    BPDefaultButtons: TButtonPanel;
    SEMessage: TSynEdit;
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

