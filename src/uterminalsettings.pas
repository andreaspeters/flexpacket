unit uterminalsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, ExtCtrls, Buttons, utypes, uini;

type

  { TTFTerminalSettings }

  PTFPConfig = ^TFPConfig;

  TTFTerminalSettings = class(TForm)
    BB7Plus: TBitBtn;
    BBAutobin: TBitBtn;
    BPDefaultButtons: TButtonPanel;
    CBBackground: TColorButton;
    CBFontColor: TColorButton;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LE7PlusDirectory: TLabeledEdit;
    LEAutoBinDirectory: TLabeledEdit;
    SDDSelectDirectory: TSelectDirectoryDialog;
    SPFontSize: TSpinEdit;
    procedure BBAutobinClick(Sender: TObject);
    procedure BB7PlusClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  TFTerminalSettings: TTFTerminalSettings;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TTFTerminalSettings }

procedure TTFTerminalSettings.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  CBBackground.ButtonColor := FPConfig^.TerminalBGColor;
  CBFontColor.ButtonColor := FPConfig^.TerminalFontColor;
  SPFontSize.Value := FPConfig^.TerminalFontSize;
  LE7PlusDirectory.Text := FPConfig^.Directory7Plus;
  LEAutobinDirectory.Text := FPConfig^.DirectoryAutoBin;
end;

procedure TTFTerminalSettings.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.TerminalBGColor := CBBackground.ButtonColor;
  FPConfig^.TerminalFontSize := SPFontSize.Value;
  FPConfig^.TerminalFontColor := CBFontColor.ButtonColor;
  FPConfig^.Directory7Plus := LE7PlusDirectory.Text;
  FPConfig^.DirectoryAutoBin := LEAutobinDirectory.Text;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;

procedure TTFTerminalSettings.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTerminalSettings.BB7PlusClick(Sender: TObject);
begin
  if SDDSelectDirectory.Execute then
    LE7PlusDirectory.Text := SDDSelectDirectory.FileName;
end;


procedure TTFTerminalSettings.BBAutobinClick(Sender: TObject);
begin
  if SDDSelectDirectory.Execute then
    LEAutoBinDirectory.Text := SDDSelectDirectory.FileName;
end;

end.

