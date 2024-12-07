unit uterminalsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, ExtCtrls, Buttons, utypes, uini;

type

  { TTFTerminalSettings }

  TTFTerminalSettings = class(TForm)
    BB7Plus: TBitBtn;
    BB7PlusExe1: TBitBtn;
    BBAutobin: TBitBtn;
    BB7PlusExe: TBitBtn;
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
    LEAPRSMapExe: TLabeledEdit;
    LEAutoBinDirectory: TLabeledEdit;
    LE7PlusExe: TLabeledEdit;
    ODExecutable: TOpenDialog;
    SDDSelectDirectory: TSelectDirectoryDialog;
    SPFontSize: TSpinEdit;
    procedure BBAutobinClick(Sender: TObject);
    procedure BB7PlusClick(Sender: TObject);
    procedure BB7PlusExeClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BBAPRSMapExeClick(Sender: TObject);
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
  LE7PlusExe.Text := FPConfig^.Executable7Plus;
  LEAPRSMapExe.Text := FPConfig^.ExecutableAPRSMap;
end;

procedure TTFTerminalSettings.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.TerminalBGColor := CBBackground.ButtonColor;
  FPConfig^.TerminalFontSize := SPFontSize.Value;
  FPConfig^.TerminalFontColor := CBFontColor.ButtonColor;
  FPConfig^.Directory7Plus := LE7PlusDirectory.Text;
  FPConfig^.DirectoryAutoBin := LEAutobinDirectory.Text;
  FPConfig^.Executable7Plus := LE7PlusExe.Text;
  FPConfig^.ExecutableAPRSMap := LEAPRSMapExe.Text;
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

procedure TTFTerminalSettings.BB7PlusExeClick(Sender: TObject);
begin
  if ODExecutable.Execute then
    LE7PlusExe.Text := ODExecutable.FileName;
end;

procedure TTFTerminalSettings.BBAPRSMapExeClick(Sender: TObject);
begin
  if ODExecutable.Execute then
    LEAPRSMapExe.Text := ODExecutable.FileName;
end;

end.

