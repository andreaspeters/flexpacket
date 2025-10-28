unit uterminalsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin,
  ButtonPanel, ExtCtrls, Buttons, SynEdit, BGRAImageList, utypes, uini;

type

  { TTFTerminalSettings }

  TTFTerminalSettings = class(TForm)
    BPDefaultButtons: TButtonPanel;
    CBBackground: TColorButton;
    CBConversBackground: TColorButton;
    CBConversBackground1: TColorButton;
    CBFontColor: TColorButton;
    FontDialog1: TFontDialog;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    LEFontName: TLabeledEdit;
    LE7PlusDirectory: TLabeledEdit;
    LEAPRSMapExe: TLabeledEdit;
    LEFormsExe: TLabeledEdit;
    LEAutoBinDirectory: TLabeledEdit;
    LE7PlusExe: TLabeledEdit;
    ODExecutable: TOpenDialog;
    SDDSelectDirectory: TSelectDirectoryDialog;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    sbChooseFont: TSpeedButton;
    SPFontSize: TSpinEdit;
    SESignature: TSynEdit;
    procedure BBAutobinClick(Sender: TObject);
    procedure BB7PlusClick(Sender: TObject);
    procedure BB7PlusExeClick(Sender: TObject);
    procedure BBFormsExeClick(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BBAPRSMapExeClick(Sender: TObject);
    procedure sbChooseFontClick(Sender: TObject);
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
  LEFontName.Text := FPConfig^.TerminalFontName;
  LE7PlusDirectory.Text := FPConfig^.Directory7Plus;
  LEAutobinDirectory.Text := FPConfig^.DirectoryAutoBin;
  LE7PlusExe.Text := FPConfig^.Executable7Plus;
  LEAPRSMapExe.Text := FPConfig^.ExecutableAPRSMap;
  LEFormsExe.Text := FPConfig^.ExecutableForms;
  SESignature.Text := FPConfig^.TerminalSignature;
  CBConversBackground.ButtonColor := FPConfig^.ConversBGColor;
end;

procedure TTFTerminalSettings.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.TerminalBGColor := CBBackground.ButtonColor;
  FPConfig^.TerminalFontSize := SPFontSize.Value;
  FPConfig^.TerminalFontColor := CBFontColor.ButtonColor;
  FPConfig^.TerminalFontName := LEFontName.Text;
  FPConfig^.Directory7Plus := LE7PlusDirectory.Text;
  FPConfig^.DirectoryAutoBin := LEAutobinDirectory.Text;
  FPConfig^.Executable7Plus := LE7PlusExe.Text;
  FPConfig^.ExecutableAPRSMap := LEAPRSMapExe.Text;
  FPConfig^.ExecutableForms := LEFormsExe.Text;
  FPConfig^.TerminalSignature := SESignature.Text;
  FPConfig^.ConversBGColor := CBConversBackground.ButtonColor;
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

procedure TTFTerminalSettings.BBFormsExeClick(Sender: TObject);
begin
  if ODExecutable.Execute then
    LEFormsExe.Text := ODExecutable.FileName;
end;

procedure TTFTerminalSettings.BBAPRSMapExeClick(Sender: TObject);
begin
  if ODExecutable.Execute then
    LEAPRSMapExe.Text := ODExecutable.FileName;
end;

procedure TTFTerminalSettings.sbChooseFontClick(Sender: TObject);
begin
  if FontDialog1.Execute then
  begin
    LEFontName.Text := FontDialog1.Font.Name;
    SPFontSize.Value := FontDialog1.Font.Size;
  end;
end;

end.

