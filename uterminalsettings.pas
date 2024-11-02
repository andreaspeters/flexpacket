unit uterminalsettings;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Spin, utypes;

type

  { TTFTerminalSettings }

  PTFPConfig = ^TFPConfig;

  TTFTerminalSettings = class(TForm)
    BtnCancel: TButton;
    BtnSave: TButton;
    CBBackground: TColorButton;
    CBFontColor: TColorButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    SPFontSize: TSpinEdit;
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
end;

procedure TTFTerminalSettings.BtnSaveClick(Sender: TObject);
begin
  FPConfig^.TerminalBGColor := CBBackground.ButtonColor;
  FPConfig^.TerminalFontSize := SPFontSize.Value;
  FPConfig^.TerminalFontColor := CBFontColor.ButtonColor;
  Close;
end;

procedure TTFTerminalSettings.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

