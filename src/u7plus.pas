unit u7plus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Spin, ButtonPanel, utypes;

type

  { TF7Plus }

  PTFPConfig = ^TFPConfig;

  TF7Plus = class(TForm)
    BBSource: TBitBtn;
    BBDestination: TBitBtn;
    BPDefaultButtons: TButtonPanel;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    LEDestinationDirectory: TLabeledEdit;
    LESourceFile: TLabeledEdit;
    OPSource: TOpenDialog;
    RBParts: TRadioButton;
    RBPartSize: TRadioButton;
    SDDDestination: TSelectDirectoryDialog;
    SPPartSize: TSpinEdit;
    SPAmount: TSpinEdit;
    procedure BBDestinationClick(Sender: TObject);
    procedure BBSourceClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
  private

  public
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  F7Plus: TF7Plus;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TF7Plus }

procedure TF7Plus.CloseButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TF7Plus.BBSourceClick(Sender: TObject);
begin
  if OPSource.Execute then
    LESourceFile.Text := OPSource.FileName;
end;

procedure TF7Plus.BBDestinationClick(Sender: TObject);
begin
  if SDDDestination.Execute then
    LEDestinationDirectory.Text := SDDDestination.FileName;
end;

procedure TF7Plus.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

end.

