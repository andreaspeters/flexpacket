unit u7plus;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  Buttons, Spin, ButtonPanel, utypes, Process;

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
    SPParts: TSpinEdit;
    procedure BBDestinationClick(Sender: TObject);
    procedure BBSourceClick(Sender: TObject);
    procedure CloseButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
    procedure RBPartsClick(Sender: TObject);
    procedure RBPartSizeClick(Sender: TObject);
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

procedure TF7Plus.OKButtonClick(Sender: TObject);
var run: TProcess;
begin
  run := TProcess.Create(nil);
  try
    run.Executable := FPConfig^.Executable7Plus;
    run.CurrentDirectory := LEDestinationDirectory.Text;

    if SPPartSize.Enabled then
      run.Parameters.Add('-sb ' + SPPartSize.Text);

    if SPParts.Enabled then
      run.Parameters.Add('-sp ' + SPParts.Text);

    run.Parameters.Add(LESourceFile.Text);

    run.Options := [];
    run.Execute;
  finally
    run.Free;
  end;
  Close;
end;

procedure TF7Plus.RBPartsClick(Sender: TObject);
begin
  RBPartSize.Checked := False;
  SPParts.Enabled := True;
  SPPartSize.Enabled := False;
end;

procedure TF7Plus.RBPartSizeClick(Sender: TObject);
begin
  RBParts.Checked := False;
  SPParts.Enabled := False;
  SPPartSize.Enabled := True;
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
  LEDestinationDirectory.Text := FPConfig^.Directory7Plus;
end;

end.

