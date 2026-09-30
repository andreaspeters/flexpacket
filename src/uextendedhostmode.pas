unit uextendedhostmode;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ButtonPanel,
  utypes;

type
  TTFExtendedHostmode = class(TForm)
    BPDefaultButtons: TButtonPanel;
    CBCompany: TComboBox;
    CBModel: TComboBox;
    CBMode: TComboBox;
    CBSubmode: TComboBox;
    GBOperatingMode: TGroupBox;
    GBTNC: TGroupBox;
    LabelCompany: TLabel;
    LabelModel: TLabel;
    LabelMode: TLabel;
    LabelSubmode: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
    procedure SelectionChanged(Sender: TObject);
  private
    FPConfig: PTFPConfig;
    FHostmodeType: String;
    FLoading: Boolean;
    procedure SendSCSCommand(const Command: String);
  public
  end;

var
  TFExtendedHostmode: TTFExtendedHostmode;

implementation

uses umain, uini;

{$R *.lfm}

procedure TTFExtendedHostmode.SetConfig(Config: utypes.PTFPConfig);
begin
  FPConfig := Config;
  if not Assigned(FPConfig) then
    Exit;

  FHostmodeType := FPConfig^.HostmodeType;
  FLoading := True;
  try
    CBCompany.ItemIndex := CBCompany.Items.IndexOf(FPConfig^.HostmodeCompany);
    CBModel.ItemIndex := CBModel.Items.IndexOf(FPConfig^.HostmodeModel);
    CBMode.ItemIndex := CBMode.Items.IndexOf(FPConfig^.HostmodeMode);
    CBSubmode.ItemIndex := CBSubmode.Items.IndexOf(FPConfig^.HostmodeSubmode);
    if FHostmodeType = 'SCS PTC' then
      CBCompany.ItemIndex := CBCompany.Items.IndexOf('SCS');
    if CBCompany.ItemIndex < 0 then CBCompany.ItemIndex := 0;
    if CBModel.ItemIndex < 0 then CBModel.ItemIndex := 0;
    if CBMode.ItemIndex < 0 then CBMode.ItemIndex := 0;
    if CBSubmode.ItemIndex < 0 then CBSubmode.ItemIndex := 0;
  finally
    FLoading := False;
  end;
end;

procedure TTFExtendedHostmode.BtnSaveClick(Sender: TObject);
begin
  if not Assigned(FPConfig) then Exit;
  FPConfig^.HostmodeCompany := CBCompany.Text;
  FPConfig^.HostmodeModel := CBModel.Text;
  FPConfig^.HostmodeMode := CBMode.Text;
  FPConfig^.HostmodeSubmode := CBSubmode.Text;
  FPConfig^.HostmodeType := FHostmodeType;
  FPConfig^.ExtendedHostmode := (FHostmodeType = 'SCS PTC') and
    (CBCompany.Text = 'SCS') and
    ((CBModel.Text = 'PTC-II') or (CBModel.Text = 'PTC-IIe') or
     (CBModel.Text = 'PTC-IIpro')) and
    (CBMode.Text = 'Packet Radio');
  SaveConfigToFile(FPConfig);
  Close;
end;

procedure TTFExtendedHostmode.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFExtendedHostmode.SendSCSCommand(const Command: String);
begin
  if not Assigned(FPConfig) then
    Exit;
  if (FHostmodeType <> 'SCS PTC') or (CBCompany.Text <> 'SCS') or
     ((CBModel.Text <> 'PTC-II') and (CBModel.Text <> 'PTC-IIe') and
      (CBModel.Text <> 'PTC-IIpro')) then
    Exit;
  FMain.SendStringCommand(1, 1, Command);
end;

procedure TTFExtendedHostmode.SelectionChanged(Sender: TObject);
var
  Baud: String;
begin
  if FLoading or not Assigned(FPConfig) then
    Exit;

  FPConfig^.HostmodeCompany := CBCompany.Text;
  FPConfig^.HostmodeModel := CBModel.Text;
  FPConfig^.HostmodeMode := CBMode.Text;
  FPConfig^.HostmodeSubmode := CBSubmode.Text;
  FPConfig^.HostmodeType := FHostmodeType;
  FPConfig^.ExtendedHostmode := (FHostmodeType = 'SCS PTC') and
    (CBCompany.Text = 'SCS') and
    ((CBModel.Text = 'PTC-II') or (CBModel.Text = 'PTC-IIe') or
     (CBModel.Text = 'PTC-IIpro')) and
    (CBMode.Text = 'Packet Radio');
  SaveConfigToFile(FPConfig);

  if CBMode.Text <> 'Packet Radio' then
    Exit;

  // SCS packet-radio controls use channel 1; 255 is reserved for G polling.
  if FPConfig^.ExtendedHostmode then
    SendSCSCommand('PR');

  if Sender = CBSubmode then
    if Length(CBSubmode.Text) >= 0 then
      SendSCSCommand('%B ' + CBSubmode.Text)
  else
    SendSCSCommand('%B 1200');
end;

end.
