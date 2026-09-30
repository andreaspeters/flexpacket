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
    CBModem: TComboBox;
    GBOperatingMode: TGroupBox;
    GBTNC: TGroupBox;
    LabelCompany: TLabel;
    LabelModel: TLabel;
    LabelMode: TLabel;
    LabelSubmode: TLabel;
    LabelModem: TLabel;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
    procedure SelectionChanged(Sender: TObject);
  private
    FPConfig: PTFPConfig;
    FLoading: Boolean;
    procedure SendSCSCommand(const Command: String);
  public
  end;

var
  TFExtendedHostmode: TTFExtendedHostmode;

implementation

{$R *.lfm}

procedure TTFExtendedHostmode.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  FLoading := True;
  try
  CBCompany.ItemIndex := CBCompany.Items.IndexOf(FPConfig^.HostmodeCompany);
  CBModel.ItemIndex := CBModel.Items.IndexOf(FPConfig^.HostmodeModel);
  CBMode.ItemIndex := CBMode.Items.IndexOf(FPConfig^.HostmodeMode);
  CBSubmode.ItemIndex := CBSubmode.Items.IndexOf(FPConfig^.HostmodeSubmode);
  CBModem.ItemIndex := CBModem.Items.IndexOf(FPConfig^.HostmodeModem);
  if CBCompany.ItemIndex < 0 then CBCompany.ItemIndex := 0;
  if CBModel.ItemIndex < 0 then CBModel.ItemIndex := 0;
  if CBMode.ItemIndex < 0 then CBMode.ItemIndex := 0;
  if CBSubmode.ItemIndex < 0 then CBSubmode.ItemIndex := 0;
  if CBModem.ItemIndex < 0 then CBModem.ItemIndex := 0;
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
  FPConfig^.HostmodeModem := CBModem.Text;
  FPConfig^.ExtendedHostmode := (CBCompany.Text = 'SCS') and
    ((CBModel.Text = 'PTC-II') or (CBModel.Text = 'PTC-IIpro')) and
    (CBMode.Text = 'Packet Radio');
  ApplyConfiguration;
  Close;
end;

procedure TTFExtendedHostmode.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFExtendedHostmode.SendSCSCommand(const Command: String);
begin
  if not Assigned(FPConfig) or not Assigned(FPConfig^.HostmodeCommand) then
    Exit;
  if (CBCompany.Text <> 'SCS') or
     ((CBModel.Text <> 'PTC-II') and (CBModel.Text <> 'PTC-IIpro')) then
    Exit;
  FPConfig^.HostmodeCommand(0, 1, Command);
end;

procedure TTFExtendedHostmode.SelectionChanged(Sender: TObject);
var Baud: String;
begin
  if FLoading then
    Exit;
  if CBMode.Text <> 'Packet Radio' then
    Exit;

  // %B is the documented SCS PTC packet-radio baud command.  Hostmode
  // commands use channel 0 in the existing TNC command path.
  if Sender = CBSubmode then
  begin
    if CBSubmode.Text = '1200 Baud' then
      Baud := '1200'
    else if CBSubmode.Text = '9600 Baud' then
      Baud := '9600'
    else
      Exit;
    SendSCSCommand('%B ' + Baud);
  end
  else if Sender = CBModem then
  begin
    // The PTC-II has no separate documented hostmode AFSK/FSK selector;
    // the selected packet baudrate selects the corresponding modem class.
    if CBModem.Text = 'AFSK' then
      SendSCSCommand('%B 1200')
    else if CBModem.Text = 'FSK' then
      SendSCSCommand('%B 9600');
  end;
end;

end.
