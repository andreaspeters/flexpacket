unit ukiss;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Bluetooth, ctypes, sockets,
  Buttons, ExtCtrls, ButtonPanel, StdCtrls, Spin, ActnList, utypes, uini, baseunix;

type

  { TFKiss }

  PTFPConfig = ^TFPConfig;

  TFKiss = class(TForm)
    actScanBluetooth: TAction;
    ActionList1: TActionList;
    BitBtn1: TBitBtn;
    BPDefaultButtons: TButtonPanel;
    cbBluetoothDevices: TComboBox;
    ECallsign: TLabeledEdit;
    Label1: TLabel;
    ODSelectFile: TOpenDialog;
    SPMaxChannels: TSpinEdit;
    procedure actScanBluetoothExecute(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
  private

  public

  end;

var
  FKiss: TFKiss;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TFKiss }

procedure TFKiss.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  SPMaxChannels.Value := FPConfig^.MaxChannels;
end;


procedure TFKiss.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFKiss.actScanBluetoothExecute(Sender: TObject);
var
  device_id, device_sock: cint;
  scan_info: array[0..127] of inquiry_info;
  scan_info_ptr: Pinquiry_info;
  found_devices: cint;
  DevName: array[0..255] of Char;
  PDevName: PCChar;
  RemoteName: array[0..255] of Char;
  PRemoteName: PCChar;
  i: Integer;
  timeout1: Integer = 5;
  timeout2: Integer = 5000;
begin
  // got from the example website of bluetoothlaz
  // get the id of the first bluetooth device.
  device_id := hci_get_route(nil);
  if (device_id < 0) then
    raise Exception.Create('FindBlueTooth: hci_get_route')
  else
    writeln('device_id = ',device_id);

  // create a socket to the device
  device_sock := hci_open_dev(device_id);
  if (device_sock < 0) then
    raise Exception.Create('FindBlueTooth: hci_open_dev')
  else
    writeln('device_sock = ',device_sock);

  // scan for bluetooth devices for 'timeout1' seconds
  scan_info_ptr:=@scan_info[0];
  FillByte(scan_info[0],SizeOf(inquiry_info)*128,0);
  found_devices := 0;
  try
    found_devices := hci_inquiry_1(device_id, timeout1, 128, nil, @scan_info_ptr, IREQ_CACHE_FLUSH);
  except
  end;

  writeln('found_devices (count) = ',found_devices);

  if (found_devices > 0) then
  begin
    for i := 0 to (found_devices - 1) do
    begin
      PDevName:=@DevName[0];
      PRemoteName:=@RemoteName[0];
      ba2str(@scan_info[i].bdaddr, PDevName);
      // Read the remote name for 'timeout2' milliseconds
      if (hci_read_remote_name(device_sock,@scan_info[i].bdaddr,255,PRemoteName,timeout2) = 0) then
        cbBluetoothDevices.Items.Add(Format('%s ,%s', [PChar(PRemoteName), PChar(PDevName)]))
    end;
  end;

  hci_close_dev(device_sock);
end;

procedure TFKiss.BtnSaveClick(Sender: TObject);
var bl: String;
begin
  FPConfig^.MaxChannels := SPMaxChannels.Value;
  if cbBluetoothDevices.ItemIndex >= 0 then
  begin
    bl := cbBluetoothDevices.Items[cbBluetoothDevices.ItemIndex];
    bl.Split(',');
    if length(bl) = 2 then
    begin
      FPConfig^.KISSBluetoothName := Trim(bl[1]);
      FPConfig^.KISSBluetoothMac := Trim(bl[2]);
    end;
  end;

  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;

procedure TFKiss.FormShow(Sender: TObject);
begin
  if (Length(FPConfig^.KISSBluetoothMac) = 17) and not (FPConfig^.KISSBluetoothMac = '00:00:00:00:00:00') and (cbBluetoothDevices.Items.Count > 0) then
    cbBluetoothDevices.Items.Add(Format('%s ,%s', [PChar(FPConfig^.KISSBluetoothName), PChar(FPConfig^.KISSBluetoothMac)]))


end;


end.

