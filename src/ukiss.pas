unit ukiss;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Bluetooth,
  Buttons, ExtCtrls, ButtonPanel, StdCtrls, Spin, utypes, uini
  {$IFDEF UNIX}, BaseUnix{$ENDIF}
  {$IFDEF MSWINDOWS}, Registry{$ENDIF};

type

  { TFKiss }

  PTFPConfig = ^TFPConfig;

  TFKiss = class(TForm)
    BPDefaultButtons: TButtonPanel;
    cbBluetoothDevices: TComboBox;
    ECallsign: TLabeledEdit;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    LESocketPath: TLabeledEdit;
    ODSelectFile: TOpenDialog;
    RGTransport: TRadioGroup;
    SpeedButton1: TSpeedButton;
    sbScanBluetooth: TSpeedButton;
    SPMaxChannels: TSpinEdit;
    CBComPort: TComboBox;
    CBComSpeed: TComboBox;
    procedure actScanBluetoothExecute(Sender: TObject);
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure BBSocketPathClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RGTransportClick(Sender: TObject);
  private
    procedure PopulateSerialPorts;
    {$IFDEF UNIX}
    procedure AddSerialPorts(const Pattern: String);
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    function GetAvailableCOMPorts: TStringList;
    {$ENDIF}
  public

  end;

var
  FKiss: TFKiss;
  FPConfig: PTFPConfig;
  OldWidth, OldHeight: Integer;

implementation

{$R *.lfm}

{ TFKiss }

procedure TFKiss.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
  LESocketPath.Text := FPConfig^.KISSPipe;
  SPMaxChannels.Value := FPConfig^.MaxChannels;
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
  i, x: Integer;
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
  scan_info_ptr := @scan_info[0];
  FillByte(scan_info[0],SizeOf(inquiry_info)*128,0);
  found_devices := 0;
  try
    found_devices := hci_inquiry_1(device_id, timeout1, 128, nil, @scan_info_ptr, IREQ_CACHE_FLUSH);
  except
  end;

  writeln('found_devices (count) = ',found_devices);

  for x := 0 to 5 do
  begin
    if (found_devices > 0) then
    begin
      for i := 0 to (found_devices - 1) do
      begin
        PDevName := @DevName[0];
        PRemoteName := @RemoteName[0];
        ba2str(@scan_info[i].bdaddr, PDevName);
        // Read the remote name for 'timeout2' milliseconds
        if (hci_read_remote_name(device_sock,@scan_info[i].bdaddr,255,PRemoteName,timeout2) = 0) then
        begin
          cbBluetoothDevices.Items.Add(Format('%s ,%s', [PChar(PRemoteName), PChar(PDevName)]));
          cbBluetoothDevices.ItemIndex := 0;
        end;
      end;
      break;
    end;
    sleep(3000);
  end;

  hci_close_dev(device_sock);
end;

procedure TFKiss.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFKiss.BBSocketPathClick(Sender: TObject);
begin
  if ODSelectFile.Execute then
    LESocketPath.Text := ODSelectFile.FileName;
end;

procedure TFKiss.RGTransportClick(Sender: TObject);
begin
  Label2.Enabled := RGTransport.ItemIndex = 0;
  cbBluetoothDevices.Enabled := RGTransport.ItemIndex = 0;
  sbScanBluetooth.Enabled := RGTransport.ItemIndex = 0;
  Label3.Enabled := RGTransport.ItemIndex = 1;
  Label4.Enabled := RGTransport.ItemIndex = 1;
  CBComPort.Enabled := RGTransport.ItemIndex = 1;
  CBComSpeed.Enabled := RGTransport.ItemIndex = 1;
end;

procedure TFKiss.BtnSaveClick(Sender: TObject);
var
  BluetoothDevice: TStringArray;
  Speed: Integer;
  UseBluetooth: Boolean;
  BluetoothMac, BluetoothName, ComPort: String;
begin
  UseBluetooth := RGTransport.ItemIndex = 0;
  BluetoothMac := FPConfig^.KISSBluetoothMac;
  BluetoothName := FPConfig^.KISSBluetoothName;
  ComPort := Trim(CBComPort.Text);
  Speed := FPConfig^.KISSComSpeed;

  if UseBluetooth then
  begin
    if cbBluetoothDevices.ItemIndex >= 0 then
    begin
      BluetoothDevice := cbBluetoothDevices.Items[cbBluetoothDevices.ItemIndex].Split(',');
      if Length(BluetoothDevice) = 2 then
      begin
        BluetoothName := Trim(BluetoothDevice[0]);
        BluetoothMac := Trim(BluetoothDevice[1]);
      end;
    end;

    if (Length(BluetoothMac) <> 17) or
       (BluetoothMac = '00:00:00:00:00:00') then
    begin
      MessageDlg('Please select a Bluetooth device.', mtError, [mbOK], 0);
      Exit;
    end;
  end
  else
  begin
    if ComPort = '' then
    begin
      MessageDlg('Please select or enter a serial port.', mtError, [mbOK], 0);
      Exit;
    end;

    if not TryStrToInt(CBComSpeed.Text, Speed) or
      not IsSupportedKISSSpeed(Speed) then
    begin
      MessageDlg('Please select a valid baud rate.', mtError, [mbOK], 0);
      Exit;
    end;
  end;

  BeginConfigurationChange;
  FPConfig^.KISSPipe := Trim(LESocketPath.Text);
  FPConfig^.MaxChannels := SPMaxChannels.Value;
  FPConfig^.KISSUseBluetooth := UseBluetooth;
  FPConfig^.KISSBluetoothName := BluetoothName;
  FPConfig^.KISSBluetoothMac := BluetoothMac;
  FPConfig^.KISSComPort := ComPort;
  FPConfig^.KISSComSpeed := Speed;
  ApplyConfiguration;
  Close;
end;

procedure TFKiss.FormCreate(Sender: TObject);
begin
  RGTransport.Items.Clear;
  RGTransport.Items.Add('Bluetooth');
  RGTransport.Items.Add('Serial');
  RGTransport.ItemIndex := 0;

  CBComSpeed.Items.Clear;
  CBComSpeed.Items.Add('150');
  CBComSpeed.Items.Add('300');
  CBComSpeed.Items.Add('600');
  CBComSpeed.Items.Add('1200');
  CBComSpeed.Items.Add('2400');
  CBComSpeed.Items.Add('4800');
  CBComSpeed.Items.Add('9600');
  CBComSpeed.Items.Add('19200');
  CBComSpeed.Items.Add('38400');
  CBComSpeed.ItemIndex := CBComSpeed.Items.IndexOf('9600');

  // fix for wayland
  OldHeight := Height;
  OldWidth := Width;
end;

{$IFDEF UNIX}
procedure TFKiss.AddSerialPorts(const Pattern: String);
var
  SearchResult: TSearchRec;
  DeviceName: String;
begin
  if FindFirst(Pattern, faAnyFile, SearchResult) <> 0 then
    Exit;
  try
    repeat
      DeviceName := '/dev/' + SearchResult.Name;
      if CBComPort.Items.IndexOf(DeviceName) < 0 then
        CBComPort.Items.Add(DeviceName);
    until FindNext(SearchResult) <> 0;
  finally
    FindClose(SearchResult);
  end;
end;
{$ENDIF}

{$IFDEF MSWINDOWS}
function TFKiss.GetAvailableCOMPorts: TStringList;
var
  Registry: TRegistry;
  Keys: TStringList;
  I: Integer;
begin
  Result := TStringList.Create;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    begin
      Keys := TStringList.Create;
      try
        Registry.GetValueNames(Keys);
        for I := 0 to Keys.Count - 1 do
          Result.Add(Registry.ReadString(Keys[I]));
      finally
        Keys.Free;
      end;
    end;
  finally
    Registry.Free;
  end;
end;
{$ENDIF}

procedure TFKiss.PopulateSerialPorts;
{$IFDEF MSWINDOWS}
var
  Ports: TStringList;
  I: Integer;
{$ENDIF}
begin
  CBComPort.Items.Clear;
  {$IFDEF UNIX}
  AddSerialPorts('/dev/ttyUSB*');
  AddSerialPorts('/dev/ttyACM*');
  AddSerialPorts('/dev/ttyS*');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Ports := GetAvailableCOMPorts;
  try
    for I := 0 to Ports.Count - 1 do
      CBComPort.Items.Add(Ports[I]);
  finally
    Ports.Free;
  end;
  {$ENDIF}

  if (FPConfig^.KISSComPort <> '') and
     (CBComPort.Items.IndexOf(FPConfig^.KISSComPort) < 0) then
    CBComPort.Items.Add(FPConfig^.KISSComPort);

  if FPConfig^.KISSComPort <> '' then
    CBComPort.ItemIndex := CBComPort.Items.IndexOf(FPConfig^.KISSComPort)
  else if CBComPort.Items.Count > 0 then
    CBComPort.ItemIndex := 0;
end;

procedure TFKiss.FormShow(Sender: TObject);
var
  SpeedIndex: Integer;
begin
  Height := OldHeight;
  Width := OldWidth;
  PopulateSerialPorts;

  SpeedIndex := CBComSpeed.Items.IndexOf(IntToStr(FPConfig^.KISSComSpeed));
  if SpeedIndex >= 0 then
    CBComSpeed.ItemIndex := SpeedIndex
  else
    CBComSpeed.ItemIndex := CBComSpeed.Items.IndexOf('9600');

  if FPConfig^.KISSUseBluetooth then
    RGTransport.ItemIndex := 0
  else
    RGTransport.ItemIndex := 1;

  cbBluetoothDevices.Items.Clear;
  if (Length(FPConfig^.KISSBluetoothMac) = 17) and not (FPConfig^.KISSBluetoothMac = '00:00:00:00:00:00') then
  begin
    cbBluetoothDevices.Items.Add(Format('%s ,%s', [PChar(FPConfig^.KISSBluetoothName), PChar(FPConfig^.KISSBluetoothMac)]));
    cbBluetoothDevices.ItemIndex := 0;
  end;

  RGTransportClick(nil);
end;


end.

