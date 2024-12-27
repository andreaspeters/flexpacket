unit utnc;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, utypes, ExtCtrls, ButtonPanel, Spin, uini
  {$IFDEF MSWINDOWS}, Registry {$ENDIF};

type

  { TTFTNC }

  PTFPConfig = ^TFPConfig;

  TTFTNC = class(TForm)
    BPDefaultButtons: TButtonPanel;
    CBComPort: TComboBox;
    CBComSpeed: TComboBox;
    CBComBits: TComboBox;
    CBComParity: TComboBox;
    CBComStopBit: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    SPMaxChannels: TSpinEdit;
    procedure BtnCancelClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure SetConfig(Config: PTFPConfig);
    {$IFDEF MSWINDOWS}
    function GetAvailableCOMPorts: TStringList;
    {$ENDIF}
  private
  public
  end;

var
  TFTNC: TTFTNC;
  FPConfig: PTFPConfig;

implementation

{$R *.lfm}

{ TTFTNC }

procedure TTFTNC.SetConfig(Config: PTFPConfig);
var i: Byte;
    {$IFDEF UNIX}
    SearchResult: TSearchRec;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    Ports: TStringList;
    {$ENDIF}
begin
  FPConfig := Config;
  SPMaxChannels.Value := FPConfig^.MaxChannels;

  for i := 0 to CBComSpeed.Items.Count - 1 do
  begin
    if CBComSpeed.Items[i] = IntToStr(FPConfig^.ComSpeed) then
      CBComSpeed.ItemIndex := i;
  end;

  for i := 0 to CBComBits.Items.Count - 1 do
  begin
    if CBComBits.Items[i] = IntToStr(FPConfig^.ComBits) then
      CBComBits.ItemIndex := i;
  end;

  for i := 0 to CBComParity.Items.Count - 1 do
  begin
    if CBComParity.Items[i][1] = FPConfig^.ComParity then
      CBComParity.ItemIndex := i;
  end;

  for i := 0 to CBComStopBit.Items.Count - 1 do
  begin
    if CBComStopBit.Items[i] = IntToStr(FPConfig^.ComStopBit) then
      CBComStopBit.ItemIndex := i;
  end;

  CBComPort.Items.Clear;

  {$IFDEF UNIX}
   if FindFirst('/dev/ttyUSB*', faAnyFile, SearchResult) = 0 then
   begin
     repeat
       CBComPort.Items.Add('/dev/' + SearchResult.Name);
     until FindNext(SearchResult) <> 0;
     FindClose(SearchResult);
   end;
   if FindFirst('/dev/ttyS*', faAnyFile, SearchResult) = 0 then
   begin
     repeat
       CBComPort.Items.Add('/dev/' + SearchResult.Name);
     until FindNext(SearchResult) <> 0;
     FindClose(SearchResult);
   end;
   {$ENDIF}

   {$IFDEF MSWINDOWS}
   Ports := GetAvailableCOMPorts;
   if Ports.Count > 0 then
   begin
     for i := 0 to Ports.Count - 1 do
       CBComPort.Items.Add(Ports[i]);
     Ports.Free;
   end;
   {$ENDIF}
   if CBComPort.Items.Count > 0 then
     for i := 0 to CBComPort.Items.Count - 1 do
     begin
       if CBComPort.Items[i] = FPConfig^.ComPort then
         CBComPort.ItemIndex := i;
     end;
end;

{$IFDEF MSWINDOWS}
function TTFTNC.GetAvailableCOMPorts: TStringList;
var
  Registry: TRegistry;
  Ports: TStringList;
  Keys: TStringList;
  i: Integer;
begin
  Ports := TStringList.Create;
  Registry := TRegistry.Create;
  try
    Registry.RootKey := HKEY_LOCAL_MACHINE;
    if Registry.OpenKeyReadOnly('HARDWARE\DEVICEMAP\SERIALCOMM') then
    begin
      Keys := TStringList.Create;
      try
        Registry.GetValueNames(Keys);
        for i := 0 to Keys.Count - 1 do
        begin
          Ports.Add(Registry.ReadString(Keys[i]));
        end;
      finally
        Keys.Free;
      end;
    end;
  finally
    Registry.Free;
  end;
  Result := Ports;
end;
{$ENDIF}

procedure TTFTNC.BtnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TTFTNC.BtnSaveClick(Sender: TObject);
begin
  if CBComPort.ItemIndex >= 0 then
    FPConfig^.ComPort := CBComPort.Items[CBComPort.ItemIndex];
  FPConfig^.ComSpeed := StrToInt(CBComSpeed.Items[CBComSpeed.ItemIndex]);
  FPConfig^.ComBits := StrToInt(CBComBits.Items[CBComBits.ItemIndex]);
  FPConfig^.ComStopBit := StrToInt(CBComStopBit.Items[CBComStopBit.ItemIndex]);
  FPConfig^.ComParity := CBComParity.Items[CBComParity.ItemIndex][1];
  FPConfig^.MaxChannels := SPMaxChannels.Value;
  SaveConfigToFile(FPConfig);
  if MessageDlg('To apply the configuration, we have to restart FlexPacket.', mtConfirmation, [mbCancel, mbOk], 0) = mrOk then
    RestartApplication;
  Close;
end;


end.

