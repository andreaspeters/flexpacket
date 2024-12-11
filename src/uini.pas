unit uini;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, inifiles, utypes;

type
  PTFPConfig = ^TFPConfig;

procedure SaveConfigToFile(Config: PTFPConfig);
procedure LoadConfigFromFile(Config: PTFPConfig);

implementation

var
  HomeDir: String;

procedure SaveConfigToFile(Config: PTFPConfig);
var
  ini : TIniFile;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'\.flexpacket\';
  {$ENDIF}

  ini := TIniFile.Create(HomeDir+'/fp.ini');

  ini.WriteString('TNC', 'device', Config^.ComPort);
  ini.WriteInteger('TNC', 'speed', Config^.ComSpeed);
  ini.WriteInteger('TNC', 'bits', Config^.ComBits);
  ini.WriteString('TNC', 'parity', Config^.ComParity);
  ini.WriteInteger('TNC', 'stopbits', Config^.ComStopBit);
  ini.WriteString('TNC', 'callsign', Config^.Callsign);
  ini.WriteInteger('TNC', 'channels', Config^.MaxChannels);
  ini.WriteBool('TNC', 'enable', Config^.EnableTNC);
  ini.WriteBool('AGW', 'enable', Config^.EnableAGW);
  ini.WriteString('AGW', 'server', Config^.AGWServer);
  ini.WriteInteger('AGW', 'port', Config^.AGWServerPort);
  ini.WriteString('AGW', 'username', Config^.AGWServerUsername);
  ini.WriteString('AGW', 'password', Config^.AGWServerPassword);
  ini.WriteInteger('TERMINAL', 'fontcolor', Config^.TerminalFontColor);
  ini.WriteInteger('TERMINAL', 'fontsize', Config^.TerminalFontSize);
  ini.WriteInteger('TERMINAL', 'backgroundcolor', Config^.TerminalBGColor);
  ini.WriteString('TERMINAL', 'directory7plus', Config^.Directory7Plus);
  ini.WriteString('TERMINAL', 'directoryautobin', Config^.DirectoryAutoBin);
  ini.WriteString('TERMINAL', '7plus', Config^.Executable7Plus);
  ini.WriteString('TERMINAL', 'aprs', Config^.ExecutableAPRSMap);
end;

procedure LoadConfigFromFile(Config: PTFPConfig);
var
  ini : TIniFile;
  EXE: string;
begin
  EXE := '';

  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'\.flexpacket\';
  EXE := '.exe';
  {$ENDIF}

  // create directory structure if it does not exist
  ForceDirectories(HomeDir);
  ForceDirectories(HomeDir+'/autobin/');
  ForceDirectories(HomeDir+'/7Plus/');
  ForceDirectories(HomeDir+'/bin/');

  ini := TIniFile.Create(HomeDir+'/fp.ini');
  {$IFDEF UNIX}
  Config^.ComPort := ini.ReadString('TNC', 'device', '/dev/ttyUSB0');
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  Config^.ComPort := ini.ReadString('TNC', 'device', 'COM1');
  {$ENDIF}
  Config^.EnableTNC := ini.ReadBool('TNC', 'enable', False);
  Config^.ComSpeed := ini.ReadInteger('TNC', 'speed', 9600);
  Config^.ComBits := ini.ReadInteger('TNC', 'bits', 8);
  Config^.ComParity := ini.ReadString('TNC', 'parity', 'N');
  Config^.ComStopBit := ini.ReadInteger('TNC', 'stopbits', 1);
  Config^.Callsign := UpperCase(ini.ReadString('TNC', 'callsign', 'MYCALL-1'));
  Config^.MaxChannels := ini.ReadInteger('TNC', 'channels', 5);
  Config^.EnableAGW := ini.ReadBool('AGW', 'enable', False);
  Config^.AGWServer := ini.ReadString('AGW', 'server', 'localhost');
  Config^.AGWServerPort := ini.ReadInteger('AGW', 'port', 8000);
  Config^.AGWServerUsername := ini.ReadString('AGW', 'username', '');
  Config^.AGWServerPassword := ini.ReadString('AGW', 'password', '');
  Config^.TerminalFontColor := ini.ReadInteger('TERMINAL', 'fontcolor', 16777215);
  Config^.TerminalFontSize := ini.ReadInteger('TERMINAL', 'fontsize', 13);
  Config^.TerminalBGColor := ini.ReadInteger('TERMINAL', 'backgroundcolor', 13);
  Config^.Directory7Plus := ini.ReadString('TERMINAL', 'directory7plus', HomeDir+'7Plus/' );
  Config^.DirectoryAutoBin := ini.ReadString('TERMINAL', 'directoryautobin', HomeDir+'autobin/' );
  Config^.Executable7Plus := ini.ReadString('TERMINAL', '7plus', HomeDir+'bin/7plus' + EXE );
  Config^.ExecutableAPRSMap := ini.ReadString('TERMINAL', 'aprs', HomeDir+'bin/aprsmap' + EXE );
end;


end.

