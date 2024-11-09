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
  {$ELSE}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  // create directory if it does not exist
  ForceDirectories(HomeDir);

  ini := TIniFile.Create(HomeDir+'/fp.ini');

  ini.WriteString('TNC', 'device', Config^.ComPort);
  ini.WriteInteger('TNC', 'speed', Config^.ComSpeed);
  ini.WriteString('TNC', 'callsign', Config^.Callsign);
  ini.WriteInteger('TNC', 'channels', Config^.MaxChannels);
  ini.WriteInteger('TERMINAL', 'fontcolor', Config^.TerminalFontColor);
  ini.WriteInteger('TERMINAL', 'fontsize', Config^.TerminalFontSize);
  ini.WriteInteger('TERMINAL', 'backgroundcolor', Config^.TerminalBGColor);
end;

procedure LoadConfigFromFile(Config: PTFPConfig);
var
  ini : TIniFile;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ELSE}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  // create directory if it does not exist
  ForceDirectories(HomeDir);

  ini := TIniFile.Create(HomeDir+'/fp.ini');
  Config^.ComPort := ini.ReadString('TNC', 'device', '/dev/ttyUSB0');
  Config^.ComSpeed := ini.ReadInteger('TNC', 'speed', 9600);
  Config^.Callsign := ini.ReadString('TNC', 'callsign', 'MYCALL-1');
  Config^.MaxChannels := ini.ReadInteger('TNC', 'channels', 5);
  Config^.TerminalFontColor := ini.ReadInteger('TERMINAL', 'fontcolor', 16777215);
  Config^.TerminalFontSize := ini.ReadInteger('TERMINAL', 'fontsize', 13);
  Config^.TerminalBGColor := ini.ReadInteger('TERMINAL', 'backgroundcolor', 13);

end;

end.

