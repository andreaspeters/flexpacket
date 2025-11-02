unit uini;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, inifiles, utypes, base64;

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
  ini.WriteBool('KISS', 'enable', Config^.EnableKISS);
  ini.WriteString('KISS', 'pipe', Config^.KissPipe);
  ini.WriteBool('AGW', 'enable', Config^.EnableAGW);
  ini.WriteString('AGW', 'server', Config^.AGWServer);
  ini.WriteInteger('AGW', 'port', Config^.AGWServerPort);
  ini.WriteString('AGW', 'username', Config^.AGWServerUsername);
  ini.WriteString('AGW', 'password', Config^.AGWServerPassword);
  ini.WriteInteger('TERMINAL', 'fontcolor', Config^.TerminalFontColor);
  ini.WriteInteger('TERMINAL', 'fontsize', Config^.TerminalFontSize);
  ini.WriteString('TERMINAL', 'fontname', Config^.TerminalFontName);
  ini.WriteInteger('TERMINAL', 'backgroundcolor', Config^.TerminalBGColor);
  ini.WriteString('TERMINAL', 'directory7plus', Config^.Directory7Plus);
  ini.WriteString('TERMINAL', 'directoryautobin', Config^.DirectoryAutoBin);
  ini.WriteString('TERMINAL', 'directorymail', Config^.DirectoryMail);
  ini.WriteString('TERMINAL', '7plus', Config^.Executable7Plus);
  ini.WriteString('TERMINAL', 'aprs', Config^.ExecutableAPRSMap);
  ini.WriteString('TERMINAL', 'forms', Config^.ExecutableForms);
  ini.WriteString('TERMINAL', 'signature', EncodeStringBase64(Config^.TerminalSignature));
  ini.WriteInteger('TERMINAL', 'height', Config^.TerminalHeight);
  ini.WriteBool('TERMINAL', 'toolbarbig', Config^.TerminalToolbarBig);
  ini.WriteInteger('MAIN', 'width', Config^.MainWidth);
  ini.WriteInteger('MAIN', 'height', Config^.MainHeight);
  ini.WriteInteger('MAIN', 'posx', Config^.MainX);
  ini.WriteInteger('MAIN', 'posy', Config^.MainY);
  ini.WriteInteger('MAIL', 'width', Config^.MailWidth);
  ini.WriteInteger('MAIL', 'height', Config^.MailHeight);
  ini.WriteInteger('MAIL', 'posx', Config^.MailX);
  ini.WriteInteger('MAIL', 'posy', Config^.MailY);
  ini.WriteBool('MAIL', 'fontbold', Config^.MailFontBold);
  ini.WriteInteger('CONVERS', 'backgroundcolor', Config^.ConversBGColor);
  ini.WriteInteger('CONVERS', 'fontcolor', Config^.ConversFontColor);
  ini.WriteInteger('CONVERS', 'posx', Config^.ConversX);
  ini.WriteInteger('CONVERS', 'posy', Config^.ConversY);
  ini.WriteInteger('CONVERS', 'fontsize', Config^.ConversFontSize);
  ini.WriteString('CONVERS', 'fontname', Config^.ConversFontName);

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
  Config^.EnableTNC := ini.ReadBool('TNC', 'enable', True);
  Config^.ComSpeed := ini.ReadInteger('TNC', 'speed', 9600);
  Config^.ComBits := ini.ReadInteger('TNC', 'bits', 8);
  Config^.ComParity := ini.ReadString('TNC', 'parity', 'N');
  Config^.ComStopBit := ini.ReadInteger('TNC', 'stopbits', 1);
  Config^.Callsign := UpperCase(ini.ReadString('TNC', 'callsign', 'MYCALL-1'));
  Config^.MaxChannels := ini.ReadInteger('TNC', 'channels', 5);
  Config^.EnableKISS := ini.ReadBool('KISS', 'enable', False);
  Config^.KissPipe := ini.ReadString('KISS', 'pipe', '/tmp/tfkiss_socket' );
  Config^.EnableAGW := ini.ReadBool('AGW', 'enable', False);
  Config^.AGWServer := ini.ReadString('AGW', 'server', 'localhost');
  Config^.AGWServerPort := ini.ReadInteger('AGW', 'port', 8000);
  Config^.AGWServerUsername := ini.ReadString('AGW', 'username', '');
  Config^.AGWServerPassword := ini.ReadString('AGW', 'password', '');
  Config^.TerminalFontColor := ini.ReadInteger('TERMINAL', 'fontcolor', 16777215);
  Config^.TerminalFontSize := ini.ReadInteger('TERMINAL', 'fontsize', 13);
  Config^.TerminalFontName := ini.ReadString('TERMINAL', 'fontname', 'Courier New');
  Config^.TerminalSignature := DecodeStringBase64(ini.ReadString('TERMINAL', 'signature', ''));
  Config^.TerminalBGColor := ini.ReadInteger('TERMINAL', 'backgroundcolor', 13);
  Config^.Directory7Plus := ini.ReadString('TERMINAL', 'directory7plus', HomeDir+'7Plus/' );
  Config^.DirectoryAutoBin := ini.ReadString('TERMINAL', 'directoryautobin', HomeDir+'autobin/' );
  Config^.DirectoryMail := ini.ReadString('TERMINAL', 'directorymail', HomeDir+'mail/' );
  Config^.Executable7Plus := ini.ReadString('TERMINAL', '7plus', HomeDir+'bin/7plus' + EXE );
  Config^.ExecutableAPRSMap := ini.ReadString('TERMINAL', 'aprs', HomeDir+'bin/aprsmap' + EXE );
  Config^.ExecutableForms := ini.ReadString('TERMINAL', 'forms', HomeDir+'bin/fpforms' + EXE );
  Config^.TerminalHeight := ini.ReadInteger('TERMINAL', 'height', 400);
  Config^.TerminalToolbarBig := ini.ReadBool('TERMINAL', 'toolbarbig', True);
  Config^.MainWidth := ini.ReadInteger('MAIN', 'width', 1137);
  Config^.MainHeight := ini.ReadInteger('MAIN', 'height', 716);
  Config^.MainX := ini.ReadInteger('MAIN', 'posx', 0);
  Config^.MainY := ini.ReadInteger('MAIN', 'posy', 0);
  Config^.MailWidth := ini.ReadInteger('MAIL', 'width', 924);
  Config^.MailHeight := ini.ReadInteger('MAIL', 'height', 924);
  Config^.MailX := ini.ReadInteger('MAIL', 'posx', 0);
  Config^.MailY := ini.ReadInteger('MAIL', 'posy', 0);
  Config^.MailFontBold := ini.ReadBool('MAIL', 'fontbold', False);
  Config^.ConversBGColor := ini.ReadInteger('CONVERS', 'backgroundcolor', 15197667);
  Config^.ConversFontColor := ini.ReadInteger('CONVERS', 'fontcolor', 10257508);
  Config^.ConversX := ini.ReadInteger('CONVERS', 'posx', 0);
  Config^.ConversY := ini.ReadInteger('CONVERS', 'posy', 0);
  Config^.ConversFontSize := ini.ReadInteger('CONVERS', 'fontsize', 11);
  Config^.ConversFontName := ini.ReadString('CONVERS', 'fontname', 'Courier New');

  if not FileExists(HomeDir+'/fp.ini') then
    SaveConfigToFile(Config);
end;


end.

