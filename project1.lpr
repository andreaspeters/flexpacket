program project1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, UHostmode, umycallsign, LazSerialPort,
  utnc, uansi, utypes, uinfo, uterminalsettings;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Title:='FlexPacket';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TTFTNC, TFTNC);
  Application.CreateForm(TTFMyCallsign, TFMyCallsign);
  Application.CreateForm(TTFInfo, TFInfo);
  Application.CreateForm(TTFTerminalSettings, TFTerminalSettings);
  Application.Run;
end.

