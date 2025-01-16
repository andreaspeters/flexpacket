program flexpacket;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, UHostmode, umycallsign, LazSerialPort, cmdbox, utnc, uansi,
  utypes, uinfo, uterminalsettings, uresize, uini, uaddressbook, uagwpeclient,
  uagw, ufileupload, u7plus, upipes, ukissmode, ukiss;

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
  Application.CreateForm(TTFAdressbook, TFAdressbook);
  Application.CreateForm(TFAGW, FAGW);
  Application.CreateForm(TFFileUpload, FFileUpload);
  Application.CreateForm(TF7Plus, F7Plus);
  Application.CreateForm(TFKiss, FKiss);
  Application.Run;
end.

