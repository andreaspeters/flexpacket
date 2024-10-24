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
  utnc, uansi, utypes;

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TTFTNC, TFTNC);
  Application.CreateForm(TTFMyCallsign, TFMyCallsign);
  Application.Run;
end.

