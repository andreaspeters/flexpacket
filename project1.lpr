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
  Forms, UMain, uchannel, AX25Helper, umonitor, umycallsign
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFMain, FMain);
  Application.CreateForm(TFChannel, FChannel);
  Application.CreateForm(TTFMonitor, TFMonitor);
  Application.CreateForm(TTFMyCallsign, TFMyCallsign);
  Application.Run;
end.

