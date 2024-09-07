unit uchannel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, SynEdit,
  SynPluginSyncroEdit;

type

  { TFChannel }

  TFChannel = class(TForm)
    StaticText1: TStaticText;
  private

  public

  end;

var
  FChannel: TFChannel;

implementation

{$R *.lfm}

end.

