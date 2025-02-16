unit uCmdBoxCustom;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uCmdBox, Controls;

type
  TCmdBoxCustom = class(TCmdBox)
  private
    procedure WMMouseWheel(Sender: TObject; Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure CopyToClipBoard;
  protected
  public
    constructor Create(AOwner: TComponent); override;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Custom', [TCmdBoxCustom]);
end;

{ TCmdBoxCustom }

constructor TCmdBoxCustom.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  OnMouseWheel := @WMMouseWheel;
end;


procedure TCmdBoxCustom.CopyToClipBoard;
begin
  writeln('test');
  writeln(FStringBuffer.GetPartstring(1, 3));
end;

procedure TCmdBoxCustom.WMMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if WheelDelta > 0 then
    if Self.TopLine = 0 then
      Self.TopLine := 0
    else
      Self.TopLine := Self.TopLine - 1
  else
    Self.TopLine := Self.TopLine + 1;
  Handled := True
end;


end.

