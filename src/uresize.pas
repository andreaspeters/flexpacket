unit uresize;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, ComCtrls,
  Buttons, ExtCtrls, StdCtrls;

type
  TControlInfo = record
    OrigLeft, OrigTop, OrigWidth, OrigHeight: Integer;
  end;

var
  ControlInfoList: array of TControlInfo;

procedure StoreOriginalSizes(AControl: TWinControl);
procedure ResizeControl(AControl: TControl; scaleFactorWidth, scaleFactorHeight, scaleFactor: Double);
function Min(a, b: Double): Double; overload;
function Min(A, B: Integer): Integer; overload;

implementation


procedure StoreOriginalSizes(AControl: TWinControl);
var
  i: Integer;
  infoIndex: Integer;
begin
  if AControl = nil then
    Exit;

  if AControl.Tag < 0 then
    Exit;

  infoIndex := Length(ControlInfoList);
  SetLength(ControlInfoList, infoIndex + AControl.ControlCount);

  for i := 0 to AControl.ControlCount - 1 do
  begin
    ControlInfoList[infoIndex].OrigLeft := AControl.Controls[i].Left;
    ControlInfoList[infoIndex].OrigTop := AControl.Controls[i].Top;
    ControlInfoList[infoIndex].OrigWidth := AControl.Controls[i].Width;
    ControlInfoList[infoIndex].OrigHeight := AControl.Controls[i].Height;

    AControl.Controls[i].Tag := infoIndex;

    Inc(infoIndex);

    if AControl.Controls[i] is TWinControl then
      StoreOriginalSizes(TWinControl(AControl.Controls[i]));
  end;

  SetLength(ControlInfoList, infoIndex);
end;

procedure ResizeControl(AControl: TControl; scaleFactorWidth, scaleFactorHeight, scaleFactor: Double);
var
  i: Integer;
begin
  if AControl = nil then
    Exit;

  if (AControl.Tag < 0) or (AControl.Tag >= Length(ControlInfoList)) then
    Exit;

  if (AControl is TToolBar) or (AControl is TImage) or (AControl is TBitBtn) or (AControl is TLabel)then
    Exit;

  AControl.Top := Round(ControlInfoList[AControl.Tag].OrigTop * scaleFactorHeight);
  AControl.Left := Round(ControlInfoList[AControl.Tag].OrigLeft * scaleFactorWidth);

  AControl.Width := Round(ControlInfoList[AControl.Tag].OrigWidth * scaleFactorWidth);
  AControl.Height := Round(ControlInfoList[AControl.Tag].OrigHeight * scaleFactorHeight);

  if AControl is TWinControl then
  begin
    for i := 0 to TWinControl(AControl).ControlCount - 1 do
      if Assigned(TWinControl(AControl).Controls[i]) then
        ResizeControl(TWinControl(AControl).Controls[i], scaleFactorWidth, scaleFactorHeight, scaleFactor);
  end;
end;


function Min(A, B: Integer): Integer; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Min(A, B: Double): Double; overload;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;


end.

