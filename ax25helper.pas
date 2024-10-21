unit ax25helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics;

type
  { TAX25Helper }

  TAX25Helper = class(TThread)
  private
    FMRx: TRichMemo;
    FMTx: TMemo;
    FSerial: TBlockSerial;
    FPort: string;
    FSendTriggered: Boolean;
    procedure UpdateRx;
    procedure UpdateTx;
    procedure AddTextToMemo(Memo: TRichMemo; Text: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AMRx: TRichMemo; AMTx: TMemo; APort: string);
    destructor Destroy; override;
    procedure TriggerSend;
    procedure SendEscape;
  end;

implementation

{ TAX25Helper }

constructor TAX25Helper.Create(AMRx: TRichMemo; AMTx: TMemo; APort: string);
begin
  inherited Create(True);
  FMRx := AMRx;
  FMTx := AMTx;
  FPort := APort;
  FSendTriggered := False;
  FSerial := TBlockSerial.Create;
  FreeOnTerminate := True;
  Resume;
end;

destructor TAX25Helper.Destroy;
begin
  FSerial.Free;
  inherited Destroy;
end;

procedure TAX25Helper.Execute;
var
  Data: string;
begin
  FSerial.Connect(FPort);
  FSerial.Config(9600, 8, 'N', 1, false, false);

  while not Terminated do
  begin
    if FSerial.CanRead(100) then
    begin
      Data := FSerial.RecvString(100);
      Synchronize(@UpdateRx);
    end;

    if FSendTriggered then
    begin
      Synchronize(@UpdateTx);
      FSendTriggered := False;
    end;

    Sleep(10);
  end;

  FSerial.CloseSocket;
end;

procedure TAX25Helper.UpdateRx;
var Text : string;
begin
  // Empfangene Daten in das Memo für empfangene Nachrichten einfügen
  Text := FSerial.RecvString(100);
  FMRx.Lines.Add(Text);
  FMRx.SelStart := FMRx.GetTextLen;
  FMRx.ScrollBy(0, FMRx.Lines.Count);
  FMRx.Refresh;
end;

procedure TAX25Helper.UpdateTx;
var
  DataToSend: string;
  i: Integer;
  HighlightedText: string;
begin
  if FMTx.Lines.Count > 0 then
  begin
    DataToSend := FMTx.Lines[0];

    FMRx.Lines.Add(DataToSend);
    ShowMessage('Anzahl der Zeichen: ' + IntToStr(FMRx.GetTextLen));
    for i := 1 to Length(DataToSend) do
    begin
      if DataToSend[i] = 'A' then
      begin
        FMRx.SetRangeColor(FMRx.GetTextLen - 2, 1, clRed);
      end;
    end;

    FMTx.Lines.Delete(0);
    FSerial.SendString(DataToSend + #13);
  end;
end;

procedure TAX25Helper.TriggerSend;
begin
  FSendTriggered := True;  // Setze das Flag, damit der Thread Daten sendet
end;

procedure TAX25Helper.AddTextToMemo(Memo: TRichMemo; Text: string);
var Segments: uansi.TGraphicArray;
begin
  Segments := uansi.ApplyANSIColor(Text);
  DisplayANSITextInMemo(Memo, Segments);
  Memo.SelStart := Memo.GetTextLen;
  Memo.ScrollBy(0, Memo.Lines.Count);
  Memo.Refresh;
end;

procedure TAX25Helper.SendEscape;
begin
  FSerial.SendString(#27);
end;

end.

