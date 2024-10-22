unit ax25helper;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics;

type
  { TAX25Helper }
  PtTRichMemo = ^TRichMemo;
  TAX25Helper = class(TThread)
  private
    FMRx: PtTRichMemo;
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
    constructor Create(AMRx: PtTRichMemo; AMTx: TMemo; APort: string);
    destructor Destroy; override;
    procedure TriggerSend;
    procedure SendEscape;
    procedure SendCommand(command: string);
    procedure SetChannel(channel: byte; AMRx: PtTRichMemo);
  end;

implementation

{ TAX25Helper }


constructor TAX25Helper.Create(AMRx: PtTRichMemo; AMTx: TMemo; APort: string);
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
  AddTextToMemo(FMRx^, Text);
end;

procedure TAX25Helper.UpdateTx;
var
  DataToSend: string;
  i, x: Integer;
begin
  i := 0;
  x := FMTx.Lines.Count;
  while i < FMTx.Lines.Count do
  begin
    DataToSend := FMTx.Lines[i];
    AddTextToMemo(FMRx^, DataToSend);
    FSerial.SendString(DataToSend + #13);
    inc(i);
  end;
  FMTx.Clear;
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

procedure TAX25Helper.SendCommand(command: string);
begin
  FSerial.SendString(#27 + command + #13);
end;

procedure TAX25Helper.SetChannel(channel: byte; AMRx: PtTRichMemo);
begin
  FMRx := AMrx;
  SendCommand('S '+ IntToStr(channel));
end;


end.

