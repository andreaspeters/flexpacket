unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics;

type
  { THostmode }
  PtTRichMemo = ^TRichMemo;

  THostmode = class(TThread)
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
    procedure SendByteCommand(channel, kind: byte; Command: string);
  end;

implementation

{ THostmode }


constructor THostmode.Create(AMRx: PtTRichMemo; AMTx: TMemo; APort: string);
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

destructor THostmode.Destroy;
begin
  FSerial.Free;
  inherited Destroy;
end;

procedure THostmode.Execute;
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

procedure THostmode.UpdateRx;
var Text : string;
begin
  // Empfangene Daten in das Memo für empfangene Nachrichten einfügen
  Text := FSerial.RecvString(100);
  AddTextToMemo(FMRx^, Text);
end;

procedure THostmode.UpdateTx;
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

procedure THostmode.TriggerSend;
begin
  FSendTriggered := True;  // Setze das Flag, damit der Thread Daten sendet
end;

procedure THostmode.AddTextToMemo(Memo: TRichMemo; Text: string);
var Segments: uansi.TGraphicArray;
begin
  Segments := uansi.ApplyANSIColor(Text);
  DisplayANSITextInMemo(Memo, Segments);
  Memo.SelStart := Memo.GetTextLen;
  Memo.ScrollBy(0, Memo.Lines.Count);
  Memo.Refresh;
end;

procedure THostmode.SendEscape;
begin
  FSerial.SendString(#27);
end;

procedure THostmode.SendCommand(command: string);
begin
  FSerial.SendString(#27 + command + #13);
end;

procedure THostmode.SetChannel(channel: byte; AMRx: PtTRichMemo);
begin
  FMRx := AMrx;
  SendCommand('S '+ IntToStr(channel));
end;

procedure THostmode.SendByteCommand(channel, kind: byte; Command: string);
var data: TBytes;
    i: Byte;
begin

  FSerial.SendByte(channel);
  FSerial.SendByte(kind);
  FSerial.SendByte(Byte(length(Command)));
  data := TEncoding.UTF8.GetBytes(Command);
  for i := 0 to Length(data) - 1 do
    FSerial.SendByte(data[i]);
end;

end.

