unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics;

type
  { THostmode }
  TChannelBuffer = array[0..4] of string;

  THostmode = class(TThread)
  private
    FSerial: TBlockSerial;
    FPort: string;
    FSendTriggered: Boolean;
    procedure ReceiveData;
    procedure SendG;

  protected
    procedure Execute; override;
  public
    constructor Create(APort: string);
    destructor Destroy; override;
    procedure SendByteCommand(channel, kind: byte; Command: string);
    function ReadChannelBuffer(channel: Byte):string;
  end;

implementation

{ THostmode }
var
  ChannelBuffer: TChannelBuffer;

constructor THostmode.Create(APort: string);
begin
  inherited Create(True);
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
  LastSendTime: Cardinal;
begin
  FSerial.Connect('/dev/ttyUSB0');
  FSerial.Config(9600, 8, 'N', 1, false, false);

  LastSendTime := GetTickCount64;

  while not Terminated do
  begin
    if (GetTickCount64 - LastSendTime) >= 2000 then
    begin
      SendG;
      LastSendTime := GetTickCount64;
    end;

    Sleep(10);
  end;

  FSerial.CloseSocket;
end;

function THostmode.ReadChannelBuffer(Channel: Byte):string;
var Text: String;
begin
  Text := ChannelBuffer[Channel];
  ChannelBuffer[Channel] := '';
  Result := Text;
end;

procedure THostmode.SendG;
begin
  writeln('Test');
  SendByteCommand(0,1,'G');
  ReceiveData;
end;

procedure THostmode.ReceiveData;
var Channel, Code: Byte;
    Len, i: Integer;
    Text: string;
begin
  if FSerial.CanRead(100) then
  begin
    Text := '';
    Channel := FSerial.RecvByte(100);
    Code := FSerial.RecvByte(100);
    Len := FSerial.RecvByte(100);

    Write(Channel);
    write(Code);
    write(Len);
    writeln();
    ChannelBuffer[Channel] := ChannelBuffer[Channel] + 'Test';
    if Len >= 0 then
    begin
      for i := 0 to Len -1 do
      begin
        Text := Chr(FSerial.RecvByte(100));
        ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text;
        write(Text);
      end;
    end;
  end;
end;

procedure THostmode.SendByteCommand(channel, kind: byte; Command: string);
var data: TBytes;
    i: Byte;
begin
  FSerial.SendByte(channel);
  FSerial.SendByte(kind);
  data := TEncoding.UTF8.GetBytes(Command);
  FSerial.SendByte(Length(data)-1);
  for i := 0 to Length(data) - 1 do
    FSerial.SendByte(data[i]);
end;

end.

