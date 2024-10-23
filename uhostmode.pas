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
    procedure SendByteCommand(Channel, Code: byte; Command: string);
    function ReadChannelBuffer(Channel: Byte):string;
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
  FSerial.Config(9600, 8, 'N', 1, True, false);

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
  if Length(ChannelBuffer[Channel]) > 0 then
  begin
    Text := ChannelBuffer[Channel];
    ChannelBuffer[Channel] := '';
    Result := Text;
  end;
end;

procedure THostmode.SendG;
begin
  SendByteCommand(0,1,'G');
  ReceiveData;
end;

procedure THostmode.ReceiveData;
var Channel, Code, Data: Byte;
    Len, i: Integer;
    Text: string;
begin
  if FSerial.CanRead(100) then
  begin
    Text := '';
    Channel := FSerial.RecvByte(100);
    Code := FSerial.RecvByte(100);

    write('Receive ');
    Write(Channel);
    write(Code);
    write();

    if Code > 0 then
    begin
      repeat
      Data := FSerial.RecvByte(100);
      if Data <> 0 then
      begin
        Text := Chr(Data);
        ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text;
        write(Text);
      end;
      until Data = 0;
    end;
    writeln();
  end;
end;

procedure THostmode.SendByteCommand(Channel, Code: byte; Command: string);
var data: TBytes;
    i: Byte;
begin
  FSerial.SendByte(channel); // Send Channel
  FSerial.SendByte(Code);    // Send Info/Cmd
  data := TEncoding.UTF8.GetBytes(Command);


  write('Send ');
  Write(Channel);
  write(Code);


  if Code = 1 then
  begin
    write(Length(data)-1);
    FSerial.SendByte(Length(data)-1);
  end
  else
  begin
    write(Length(data));
    FSerial.SendByte(Length(data));
  end;

  write();

  // Send Data
  for i := 0 to Length(data)-1 do
  begin
    write(Chr(data[i]));
    FSerial.SendByte(data[i]);
  end;

  // If it is not a command, then send CR
  if Code = 0 then
  begin
    write('<CR>');
    FSerial.SendByte(13);
  end;

  writeln();

end;

end.

