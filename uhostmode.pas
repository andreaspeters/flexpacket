unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics, StrUtils;

type
  { THostmode }
  TChannelString = array[0..4] of string;

  THostmode = class(TThread)
  private
    FSerial: TBlockSerial;
    FPort: string;
    FSendTriggered: Boolean;
    ChannelStatus: TChannelString;
    ChannelBuffer: TChannelString;
    procedure ReceiveData;
    procedure SendG;
    procedure SendL;
    function ReceiveDataUntilZero:String;
    function ReceiveDataUntilCR:String;
  protected
    procedure Execute; override;
  public
    constructor Create(APort: string);
    destructor Destroy; override;
    procedure SendByteCommand(Channel, Code: byte; Command: string);
    function ReadChannelBuffer(Channel: Byte):string;
    function GetStatus(Channel: Byte):String;
  end;

implementation

{ THostmode }

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

  // set TNC in host mode
  if FSerial.CanWrite(100) then
     FSerial.SendString(#27+'JHOST1'+#13);

  LastSendTime := GetTickCount64;

  while not Terminated do
  begin
    if (GetTickCount64 - LastSendTime) >= 2000 then
    begin
      SendG;
      //SendL; // Funktioniert ist nur für das debugen störend.
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

// get status of all channels
procedure THostmode.SendL;
var i: Byte;
begin
  for i:=1 to 4 do
    SendByteCommand(i,1,'L');
end;

procedure THostmode.ReceiveData;
var Channel, Code, Data: Byte;
    Text: String;
    StatusArray: TStringArray;
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

    case Code of
      1: // Command Answer
      begin
        Text := ReceiveDataUntilZero;
        // Check if it's a state (L) result
        StatusArray := SplitString(Text, ' ');
        if (Length(Text) = 12) and (Length(StatusArray) = 6) then
          ChannelStatus[Channel] := Text
        else
          ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text;
      end;
      2: // Error
      begin
        Text := ReceiveDataUntilZero;
        ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[31m' + '>>> ERROR: ' + Text + '<<<' + #27'[0m'
      end;
      3: // Link Status
      begin
        Text := ReceiveDataUntilZero;
        ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[32m' + '>>> LINK STATUS: ' + Text + '<<<' + #27'[0m'
      end;
      4: // Monitor Header
      begin
        Text := ReceiveDataUntilZero;
        ChannelBuffer[0] := ChannelBuffer[Channel] + Text;
      end;
      5: // Monitor Header
      begin
        Text := ReceiveDataUntilZero;
        ChannelBuffer[0] := ChannelBuffer[Channel] + Text;
      end;
      6: // Monitor Daten
      begin
        Text := ReceiveDataUntilCR;
        ChannelBuffer[0] := ChannelBuffer[Channel] + Text;
      end;
      7: // Info Answer
      begin
        Text := ReceiveDataUntilCR;
        ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text
      end;
    end;

    write(Text);
    writeln();
  end;
end;

function THostmode.ReceiveDataUntilZero:String;
var Data: Byte;
begin
  Result := '';
  repeat
    Data := FSerial.RecvByte(100);
    Result := Result + Chr(Data);
  until Data = 0;
end;

function THostmode.ReceiveDataUntilCR:String;
var Data, Len, i: Byte;
begin
  Result := '';
  i := 0;
  Len := FSerial.RecvByte(100) + 1;
  repeat
    inc(i);
    Data := FSerial.RecvByte(100);
    Result := Result + Chr(Data);
  until i = Len;
end;

function THostmode.GetStatus(Channel: Byte):String;
begin
  Result := ChannelStatus[Channel];
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

