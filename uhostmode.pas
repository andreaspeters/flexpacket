unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, StdCtrls, ExtCtrls,
  lazsynaser, uansi, RichMemo, Graphics, StrUtils, utypes, RegExpr;

type
  { THostmode }

  TChannelString = array[0..10] of string;
  TLinkStatus = array[0..2] of string;
  PTFPConfig = ^TFPConfig;
  TChannelStatus = array[0..10] of TStatusLine;

  THostmode = class(TThread)
  private
    FSerial: TBlockSerial;
    FPort: string;
    FSendTriggered: Boolean;
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    FPConfig: PTFPConfig;
    procedure ReceiveData;
    procedure SendG;
    procedure SendL;
    function ReceiveDataUntilZero:string;
    function ReceiveDataUntilCR:string;
    function DecodeLinkStatus(Text: string):TLinkStatus;
  protected
    procedure Execute; override;
  public
    constructor Create(Config: PTFPConfig);
    destructor Destroy; override;
    procedure SendByteCommand(Channel, Code: byte; Command: string);
    function ReadChannelBuffer(Channel: Byte):string;
    function GetStatus(Channel: Byte):TStatusLine;
    procedure LoadTNCInit;
    procedure SetCallsign;
  end;

implementation

{ THostmode }

constructor THostmode.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FSendTriggered := False;
  FSerial := TBlockSerial.Create;
  FreeOnTerminate := True;

  if Length(FPConfig^.ComPort) <= 0 then
    ShowMessage('Please configure the TNC Com Port');

  Resume;
end;

destructor THostmode.Destroy;
begin
  FSerial.Free;
  inherited Destroy;
end;

procedure THostmode.Execute;
var
  LastSendTimeG, LastSendTimeL: Cardinal;
  i: Integer;
begin
  FSerial.Connect(FPConfig^.ComPort);
  FSerial.Config(FPConfig^.ComSpeed, 8, 'N', 1, false, false);

  // init TNC
  repeat
    if FSerial.CanWrite(1000) then
    begin
      FSerial.SendString(#17#24#13);
      FSerial.SendString(#27+'JHOST1'+#13);
    end;
  until FSerial.RecvByte(100) <> 13;


  LoadTNCInit;
  SetCallsign;

  LastSendTimeG := GetTickCount64;
  LastSendTimeL := GetTickCount64;

  while not Terminated do
  begin
    if (GetTickCount64 - LastSendTimeG) >= 2000 then
    begin
      SendG;
      LastSendTimeG := GetTickCount64;
    end;
    if (GetTickCount64 - LastSendTimeL) >= 20000 then
    begin
      SendL;
      LastSendTimeL := GetTickCount64;
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
var i: Integer;
begin
  for i:=0 to FPConfig^.MaxChannels do
  begin
    if FPConfig^.Active[i] then
    begin
      SendByteCommand(i,1,'G');
      ReceiveData;
      sleep(20);
    end;
  end;
end;

// get status of all channels
procedure THostmode.SendL;
var i: Byte;
begin
  for i:=1 to FPConfig^.MaxChannels do
  begin
    if FPConfig^.Connected[i] then
    begin
      SendByteCommand(i,1,'L');
      ReceiveData;
      sleep(20);
    end;
  end;
end;

procedure THostmode.ReceiveData;
var Channel, Code, Data, x: Byte;
    Text: String;
    StatusArray: TStringArray;
    LinkStatus: TLinkStatus;
begin
  if FSerial.CanRead(100) then
  begin
    Text := '';
    Channel := FSerial.RecvByte(100);
    Code := FSerial.RecvByte(100);

    if (Channel > FPConfig^.MaxChannels) or (Code > 7) or (Code = 0) then
    begin
       Exit;
    end;

    write('Receive ');
    Write('CH: '+IntToStr(Channel)+' ');
    write('CO: '+IntToStr(Code)+' ');
    write();

    case Code of
      1: // Command Answer
      begin
        Text := ReceiveDataUntilZero;
        // Check if it's a state (L) result
        StatusArray := SplitString(Text, ' ');
        if (Length(Text) = 12) and (Length(StatusArray) = 6) then
        begin
          for x := 0 to 5 do
          begin
            ChannelStatus[Channel][x+1] := StatusArray[x];
          end;
        end
        else
        begin
          if Length(Text) > 0 then
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[34m' + Text + #13#27'[0m';
        end;
        write(text);
      end;
      2: // Error
      begin
        Text := ReceiveDataUntilZero;
        if Length(Text) > 0 then
          ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[31m' + '>>> ERROR: ' + Text + #13#27'[0m';
        write(text);
      end;
      3: // Link Status
      begin
        if (Channel) > 0 then
        begin
          Text := ReceiveDataUntilZero;
          if Length(Text) > 0 then
          begin
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[32m' + '>>> LINK STATUS: ' + Text + #13#27'[0m';
            LinkStatus := DecodeLinkStatus(Text);
            ChannelStatus[channel][6] := LinkStatus[0]; // Status Text CONNECTED, DISCONNECTED, etc
            ChannelStatus[channel][7] := LinkStatus[1]; // Call of the other station
            ChannelStatus[channel][8] := LinkStatus[2]; // digipeater call
          end;
          write(text);
        end;
      end;
      4: // Monitor Header
      begin
        Text := ReceiveDataUntilZero;
        if Length(Text) > 0 then
          ChannelBuffer[channel] := ChannelBuffer[channel] + #27'[32m' + Text + #13#27'[0m';
        write(text);
      end;
      5: // Monitor Header
      begin
        Text := ReceiveDataUntilZero;
        if Length(Text) > 0 then
          ChannelBuffer[channel] := ChannelBuffer[channel] + #27'[32m' + Text + #13#27'[0m';
        write(text);
      end;
      6: // Monitor Daten
      begin
        Text := ReceiveDataUntilCR;
        if Length(Text) > 0 then
          ChannelBuffer[channel] := ChannelBuffer[channel] + #27'[32m' + Text + #13#27'[0m';
        write(text);
      end;
      7: // Info Answer
      begin
        Text := ReceiveDataUntilCR;
        if Length(Text) > 0 then
          ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[39m' + Text + #27'[0m';
        write(text);
      end;
    end;

    writeln();
  end;
end;

function THostmode.DecodeLinkStatus(Text:string):TLinkStatus;
var Regex: TRegExpr;
    Status, CallSign, Digipeaters: string;
begin
  Regex := TRegExpr.Create;

  try
    // Regular Expression für verschiedene Textmuster
    Regex.Expression := '\(\{\d+\}\) (BUSY|CONNECTED|DISCONNECTED|LINK RESET|LINK FAILURE|FRAME REJECT).* (fm|to) (\w+)(?: via (\w+))?';

    if Regex.Exec(Text) then
    begin
      Status := Regex.Match[1];   // CONNECTED, DISCONNECTED, etc.
      CallSign := Regex.Match[3]; // {call}
      Digipeaters := Regex.Match[4]; // {digipeaters}

      writeln('Status: ', Status);
      writeln('CallSign: ', CallSign);
      writeln('Digipeaters: ', Digipeaters);

      Result[0] := Status;
      Result[1] := Callsign;
      Result[2] := Digipeaters;
    end;
  finally
    Regex.Free;
  end;
end;


function THostmode.ReceiveDataUntilZero:String;
var Data, i: Byte;
begin
  Result := '';
  i := 0;
  if FSerial.CanRead(100) then
  begin
    repeat
      Data := FSerial.RecvByte(100);
      Result := Result + Chr(Data);
      inc(i);
    until (Data = 0) or (i = 254);
  end;
end;

function THostmode.ReceiveDataUntilCR:String;
var Data, Len, i: Byte;
begin
  Result := '';
  i := 0;
  if FSerial.CanRead(100) then
  begin
    Len := FSerial.RecvByte(100) + 1;
    repeat
      inc(i);
      Data := FSerial.RecvByte(100);
      Result := Result + Chr(Data);
    until (i = Len) or (i = 254) or (Chr(Data) = #13);
  end;
end;

function THostmode.GetStatus(Channel: Byte):TStatusLine;
var i: Byte;
begin
  // 0 = Number of link status messages not yet displayed)
  // 1 = Number of receive frames not yet displayed
  // 2 = Number of send frames not yet transmitted
  // 3 = Number of transmitted frames not yet acknowledged
  // 4 = Number of tries on current operation
  // 5 = Link state
  // 6 = Status Text (CONNECTED, DISCONNECTED, etc
  // 7 = The CALL of the other station
  // 8 = call of the digipeater
  for i := 0 to 8 do
  begin
    Result[i] := ChannelStatus[Channel][i];
  end;
end;

procedure THostmode.SendByteCommand(Channel, Code: byte; Command: string);
var data: TBytes;
    i: Byte;
begin
  if FSerial.CanWrite(100) then
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
end;


procedure THostmode.LoadTNCInit;
var FileHandle: TextFile;
    HomeDir, Line: string;
    i: Byte;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ELSE}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  AssignFile(FileHandle, HomeDir + '/tnc_init');

  if not FileExists(HomeDir + '/tnc_init') then
  begin
    Rewrite(FileHandle);
    try
      WriteLn(FileHandle, 'T 50');
      WriteLn(FileHandle, 'X 1');
      WriteLn(FileHandle, 'O 1');
      WriteLn(FileHandle, 'F 6');
      WriteLn(FileHandle, 'P 20');
      WriteLn(FileHandle, 'W 10');
      WriteLn(FileHandle, 'K 1');
      WriteLn(FileHandle, 'Y 4');
      WriteLn(FileHandle, '@D 0');
      WriteLn(FileHandle, '@T2 500');
      WriteLn(FileHandle, '@T3 30000');
      WriteLn(FileHandle, 'U 1 ** this is station MYCALL **');
      WriteLn(FileHandle, 'M USIC');
    finally
      CloseFile(FileHandle);
    end;
  end;

  Reset(FileHandle);
  try
    if FSerial.CanWrite(100) then
    begin
      while not EOF(FileHandle) do
      begin
        Readln(FileHandle, Line);
        for i:=0 to FPConfig^.MaxChannels do
          SendByteCommand(i,1,Line);
        ReceiveData;
      end;
    end;
  finally
    CloseFile(FileHandle);
  end;
end;

procedure THostmode.SetCallsign;
var i: Byte;
begin
  for i:=0 to FPConfig^.MaxChannels do
    SendByteCommand(i,1,'I '+FPConfig^.Callsign);
end;

end.

