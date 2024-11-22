unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  lazsynaser, Graphics, StrUtils, utypes, RegExpr;

type
  { THostmode }

  TChannelString = array[0..10] of string;
  TChannelByte = array[0..10] of TBytes;
  TLinkStatus = array[0..2] of string;
  PTFPConfig = ^TFPConfig;
  TChannelStatus = array[0..10] of TStatusLine;

  THostmode = class(TThread)
  private
    FSerial: TBlockSerial;
    FSendTriggered: Boolean;
    FPConfig: PTFPConfig;
    procedure ReceiveData;
    procedure SendG;
    procedure SendL;
    function ReceiveDataUntilZero:string;
    function ReceiveStringData:string;
    function ReceiveByteData:TBytes;
    function DecodeLinkStatus(Text: string):TLinkStatus;
  protected
    procedure Execute; override;
  public
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    ChannelByteData: TChannelByte;
    constructor Create(Config: PTFPConfig);
    destructor Destroy; override;
    procedure LoadTNCInit;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Command: String);
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    procedure SendFile(const Channel: byte);
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

  Start;
end;

destructor THostmode.Destroy;
begin
  FSerial.Free;
  inherited Destroy;
end;

procedure THostmode.Execute;
var
  LastSendTimeG, LastSendTimeL: Cardinal;
begin
  repeat
    writeln('Try to open TNC at port: '+FPConfig^.ComPort);
    FSerial.Connect(FPConfig^.ComPort);
    FSerial.Config(FPConfig^.ComSpeed, 8, 'N', 1, False, False);
    sleep (200);
  until FSerial.InstanceActive;

  // init TNC
  FSerial.SendString(#17#24#13);
  FSerial.SendString(#27+'JHOST1'+#13);

  LoadTNCInit;
  SetCallsign;

  LastSendTimeG := GetTickCount64;
  LastSendTimeL := GetTickCount64;

  while not Terminated do
  begin
    ReceiveData;
    if (GetTickCount64 - LastSendTimeG) >= 1000 then
    begin
      SendG;
      LastSendTimeG := GetTickCount64;
    end;
    if (GetTickCount64 - LastSendTimeL) >= 10000 then
    begin
      SendL;
      LastSendTimeL := GetTickCount64;
    end;
    Sleep(5);
  end;

  FSerial.CloseSocket;
end;

procedure THostmode.SendG;
var i: Integer;
begin
  for i:=0 to FPConfig^.MaxChannels do
  begin
    if FPConfig^.Active[i] then
    begin
      SendStringCommand(i,1,'G');
    end;
  end;
end;

// get status of all channels
procedure THostmode.SendL;
var i: Byte;
begin
  for i:=1 to FPConfig^.MaxChannels do
  begin
    SendStringCommand(i,1,'L');
  end;
end;

procedure THostmode.ReceiveData;
var Channel, Code, x: Byte;
    Text: String;
    StatusArray: TStringArray;
    LinkStatus: TLinkStatus;
    DataBuffer: TBytes;
begin
  if FSerial.CanRead(1000) then
  begin
    Text := '';
    Channel := FSerial.RecvByte(100);
    Code := FSerial.RecvByte(100);

    if (Channel > FPConfig^.MaxChannels) or (Code > 7) or (Code = 0) then
    begin
       Exit;
    end;

    writeln();
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
        if (Length(Text) = 11) and (Length(StatusArray) = 6) then
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
        Text := ReceiveStringData;
        if Length(Text) > 0 then
          ChannelBuffer[channel] := ChannelBuffer[channel] + #27'[32m' + Text + #13#27'[0m';
        write(text);
      end;
      7: // Info Answer
      begin
        // if channel is in upload mode, write in file not in channel buffer
        if FPConfig^.Download[Channel].Enabled then
        begin
          DataBuffer := ReceiveByteData;
          if Length(DataBuffer) > 0 then
          begin
            SetLength(ChannelByteData[Channel], Length(ChannelByteData[Channel]) + Length(DataBuffer));
            Move(DataBuffer[0], ChannelByteData[Channel][Length(ChannelByteData[Channel]) - Length(DataBuffer)], Length(DataBuffer));
          end;
        end
        else
        begin
          Text := ReceiveStringData;
          if Length(Text) > 0 then
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text;
          write(text);
        end;
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
    Regex.Expression := '^\(\d+\)\s+(CONNECTED|DISCONNECTED|BUSY|LINK RESET|LINK FAILURE|FRAME REJECT)\s+(to|fm)\s+([A-Z0-9\-]+)(?:\s+via\s+([A-Z0-9\-]+))?';
    Regex.ModifierI := True;

    if Regex.Exec(Text) then
    begin
      Status := Regex.Match[1];   // CONNECTED, DISCONNECTED, etc.
      CallSign := Regex.Match[3]; // {call}
      Digipeaters := Regex.Match[4]; // {digipeaters}

      Result[0] := StringReplace(Status, ' ', '', [rfReplaceAll]);
      Result[1] := StringReplace(Callsign, ' ', '', [rfReplaceAll]);
      Result[2] := StringReplace(Digipeaters, ' ', '', [rfReplaceAll]);
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
      if Data = 0 then
         Exit;
      Result := Result + Chr(Data);
      inc(i);
    until (Data = 0) or (i = 254);
  end;
end;

function THostmode.ReceiveStringData:String;
var Data, Len, i: Byte;
begin
  Result := '';
  i := 0;
  if FSerial.CanRead(100) then
  begin
    // Channel and Code already received in the receive data procedure
    Len := FSerial.RecvByte(100) + 1;  // plus CR
    repeat
      inc(i);
      Data := FSerial.RecvByte(100);
      Result := Result + Chr(Data);
    until (i = Len) or (i = 254);
  end;
end;

function THostmode.ReceiveByteData:TBytes;
var Data, i: Byte;
    Len: Integer;
begin
  Result := TBytes.Create;
  SetLength(Result, 0);
  i := 0;
  if FSerial.CanRead(100) then
  begin
    Len := FSerial.RecvByte(100);
    SetLength(Result, Len);
    repeat
      inc(i);
      Data := FSerial.RecvByte(100);
      Result[i-1] := Data;
    until (i = Len) or (i = 254);
  end;
end;

procedure THostmode.SendStringCommand(const Channel, Code: byte; const Command: string);
begin
  SendByteCommand(Channel, Code, TEncoding.UTF8.GetBytes(UTF8Decode(Command)));
end;

procedure THostmode.SendByteCommand(const Channel, Code: byte; const data: TBytes);
var i: Byte;
begin
  if FSerial.CanWrite(100) then
  begin
    FSerial.SendByte(channel); // Send Channel
    FSerial.SendByte(Code);    // Send Info/Cmd

    // Code:
    // 1 = Command
    // 0 = Data
    // Send Filesize
    case Code of
       0: FSerial.SendByte(Length(data));
       1: FSerial.SendByte(Length(data)-1);
    end;

    // Send Data
    for i := 0 to Length(data)-1 do
    begin
       FSerial.SendByte(data[i]);
    end;

    // If it is not a command, then send CR
    if Code = 0 then
    begin
       FSerial.SendByte(13);
    end;

  end;
end;

procedure THostmode.SendFile(const Channel: byte);
const
  ChunkSize = 128;
var
  FileStream: TFileStream;
  Buffer: TBytes;
  FileSize: Int64;
  BytesRead, i: Integer;
begin
  Buffer := TBytes.Create;
  if (FSerial.CanWrite(100)) and (Length(FPConfig^.Upload[Channel].FileName) > 0) then
  begin
    try
      FileStream := TFileStream.Create(FPConfig^.Upload[Channel].FileName, fmOpenRead or fmShareDenyWrite);
      try
        FileSize := FileStream.Size;
        SetLength(Buffer, ChunkSize);
        FSerial.SendByte(channel);  // Send Channel
        FSerial.SendByte(0);        // Send Info Code
        FSerial.SendByte(FileSize); // Send Length

        // read data from file until all data was send
        repeat
          BytesRead := FileStream.Read(Buffer[0], ChunkSize);

          if BytesRead > 0 then
          begin
            SetLength(Buffer, BytesRead);
            for i := 0 to Length(Buffer) do
              FSerial.SendByte(Buffer[i]);
            SetLength(Buffer, ChunkSize);
          end;
        until BytesRead = 0;
      finally
        FileStream.Free;
      end;
    except
      on E: Exception do
      begin
        ShowMessage('Could not read file: ' + E.Message);
        Exit;
      end;
    end;
  end;
end;

procedure THostmode.LoadTNCInit;
var FileHandle: TextFile;
    HomeDir, Line: string;
begin
  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  AssignFile(FileHandle, HomeDir + '/tnc_init');

  // write init file if it does not exist
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
      WriteLn(FileHandle, '@D 0');
      WriteLn(FileHandle, '@T2 500');
      WriteLn(FileHandle, '@T3 30000');
    finally
      CloseFile(FileHandle);
    end;
  end;

  Reset(FileHandle);
  try
    if FSerial.CanWrite(100) then
    begin
      // send needed parameter
      SendStringCommand(0,1,'Y '+IntToStr(FPConfig^.MaxChannels));
      SendStringCommand(0,1,'M USIC');

      // send parameter from init file
      while not EOF(FileHandle) do
      begin
        Readln(FileHandle, Line);
        SendStringCommand(0,1,Line);
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
    SendStringCommand(i,1,'I '+FPConfig^.Callsign);
end;

end.

