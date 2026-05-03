unit uhostmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  lazsynaser, Graphics, StrUtils, utypes, RegExpr;

type
  { THostmode }

  TChannelString = array[0..10] of AnsiString;
  TChannelByte = array[0..10] of TBytes;
  TLinkStatus = array[0..2] of string;
  TChannelStatus = array[0..10] of TStatusLine;

  THostmode = class(TThread)
  private
    FSerial: TBlockSerial;
    FSendTriggered: Boolean;
    procedure ReceiveData;
    procedure SetTNCStatusMessage(msg: String);
    function ReceiveDataUntilZero: AnsiString;
    function ReceiveStringData: AnsiString;
    function ReceiveByteData:TBytes;
    function ReadWithTimeout(Ser: TBlockSerial; TimeoutMS: Integer): String;
    function ReadRawWithTimeout(TimeoutMS: Integer): RawByteString;
    function IsHostmodeReply(const Buf: RawByteString): Boolean;
    function EnterHostmode: Boolean;
    function SendInitCommand(Channel: Byte; const ACmd: string): Boolean;
  protected
    procedure Execute; override;
  public
    FPConfig: PTFPConfig;
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    ChannelByteData: TChannelByte;
    Connected: Boolean;
    constructor Create(Config: PTFPConfig);
    destructor Destroy; override;
    procedure SendG;
    procedure SendL;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Command: String);
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    procedure SendFile(const Channel: byte);
    function DecodeLinkStatus(const Text: String):TLinkStatus;
    function DecodeSendLResult(const Text: String):TStringArray;
    function ComPortExists(const APort: String): Boolean;
    function LoadTNCInit: Boolean;
  end;

implementation

{ THostmode }

constructor THostmode.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FSendTriggered := False;
  FSerial := TBlockSerial.Create;
  FSerial.LinuxLock := False;
  FreeOnTerminate := False;
  Connected := False;
end;

destructor THostMode.Destroy;
begin
  if Assigned(FSerial) then
  begin
    SendStringCommand(0, 0, 'JHOST0');
    sleep(2000);
    FSerial.CloseSocket;
    FreeAndNil(FSerial);
  end;

  inherited Destroy;
end;

procedure THostmode.SetTNCStatusMessage(msg: String);
var i: Byte;
begin
  for i:= 0 to FPConfig^.MaxChannels do
    ChannelStatus[i][9] := msg;
end;

function THostmode.ComPortExists(const APort: String): Boolean;
begin
  {$IFDEF UNIX}
  Result := FileExists(APort);
  {$ELSE}
  Result := True;
  {$ENDIF}
end;

function THostmode.ReadRawWithTimeout(TimeoutMS: Integer): RawByteString;
var
  StartTick: QWord;
  S: AnsiString;
begin
  Result := '';
  StartTick := GetTickCount64;

  while (GetTickCount64 - StartTick) < QWord(TimeoutMS) do
  begin
    S := FSerial.RecvPacket(0);

    if S <> '' then
      Result := Result + RawByteString(S);

    if Length(Result) >= 2 then
      Exit;

    Sleep(10);
  end;
end;

function THostmode.IsHostmodeReply(const Buf: RawByteString): Boolean;
var
  Ch, Code: Byte;
begin
  Result := False;

  if Length(Buf) < 2 then
    Exit;

  Ch   := Byte(Buf[1]);
  Code := Byte(Buf[2]);

  // WA8DED Hostmode:
  // Byte1 = Kanal
  // Byte2 = Code (0..7 gültig)

  if (Ch <= 31) and (Code <= 7) then
    Result := True;
end;

function THostmode.EnterHostmode: Boolean;
var
  I: Integer;
  Resp: RawByteString;
begin
  Result := False;

  for I := 1 to 5 do
  begin
    try
      FSerial.Flush;
    except
    end;

    FSerial.SendString(#17#24#13#27'JHOST1'#13);
    Sleep(300);
    Resp := ReadRawWithTimeout(700);

    if IsHostmodeReply(Resp) then
    begin
      Result := True;
      Exit;
    end;

    // Fallback different Firmware
    if Pos('HOSTMODE', UpperCase(String(Resp))) > 0 then
    begin
      Result := True;
      Exit;
    end;

    Sleep(250);
  end;
end;

procedure THostmode.Execute;
var LastSendTimeG, LastSendTimeL: Cardinal;
begin
  repeat
    SetTNCStatusMessage('TNC Init COM Port');
    if FPConfig^.ComPort <> '' then
    begin
      try
        if not ComPortExists(FPConfig^.ComPort) then
        begin
          SetTNCStatusMessage('COM Port does not exist');
          Terminate;
          Exit;
        end;
        FSerial.Connect(FPConfig^.ComPort);
        FSerial.Config(FPConfig^.ComSpeed, FPConfig^.ComBits, FPConfig^.ComParity[1], FPConfig^.ComStopBit, False, False);
      except
        on E: Exception do
        begin
          FSerial.CloseSocket;
          SetTNCStatusMessage('Can''t open COM Port');
          Terminate;
          Exit;
        end;
      end;
    end;
    sleep (200);
  until FSerial.InstanceActive;

  // Init TNC
  SetTNCStatusMessage('TNC Set Hostmode');

  if not EnterHostmode then
  begin
    SetTNCStatusMessage('TNC could not enter Hostmode');
    Terminate;
    Exit;
  end;

  Connected := True;

  if not LoadTNCInit then
  begin
    SetTNCStatusMessage('TNC could not init');
  end;
  SetCallsign;

  SetTNCStatusMessage('TNC Ready');

  LastSendTimeG := GetTickCount64;
  LastSendTimeL := GetTickCount64;

  while not Terminated do
  begin
    try
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
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Receive Data Error: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;

  Connected := False;
  Terminate;
  WaitFor;
end;

function THostmode.ReadWithTimeout(Ser: TBlockSerial; TimeoutMS: Integer): String;
var T0: QWord;
begin
  Result := '';
  T0 := GetTickCount64;

  repeat
    if Ser.CanRead(50) then
      Result := Result + Ser.RecvString(50);

    if Result <> '' then
      Exit;

    Sleep(10);
  until (GetTickCount64 - T0) > QWord(TimeoutMS);
end;

procedure THostmode.SendG;
var i: Integer;
begin
  for i:=0 to FPConfig^.MaxChannels do
    if Connected then
      SendStringCommand(i,1,'G');
end;

// get status of all channels
procedure THostmode.SendL;
var i: Byte;
begin
  for i:=1 to FPConfig^.MaxChannels do
    if Connected then
      SendStringCommand(i,1,'L');
end;

procedure THostmode.ReceiveData;
var Channel, Code, x: Byte;
    Text: AnsiString;
    StatusArray: TStringArray;
    LinkStatus: TLinkStatus;
    DataBuffer: TBytes;
begin
  if FSerial.CanRead(1000) then
  begin
    Text := '';
    Channel := FSerial.RecvByte(500);
    Code := FSerial.RecvByte(500);

    if (Channel > FPConfig^.MaxChannels) or (Code > 7) or (Code = 0) then
       Exit;

    //writeln();
    //write('Receive ');
    //Write('CH: '+IntToStr(Channel)+' ');
    //write('CO: '+IntToStr(Code)+' ');
    //write();
    try
      case Code of
        1: // Command Answer
        begin
          Text := ReceiveDataUntilZero;
          // Check if it's a state (L) result
          StatusArray := DecodeSendLResult(Text);
          if (Length(StatusArray) > 0) then
          begin
            for x := 0 to Length(StatusArray) - 1 do
            begin
              ChannelStatus[Channel][x] := StatusArray[x];
            end;
          end
          else
          begin
            if Length(Text) > 0 then
              ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[34m' + Text + #27'[0m'#13#10;
          end;
        end;
        2: // Error
        begin
          Text := ReceiveDataUntilZero;
          if Length(Text) > 0 then
          begin
            if Text = 'NO SOURCE CALLSIGN' then
            begin
              Text := Text + ' - AutoSet Callsign to: '+FPConfig^.Callsign;
              SetCallsign;
              SetTNCStatusMessage('TNC Ready');
            end;
            if Length(Text) > 0 then
              ChannelBuffer[Channel] := ChannelBuffer[Channel] + #13#10#27'[31m' + '>>> ERROR: ' + Text + #27'[0m'#13#10;
          end;
        end;
        3: // Link Status
        begin
          if (Channel) > 0 then
          begin
            Text := ReceiveDataUntilZero;
            if Length(Text) > 0 then
            begin
              ChannelBuffer[Channel] := ChannelBuffer[Channel] + #13#10#27'[32m' + '>>> LINK STATUS: ' + Text + #27'[0m'#13#10;
              LinkStatus := DecodeLinkStatus(Text);
              ChannelStatus[channel][6] := LinkStatus[0]; // Status Text CONNECTED, DISCONNECTED, etc
              ChannelStatus[channel][7] := LinkStatus[1]; // Call of the other station
              ChannelStatus[channel][8] := LinkStatus[2]; // digipeater call
            end;
          end;
        end;
        4: // Monitor Header
        begin
          Text := ReceiveDataUntilZero;
          if Length(Text) > 0 then
            ChannelBuffer[0] := ChannelBuffer[0] + Text + #13#10;
        end;
        5: // Monitor Header
        begin
          Text := ReceiveDataUntilZero;
          if Length(Text) > 0 then
            ChannelBuffer[0] := ChannelBuffer[0] + Text + #13#10;
        end;
        6: // Monitor Daten
        begin
          Text := ReceiveStringData;
          if Length(Text) > 0 then
            ChannelBuffer[0] := ChannelBuffer[0] + Text + #13#10;
        end;
        7: // Info Answer
        begin
          // if channel is in upload mode, write also in channel buffer
          if FPConfig^.Download[Channel].Enabled then
          begin
            DataBuffer := ReceiveByteData;
            if Length(DataBuffer) > 0 then
            begin
              ChannelBuffer[Channel] := ChannelBuffer[Channel] + BytesToRawString(DataBuffer);
              SetLength(ChannelByteData[Channel], Length(ChannelByteData[Channel]) + Length(DataBuffer));
              Move(DataBuffer[0], ChannelByteData[Channel][Length(ChannelByteData[Channel]) - Length(DataBuffer)], Length(DataBuffer));
            end;
          end
          else
          begin
            Text := ReceiveStringData;
            if Length(Text) > 0 then
              ChannelBuffer[Channel] := ChannelBuffer[Channel] + Text;
          end;
        end;
      end;
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Receive Data Error: ', E.Message);
        {$ENDIF}
      end;
    end;
  end;
end;

function THostmode.DecodeSendLResult(const Text: String): TStringArray;
var
  Regex: TRegExpr;
  i: Integer;
begin
  Regex := TRegExpr.Create;
  Result := TStringArray.Create;
  try
    Regex.Expression := '^(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)$';
    Regex.ModifierI := False;
    if Regex.Exec(Text) then
    begin
      SetLength(Result, Regex.SubExprMatchCount);
      for i := 1 to Regex.SubExprMatchCount do
        Result[i - 1] := Regex.Match[i];
    end
    else
    begin
      SetLength(Result, 0);
    end;
  finally
    Regex.Free;
  end;
end;


function THostmode.DecodeLinkStatus(const Text:string):TLinkStatus;
var Regex: TRegExpr;
    Status, CallSign, Digipeaters: string;
begin
  Regex := TRegExpr.Create;

  try
    // Regular Expression für verschiedene Textmuster
    Regex.Expression := '\(\d+\)\s+(CONNECTED|DISCONNECTED|BUSY|LINK RESET|LINK FAILURE|FRAME REJECT)\s+(to|fm|with)\s+([A-Z0-9\-]+)(?:\s+via\s+([A-Z0-9\-]+))?';
    Regex.ModifierI := True;

    if Regex.Exec(Text) then
    begin
      Status := Regex.Match[1];   // CONNECTED, DISCONNECTED, etc.
      CallSign := Regex.Match[3]; // {call}
      Digipeaters := Regex.Match[4]; // {digipeaters}

      Result[0] := Trim(Status);
      Result[1] := Trim(Callsign);
      Result[2] := Trim(Digipeaters);
    end;
  finally
    Regex.Free;
  end;
end;

function THostmode.ReceiveDataUntilZero: AnsiString;
var Data, i: Byte;
begin
  Result := '';
  i := 0;
  if FSerial.CanRead(500) then
  begin
    repeat
      Data := FSerial.RecvByte(500);
      if Data = 0 then
         Exit;
      Result := Result + Chr(Data);
      inc(i);
    until i = 254;
  end;
end;

function THostmode.ReceiveStringData: AnsiString;
var Data, Len, i: Byte;
begin
  Result := '';
  i := 0;
  if FSerial.CanRead(500) then
  begin
    // Channel and Code already received in the receive data procedure
    Len := FSerial.RecvByte(500);
    if len > 0 then
      repeat
        inc(i);
        Data := FSerial.RecvByte(200);
        Result := Result + Chr(Data);
      until (i = Len+1);
  end;
end;



function THostmode.ReceiveByteData:TBytes;
var i: Byte;
    Len: Integer;
begin
  Result := TBytes.Create;
  SetLength(Result, 0);
  i := 0;
  if FSerial.CanRead(500) then
  begin
    Len := FSerial.RecvByte(500)+1;
    if Len > 0 then
    begin
      SetLength(Result, Len);
      for i := 0 to Len - 1 do
      begin
        Result[i] := FSerial.RecvByte(200);
      end;
    end
    else
      SetLength(Result, 0);
  end;
end;

procedure THostmode.SendStringCommand(const Channel, Code: byte; const Command: string);
begin
  SendByteCommand(Channel, Code, TEncoding.UTF8.GetBytes(UTF8Decode(Command)));
end;

procedure THostmode.SendByteCommand(const Channel, Code: byte; const data: TBytes);
var i: Byte;
begin
  if not Connected then
    Exit;

  if FSerial.CanWrite(500) then
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
    except
      on E: Exception do
      begin
        {$IFDEF UNIX}
        writeln('Send Data Error: ' + E.Message);
        {$ENDIF}
        Exit;
      end;
    end;
  end;
end;

function THostmode.SendInitCommand(Channel: Byte; const ACmd: string): Boolean;
var I: Integer;
    Cmd, Resp: String;
begin
  Result := False;

  Cmd := Trim(ACmd);

  if Cmd = '' then
    Exit(True);

  // ignore comments
  if (Cmd[1] = ';') or (Cmd[1] = '#') then
    Exit(True);

  for I := 1 to 3 do
  begin
    repeat
      Sleep(50);
    until FSerial.CanWrite(500);

    SendStringCommand(Channel, 1, Cmd);

    Sleep(180);

    // check error
    Resp := ReadWithTimeout(FSerial, 400);

    if Pos('*', Resp) = 0 then
    begin
      Result := True;
      Exit;
    end;

    Sleep(150);
  end;
end;

function THostmode.LoadTNCInit: Boolean;
var FileHandle: TextFile;
    HomeDir, Line: string;
begin
  Result := False;
  if not Connected then
    Exit;

  // Load config file
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'\.flexpacket\';
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
    SetTNCStatusMessage('TNC Init');

    // send parameter from init file
    while not EOF(FileHandle) do
    begin
      ReadLn(FileHandle, Line);

      Line := Trim(Line);

      if Line = '' then
        Continue;

      SetTNCStatusMessage('TNC Init: ' + Line);

      if not SendInitCommand(0, Line) then
      begin
        SetTNCStatusMessage('TNC Init failed: ' + Line);
        Exit;
      end;

      Sleep(250);
    end;

    // send needed parameter
    SetTNCStatusMessage('TNC Init: Y '+IntToStr(FPConfig^.MaxChannels));
    if not SendInitCommand(0, 'Y '+IntToStr(FPConfig^.MaxChannels)) then
      Exit;

    Sleep(250);

    SetTNCStatusMessage('TNC Init: M USIC');
    if not SendInitCommand(0, 'M USIC') then
      Exit;
  finally
    CloseFile(FileHandle);
  end;

  Result := True;
end;

procedure THostmode.SetCallsign;
var i: Byte;
begin
  for i:=0 to FPConfig^.MaxChannels do
  begin
    SetTNCStatusMessage('TNC Set Callsign');
    if not SendInitCommand(i, 'I '+FPConfig^.Callsign) then
      Exit;

    Sleep(250)
  end;
end;

end.

