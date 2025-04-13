unit ukissmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  Graphics, utypes, RegExpr, uhostmode, Sockets{$IFDEF UNIX}, BaseUnix{$ENDIF};

type
  { TKISSMode }

  TKISSMode = class(THostmode)
  private
    FSocket: Integer;
    procedure ReceiveData;
    procedure WriteByteToSocket(const Data: Byte);
    function ReceiveDataUntilZero:string;
    function ReceiveStringData:string;
    function ReceiveByteData:TBytes;
    function ReadByteFromSocket:Byte;
  protected
    procedure Execute; override;
  public
    destructor Destroy; override;
    procedure SendG;
    procedure SendL;
    procedure LoadTNCInit;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Command: string);
    procedure SendByteCommand(const Channel, Code: byte; const data: TBytes);
  end;

implementation

{ TKISSMode }

destructor TKISSMode.Destroy;
begin
  Connected := False;
  FSocket := 0;
  inherited Destroy;
end;

{$IFDEF UNIX}
procedure TKISSMode.Execute;
var
  LastSendTimeG, LastSendTimeL: Cardinal;
  Addr: TUnixSockAddr;
  Data: TBytes;
  i: Byte;
  Str: string;
  Flags: Integer;
begin
  FSocket := fpSocket(AF_UNIX, SOCK_STREAM, 0);
  if FSocket < 0 then
    Writeln('TFKiss is not yet started');

  FillChar(Addr, SizeOf(Addr), 0);
  Addr.family := AF_UNIX;
  StrPCopy(Addr.path, FPConfig^.KISSPipe);

  if fpConnect(FSocket, @Addr, SizeOf(Addr)) < 0 then
    Writeln('Could not Connect to TFKISS');

  Flags := FpFcntl(FSocket, F_GETFL, 0);
  FpFcntl(FSocket, F_SETFL, Flags or O_NONBLOCK);


  str := #17#24#13#27+'JHOST1'+#13;
  Data := TBytes(str);
  for i := 0 to Length(Data)-1 do
    WriteByteToSocket(Data[i]);

  Connected := True;

  SetCallsign;
  LoadTNCInit;

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
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
procedure TKISSMode.Execute;
begin
end;
{$ENDIF}

procedure TKISSMode.SendG;
var i: Integer;
begin
  for i:=0 to FPConfig^.MaxChannels do
    if Connected then
      SendStringCommand(i,1,'G');
end;

// get status of all channels
procedure TKISSMode.SendL;
var i: Byte;
begin
  for i:=1 to FPConfig^.MaxChannels do
    if Connected then
      SendStringCommand(i,1,'L');
end;

procedure TKISSMode.ReceiveData;
var Channel, Code, x: Byte;
    Text: String;
    StatusArray: TStringArray;
    LinkStatus: TLinkStatus;
    DataBuffer: TBytes;
begin
  Text := '';
  Channel := ReadByteFromSocket;
  Code := ReadByteFromSocket;


  if (Channel > FPConfig^.MaxChannels) or (Code > 7) or (Code = 0) then
     Exit;

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
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + #27'[34m' + Text + #13#27'[0m';
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
          end;
          ChannelBuffer[Channel] := ChannelBuffer[Channel] + #13#27'[31m' + '>>> ERROR: ' + Text + #27'[0m'#13;
        end;
      end;
      3: // Link Status
      begin
        if (Channel) > 0 then
        begin
          Text := ReceiveDataUntilZero;
          if Length(Text) > 0 then
          begin
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + #13#27'[32m' + '>>> LINK STATUS: ' + Text + #27'[0m'#13;
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
          ChannelBuffer[0] := ChannelBuffer[0] + Text + #13;
      end;
      5: // Monitor Header
      begin
        Text := ReceiveDataUntilZero;
        if Length(Text) > 0 then
          ChannelBuffer[0] := ChannelBuffer[0] + Text + #13;
      end;
      6: // Monitor Daten
      begin
        Text := ReceiveStringData;
        if Length(Text) > 0 then
          ChannelBuffer[0] := ChannelBuffer[0] + Text + #13;
      end;
      7: // Info Answer
      begin
        // if channel is in upload mode, write in file not in channel buffer
        if FPConfig^.Download[Channel].Enabled then
        begin
          DataBuffer := ReceiveByteData;
          if Length(DataBuffer) > 0 then
          begin
            ChannelBuffer[Channel] := ChannelBuffer[Channel] + TEncoding.UTF8.GetString(DataBuffer);
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



function TKISSMode.ReceiveDataUntilZero:String;
var Data, i: Byte;
begin
  Result := '';
  i := 0;
  repeat
    Data := ReadByteFromSocket;
    if Data = 0 then
      Exit;
    Result := Result + Chr(Data);
    inc(i);
  until i = 254;
end;

function TKISSMode.ReceiveStringData:String;
var Data, Len, i: Byte;
begin
  Result := '';
  i := 0;
  // Channel and Code already received in the receive data procedure
  Len := ReadByteFromSocket + 1;
  repeat
    inc(i);
    Data := ReadByteFromSocket;
    Result := Result + UTF8Encode(Chr(Data));
  until (i = Len) or (i = 254);
end;

function TKISSMode.ReceiveByteData:TBytes;
var i: Byte;
    Len: Integer;
begin
  Result := TBytes.Create;
  SetLength(Result, 0);
  i := 0;
  Len := ReadByteFromSocket;
  if Len > 0 then
  begin
    SetLength(Result, Len);
    for i := 0 to Len - 1 do
    begin
      Result[i] := ReadByteFromSocket;
    end;
  end
  else
    SetLength(Result, 0);
end;

procedure TKISSMode.SendStringCommand(const Channel, Code: byte; const Command: string);
begin
  SendByteCommand(Channel, Code, TEncoding.UTF8.GetBytes(Command));
  //TEncoding.UTF8.GetBytes(UTF8Decode())
end;



procedure TKISSMode.SendByteCommand(const Channel, Code: byte; const data: TBytes);
var i: Byte;
begin
  if not Connected then
    Exit;

  WriteByteToSocket(Channel);
  WriteByteToSocket(Code);

  // Code:
  // 1 = Command
  // 0 = Data
  // Send Filesize
  case Code of
     0: WriteByteToSocket(Length(Data));
     1: WriteByteToSocket(Length(Data)-1);
  end;

  // Send Data
  for i := 0 to Length(data)-1 do
    WriteByteToSocket(data[i]);

  // If it is not a command, then send CR
  if Code = 0 then
    WriteByteToSocket(13);
end;

procedure TKISSMode.LoadTNCInit;
var FileHandle: TextFile;
    HomeDir, Line: string;
begin
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
   // send needed parameter
   SendStringCommand(0,1,'Y '+IntToStr(FPConfig^.MaxChannels));
   SendStringCommand(0,1,'M USIC');
   // send parameter from init file
   while not EOF(FileHandle) do
   begin
     Readln(FileHandle, Line);
     SendStringCommand(0,1,Line);
     sleep(5);
   end;
 finally
   CloseFile(FileHandle);
 end;
end;

procedure TKISSMode.SetCallsign;
var i: Byte;
begin
 if not Connected then
   Exit;

  for i:=1 to FPConfig^.MaxChannels do
    SendStringCommand(i,1,'I '+FPConfig^.Callsign);

end;

function TKISSMode.ReadByteFromSocket:Byte;
begin
  {$IFDEF UNIX}
  fpRead(FSocket, @Result, 1)
  {$ENDIF}
end;

procedure TKISSMode.WriteByteToSocket(const Data: Byte);
begin
  {$IFDEF UNIX}
  fpWrite(FSocket, @Data, 1)
  {$ENDIF}
end;

end.

