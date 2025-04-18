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
    function ReceiveDataUntilZero:string;
    function ReceiveStringData:string;
    function ReceiveByteData:TBytes;
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
    procedure LoadTNCInit;
    procedure SetCallsign;
    procedure SendStringCommand(const Channel, Code: byte; const Command: String);
    procedure SendByteCommand(const Channel, Code: byte; const Data: TBytes);
    procedure SendFile(const Channel: byte);
    function DecodeLinkStatus(const Text: string):TLinkStatus;
    function DecodeSendLResult(const Text: String):TStringArray;
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
  Connected := False;
end;

destructor THostmode.Destroy;
begin
  Connected := False;
  FSerial.CloseSocket;
  inherited Destroy;
end;

procedure THostmode.Execute;
var
  LastSendTimeG, LastSendTimeL: Cardinal;
begin
  repeat
    if FPConfig^.ComPort <> '' then
    begin
      FSerial.Connect(FPConfig^.ComPort);
      FSerial.Config(FPConfig^.ComSpeed, FPConfig^.ComBits, FPConfig^.ComParity[1], FPConfig^.ComStopBit, False, False);
    end;
    sleep (200);
  until FSerial.InstanceActive;

  // init TNC
  repeat
    FSerial.SendString(#17#24#13);
    FSerial.SendString(#27+'JHOST1'+#13);
    sleep (200);
  until FSerial.RecvByte(100) = 0;

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
    Channel := FSerial.RecvByte(100);
    Code := FSerial.RecvByte(100);

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
    until i = 254;
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
      Result := Result + UTF8Encode(Chr(Data));
    until (i = Len) or (i = 254);
  end;
end;

function THostmode.ReceiveByteData:TBytes;
var i: Byte;
    Len: Integer;
begin
  Result := TBytes.Create;
  SetLength(Result, 0);
  i := 0;
  if FSerial.CanRead(100) then
  begin
    Len := FSerial.RecvByte(100);
    if Len > 0 then
    begin
      SetLength(Result, Len);
      for i := 0 to Len - 1 do
      begin
        Result[i] := FSerial.RecvByte(100);
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

procedure THostmode.LoadTNCInit;
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

