unit upipes;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Dialogs,
  {$IFDEF UNIX}BaseUnix{$ELSE}Windows{$ENDIF};

type
  TReadPipeThread = class(TThread)
  private
  protected
    procedure Execute; override;
  public
    ReadPipeName: String;
    WritePipeName: String;
    ReadPipeData: String;
    Error: Boolean;
    procedure CreatePipe(const PipeName: String);
    procedure WriteToPipe(Data: String);
    procedure ClosePipe(const PipeName: String);
    function IsPipe:Boolean;
    function IsPipeExisting(const PipeName: String): Boolean;
    constructor Create;
  end;

var
  Pipe: Boolean;
  FHPipe: Integer;
  {$IFDEF MSWINDOWS}
  PipeHandle: THandle;
  {$ENDIF}

implementation

{ TReadPipeThread }

constructor TReadPipeThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := True;
  Error := False;
end;

function TReadPipeThread.IsPipe:Boolean;
begin
  Result := Pipe;
end;


procedure TReadPipeThread.CreatePipe(const PipeName: String);
begin
  {$IFDEF UNIX}
  if FpMkFifo(PChar('/tmp/' + PipeName), &0666) <> 0 then
  begin
    ShowMessage('Could not create Pipe: ' + PipeName);
    Exit;
  end;
  {$ENDIF}
  {$IFDEF MSWINDOWS}

  PipeHandle := CreateNamedPipe(
    PChar('\\.\pipe\' + PipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    10,     // Max Instances
    1024,  // Output Buffer Size
    1024,  // Input Buffer Size
    0,     // Timeout
    nil    // Securityattributes
  );
  if PipeHandle = INVALID_HANDLE_VALUE then
  begin
    ShowMessage('Could not create Pipe: ' + IntToStr(GetLastError()));
    Exit;
  end;
  {$ENDIF}
  Pipe := True;
end;


procedure TReadPipeThread.WriteToPipe(Data: String);
{$IFDEF UNIX}
var Pipe: Integer;
begin
  if not IsPipeExisting(WritePipeName) then
    Exit;

  if Length(Data) > 0 then
  begin
    Pipe := FpOpen(PChar('/tmp/'+WritePipeName), O_WRONLY or O_NONBLOCK);
    if Pipe >= 0 then
    begin
      FpWrite(Pipe, PChar(Data)^, Length(Data));
      FpClose(Pipe);
    end
  end;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
var BytesWritten: DWORD;
begin
  BytesWritten := 0;

  PipeHandle := CreateFile(
    PChar('\\.\pipe\' + WritePipeName),
    GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if PipeHandle = INVALID_HANDLE_VALUE then
  begin
    ShowMessage('Could not open Pipe to write: ' + WritePipeName);
    Exit;
  end;

  if not WriteFile(PipeHandle, Data, Length(Data), BytesWritten, nil) then
    ShowMessage('Error during write into pipe');

  CloseHandle(PipeHandle);
end;
{$ENDIF}


procedure TReadPipeThread.Execute;
{$IFDEF UNIX}
var Pipe: Integer;
    Buffer: array[0..255] of Char;
    BytesRead: ssize_t;
    Text : String;
begin
  repeat
    Pipe := FpOpen(PChar('/tmp/' + ReadPipeName), O_RDONLY or O_NONBLOCK);
    if Pipe < 0 then
    begin
      Writeln('Could not open Pipe to read: ', ReadPipeName);
      Exit;
    end;
    repeat
      BytesRead := FpRead(Pipe, Buffer, SizeOf(Buffer));
      if BytesRead > 0 then
      begin
        // Convert the bytes that were actually read into a string.
        SetLength(Text, BytesRead);
        Move(Buffer[0], Text[1], BytesRead);  // faster than Text := Text + Buffer[i];
        ReadPipeData := ReadPipeData + Text ;
      end;
      // Wenn BytesRead = 0 bedeutet, dass der Schreiber die Pipe geschlossen hat.
    until BytesRead = 0;  // EOF erreicht

    FpClose(Pipe);
    // Warten bis ein neuer Schreiber die Pipe erneut öffnet.
    Sleep(100);
  until Terminated;
end;
{$ELSE}
var
  PipeHandle: THandle;
  Buffer: array[0..255] of Char;
  BytesRead: DWORD;
  Text: string;
  i: Integer;

begin
  PipeHandle := CreateNamedPipe(
    PChar('\\.\pipe\' + ReadPipeName),
    PIPE_ACCESS_INBOUND,
    PIPE_TYPE_BYTE or PIPE_WAIT,
    1,
    0,
    0,
    0,
    nil
  );

  if PipeHandle = INVALID_HANDLE_VALUE then
  begin
    {$IFDEF UNIX}
    Writeln('Could not create Pipe to read: ', ReadPipeName);
    {$ENDIF}
    Error := True;
    Exit;
  end;

  try
    if ConnectNamedPipe(PipeHandle, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then
    begin
      while not Terminated do
      begin
        if ReadFile(PipeHandle, Buffer, SizeOf(Buffer) - 1, BytesRead, nil) then
        begin
          if BytesRead > 0 then
          begin
            Text := '';
            for i := 0 to BytesRead - 1 do
            begin
              Text := Text + Buffer[i];
            end;
            ReadPipeData := Text;
          end;
        end
        else
        begin
          // Fehlerbehandlung bei ReadFile
          if GetLastError() = ERROR_BROKEN_PIPE then
          begin
            // Die Pipe wurde geschlossen
            Break;
          end;
        end;
      end;
    end;
  finally
    CloseHandle(PipeHandle);
  end;
end;
{$ENDIF}

procedure TReadPipeThread.ClosePipe(const PipeName: string);
{$IFDEF UNIX}
begin
  if IsPipeExisting(PipeName) then
    if FpUnlink(PChar('/tmp/' + PipeName)) <> 0 then
      ShowMessage('Could not remove Pipe: ' + PipeName);

  Pipe := False;

  // Close ReadPipe FileHandler
  if FHPipe > 0 then
    FpClose(FHPipe);

end;
{$ENDIF}
{$IFDEF MSWINDOWS}
begin
  if not IsPipeExisting(PipeName) then
    Exit;

  // Close Pipe
  if not DisconnectNamedPipe(PipeHandle) then
    Exit;

  // Close Pipe Handling
  CloseHandle(PipeHandle);

  Pipe := False;
end;
{$ENDIF}

{
  IsPipeExisting

  Check if the pipe is already existing
}
{$IFDEF UNIX}
function TReadPipeThread.IsPipeExisting(const PipeName: string): Boolean;
begin
  if FpAccess(PChar('/tmp/' + PipeName), F_OK) = 0 then
    Result := True
  else
    Result := False;
end;
{$ENDIF}
{$IFDEF MSWINDOWS}
function IsPipeExisting(const PipeName: string): Boolean;
begin
  PipeHandle := CreateFile(
    PChar('\\.\pipe\' + PipeName),
    GENERIC_READ or GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if PipeHandle = INVALID_HANDLE_VALUE then
    Result := False
  else
    Result := True;
end;
{$ENDIF}

end.

