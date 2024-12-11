unit upipes;

{$mode ObjFPC}{$H+}

interface

uses
  {$IFDEF UNIX}
  BaseUnix,
  {$ENDIF}
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  Classes, SysUtils, Dialogs;

procedure CreatePipe(const PipeName: string);
procedure WriteToPipe(const PipeName: string; const Data: string);
procedure ClosePipe(const PipeName: string);
function IsPipe:Boolean;
function IsPipeExisting(const PipeName: string): Boolean;


var
  Pipe: Boolean;
  {$IFDEF MSWINDOWS}
  PipeHandle: THandle;
  {$ENDIF}

implementation

function IsPipe:Boolean;
begin
  Result := Pipe;
end;

procedure CreatePipe(const PipeName: string);
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


procedure WriteToPipe(const PipeName: string; const Data: string);
{$IFDEF UNIX}
var
  Pipe: Integer;
begin
  if Length(Data) > 0 then
  begin
    Pipe := FpOpen(PChar('/tmp/'+PipeName), O_WRONLY or O_NONBLOCK);
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
    PChar('\\.\pipe\' + PipeName),
    GENERIC_WRITE,
    0,
    nil,
    OPEN_EXISTING,
    0,
    0
  );

  if PipeHandle = INVALID_HANDLE_VALUE then
  begin
    ShowMessage('Could not open Pipe to write: ' + PipeName);
    Exit;
  end;

  if not WriteFile(PipeHandle, Data, Length(Data), BytesWritten, nil) then
    ShowMessage('Error during write into pipe');

  CloseHandle(PipeHandle);
end;
{$ENDIF}

procedure ClosePipe(const PipeName: string);
{$IFDEF UNIX}
begin
  if IsPipeExisting(PipeName) then
    if FpUnlink(PChar('/tmp/' + PipeName)) <> 0 then
      ShowMessage('Could not remove Pipe: ' + PipeName);

  Pipe := False;
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
function IsPipeExisting(const PipeName: string): Boolean;
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

