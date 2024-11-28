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

var
  Pipe: Boolean;

implementation

function IsPipe:Boolean;
begin
  Result := Pipe;
end;

procedure CreatePipe(const PipeName: string);
{$IFDEF UNIX}
begin
  if FpMkFifo(PChar('/tmp/' + PipeName), &0666) <> 0 then
    ShowMessage('Could not create Pipe: ' + PipeName);

  Pipe := True;
end;
{$ELSE}
var
  PipeHandle: THandle;
begin
  PipeHandle := CreateNamedPipe(
    PChar('\\.\pipe\' + PipeName),
    PIPE_ACCESS_DUPLEX,
    PIPE_TYPE_MESSAGE or PIPE_READMODE_MESSAGE or PIPE_WAIT,
    1,     // Max Instances
    1024,  // Output Buffer Size
    1024,  // Input Buffer Size
    0,     // Timeout
    nil    // Securityattributes
  );
  if PipeHandle = INVALID_HANDLE_VALUE then
    ShowMessage('Could not create Pipe: ' + PipeName);

  Pipe := True;
end;
{$ENDIF}

procedure WriteToPipe(const PipeName: string; const Data: string);
{$IFDEF UNIX}
var
  Pipe: Integer;
begin
  Pipe := FpOpen(PChar('/tmp/'+PipeName), O_WRONLY);
  if Pipe >= 0 then
  begin
    FpWrite(Pipe, PChar(Data)^, Length(Data));
    FpClose(Pipe);
  end
  else
    ShowMessage('Could not open Pipe to write: ' + PipeName);
end;
{$ELSE}
var
  PipeHandle: THandle;
  BytesWritten: DWORD;
begin
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

  if not WriteFile(PipeHandle, PChar(Data)^, Length(Data), BytesWritten, nil) then
    ShowMessage('Error during write into pipe');

  CloseHandle(PipeHandle);
end;
{$ENDIF}

procedure ClosePipe(const PipeName: string);
{$IFDEF UNIX}
begin
  if FpAccess(PChar(PipeName), F_OK) = 0 then
    if FpUnlink(PChar('/tmp/' + PipeName)) <> 0 then
      ShowMessage('Could not remove Pipe: ' + PipeName);

  Pipe := False;
end;
{$ELSE}
var
  PipeHandle: THandle;
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
  begin
    ShowMessage('Could not access Pipe: ' + PipeName);
    Exit;
  end;

  // Close Pipe
  if not DisconnectNamedPipe(PipeHandle) then
    ShowMessage('Could not remove Pipe: ' + PipeName);

  // Close Pipe Handling
  CloseHandle(PipeHandle);

  Pipe := False;
end;
{$ENDIF}

end.

