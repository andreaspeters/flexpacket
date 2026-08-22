unit ucommands;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;

type
  TCommandClock = function: QWord;

  TInternalCommandResult = record
    Handled: Boolean;
    Outgoing: String;
    LocalOutput: String;
  end;

  { TInternalCommands }

  TInternalCommands = class
  private
    FClock: TCommandClock;
    FPending: array[0..255] of Boolean;
    FToken: array[0..255] of String;
    FStartTick: array[0..255] of QWord;
    function CurrentTick: QWord;
  public
    constructor Create(AClock: TCommandClock = nil);
    function Execute(const Channel: Byte; const Input: String): TInternalCommandResult;
    function CheckEchoRequest(const Data: String): String;
    function CheckRTT(const Channel: Byte; const Data, RemoteCall,
      LocalCall: String): String;
  end;

implementation

function DefaultClock: QWord;
begin
  Result := GetTickCount64;
end;

constructor TInternalCommands.Create(AClock: TCommandClock);
begin
  inherited Create;
  if Assigned(AClock) then
    FClock := AClock
  else
    FClock := @DefaultClock;
end;

function TInternalCommands.CurrentTick: QWord;
begin
  Result := FClock();
end;

function TInternalCommands.Execute(const Channel: Byte;
  const Input: String): TInternalCommandResult;
var
  CommandText, Token: String;
  StartTick: QWord;
begin
  Result := Default(TInternalCommandResult);
  CommandText := Trim(Input);

  if Copy(CommandText, 1, 2) <> '//' then
    Exit;

  Result.Handled := True;
  CommandText := UpperCase(Trim(Copy(CommandText, 3, MaxInt)));

  if CommandText = 'HELP' then
  begin
    Result.LocalOutput :=
      '//HELP - Show this help' + LineEnding +
      '//RTT - Measure round-trip time to the connected station' + LineEnding +
      '//E //RT $TOKEN - Internal RTT echo request';
    Exit;
  end;

  if CommandText = 'RTT' then
  begin
    StartTick := CurrentTick;
    Token := IntToHex(LongWord(StartTick and QWord($FFFFFFFF)), 8);
    FPending[Channel] := True;
    FToken[Channel] := Token;
    FStartTick[Channel] := StartTick;
    Result.Outgoing := '//e //RT $' + Token;
    Exit;
  end;

  Result.LocalOutput := 'Unknown internal command. Type //HELP for help.';
end;

function TInternalCommands.CheckEchoRequest(const Data: String): String;
const
  Marker = '//E //RT $';
var
  I, MarkerPos: Integer;
  Token, UpperData: String;
begin
  Result := '';
  UpperData := UpperCase(Data);
  MarkerPos := Pos(Marker, UpperData);
  if MarkerPos = 0 then
    Exit;

  Token := Copy(UpperData, MarkerPos + Length(Marker), 8);
  if Length(Token) <> 8 then
    Exit;

  for I := 1 to Length(Token) do
    if not (Token[I] in ['0'..'9', 'A'..'F']) then
      Exit;

  I := MarkerPos + Length(Marker) + 8;
  if (I <= Length(UpperData)) and
    (UpperData[I] in ['0'..'9', 'A'..'F']) then
    Exit;

  Result := '//RT $' + Token;
end;

function TInternalCommands.CheckRTT(const Channel: Byte; const Data,
  RemoteCall, LocalCall: String): String;
var
  ElapsedSeconds: Double;
  FormatSettings: TFormatSettings;
  RemoteName, LocalName, UpperData: String;
begin
  Result := '';
  if not FPending[Channel] then
    Exit;

  UpperData := UpperCase(Data);
  if (Pos('//RT $' + FToken[Channel], UpperData) = 0) and
    (Pos('INVALID COMMAND', UpperData) = 0) then
    Exit;

  ElapsedSeconds := (CurrentTick - FStartTick[Channel]) / 1000.0;
  FPending[Channel] := False;
  FToken[Channel] := '';

  RemoteName := Trim(RemoteCall);
  if RemoteName = '' then
    RemoteName := 'remote station';
  LocalName := Trim(LocalCall);
  if LocalName = '' then
    LocalName := 'local station';

  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := Format('*** RTT = %.2f s between %s and %s',
    [ElapsedSeconds, RemoteName, LocalName], FormatSettings);
end;

end.
