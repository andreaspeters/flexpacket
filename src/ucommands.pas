unit ucommands;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, RegExpr, utypes;

type
  TCommandClock = function: QWord;

  TInternalCommandResult = record
    Handled: Boolean;
    Outgoing: String;
    LocalOutput: String;
    MessageText: String;
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
    procedure HandleRemoteCommand(channel: byte; Data: AnsiString);
    function Execute(const Channel: Byte; const Input: String): TInternalCommandResult;
    function CheckRTT(const Channel: Byte; const Data: String): String;
    function ExpandRemoteSignature(const Channel: Byte): String;
  end;


implementation

uses UMain;

function DefaultClock: QWord;
begin
  Result := GetTickCount64;
end;

function TInternalCommands.ExpandRemoteSignature(const Channel: Byte): String;
begin
  Result := FPConfig.RemoteSignature;
  Result := StringReplace(Result, '<FP_VERSION>', FLEXPACKET_VERSION, [rfReplaceAll]);
  Result := StringReplace(Result, '<FP_CHANNEL>', IntToStr(Channel), [rfReplaceAll]);
  Result := StringReplace(Result, '<FP_MYCALLSIGN>', FPConfig.Callsign, [rfReplaceAll]);
  Result := StringReplace(Result, '<FP_DATE>', FormatDateTime('yyyy-mm-dd', Now), [rfReplaceAll]);
  Result := StringReplace(Result, '<FP_TIME>', FormatDateTime('hh:nn:ss', Now), [rfReplaceAll]);
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

procedure TInternalCommands.HandleRemoteCommand(channel: byte; Data: AnsiString);
var  CommandResult: TInternalCommandResult;
begin
  if Copy(Data, 1, 2) = '//' then
  begin
    CommandResult := Execute(channel, Data);
    if CommandResult.Handled then
    begin
      if CommandResult.LocalOutput <> '' then
        FMain.AddTextToMemo(channel, #27'[33m' + CommandResult.LocalOutput + #13#10#27'[0m');
      if CommandResult.Outgoing <> '' then
      begin
        FMain.AddTextToMemo(channel, #27'[32m' + CommandResult.Outgoing + #13#10#27'[0m');
        FMain.SendTransportString(channel, 0, CommandResult.Outgoing);
      end;
    end;
  end;
end;

function TInternalCommands.Execute(const Channel: Byte;
  const Input: String): TInternalCommandResult;
var
  CommandText, CommandName, Token: String;
  EchoRegex: TRegExpr;
  StartTick: QWord;
begin
  Result := Default(TInternalCommandResult);
  CommandText := Input;

  if Copy(CommandText, 1, 2) <> '//' then
    Exit;

  Result.Handled := True;
  CommandText := Trim(Copy(CommandText, 3, MaxInt));
  CommandName := UpperCase(CommandText);

  if CommandName = 'HELP' then
  begin
    Result.LocalOutput :=
      '//HELP - Show this help' + LineEnding +
      '//MESSAGE <text> - Leave a message for the station operator' + LineEnding +
      '//RTT - Measure round-trip time to the connected station' + LineEnding +
      '//E //RT $TOKEN or //E RTT TOKEN - Internal RTT echo request';
    Exit;
  end;

  // Some stations use the shorter RTT echo request syntax.  Keep the
  // response identical to the established //E //RT $TOKEN protocol.
  EchoRegex := TRegExpr.Create;
  try
    EchoRegex.Expression := 'E\s+//RT(?:T)\s+\$(.*)';
    EchoRegex.ModifierI := false;
    if EchoRegex.Exec(CommandText) then
    begin
      Result.Outgoing := '//RT $' + UpperCase(EchoRegex.Match[1]);
      Exit;
    end;
  finally
    EchoRegex.Free;
  end;

  if (Length(CommandText) >= Length('MESSAGE')) and
    (UpperCase(Copy(CommandText, 1, Length('MESSAGE'))) = 'MESSAGE') and
    ((Length(CommandText) = Length('MESSAGE')) or
     (CommandText[Length('MESSAGE') + 1] = ' ')) then
  begin
    Delete(CommandText, 1, Length('MESSAGE'));
    CommandText := Trim(CommandText);
    if CommandText = '' then
    begin
      Result.LocalOutput := 'Usage: //MESSAGE <text>';
      Exit;
    end;
    Result.MessageText := CommandText;
    Result.LocalOutput := 'Message stored.';
    Exit;
  end;

  if CommandName = 'RTT' then
  begin
    StartTick := CurrentTick;
    Token := IntToHex(LongWord(StartTick and QWord($FFFFFFFF)), 8);
    FPending[Channel] := True;
    FToken[Channel] := Token;
    FStartTick[Channel] := StartTick;
    Result.Outgoing := '//e //RT $' + Token;
    Exit;
  end;

  EchoRegex := TRegExpr.Create;
  try
    EchoRegex.Expression := 'RT\s+\$(.*)';
    EchoRegex.ModifierI := false;
    if EchoRegex.Exec(CommandText) then
    begin
      Result.Outgoing := CheckRTT(Channel, Input);
      Exit;
    end;
  finally
    EchoRegex.Free;
  end;

  Result.LocalOutput := 'Unknown internal command. Type //HELP for help.';
end;

function TInternalCommands.CheckRTT(const Channel: Byte; const Data: String): String;
var
  ElapsedSeconds: Double;
  FormatSettings: TFormatSettings;
  UpperData: String;
begin
  Result := '';
  if not FPending[Channel] then
    Exit;

  UpperData := UpperCase(Data);

  ElapsedSeconds := (CurrentTick - FStartTick[Channel]) / 1000.0;
  FPending[Channel] := False;
  FToken[Channel] := '';

  FormatSettings := DefaultFormatSettings;
  FormatSettings.DecimalSeparator := '.';
  Result := Format('*** RTT = %.2f s', [ElapsedSeconds], FormatSettings);
end;

end.
