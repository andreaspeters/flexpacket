unit ukissmode;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Dialogs, ExtCtrls,
  lazsynaser, Graphics, utypes, RegExpr;

const
  FESC = $DB;
  FEND = $C0;
  TFESC = $DD;
  TFEND = $DC;

type
  { TKISSMode }

  TKISSMode = class(TThread)
  private
    FSerial: TBlockSerial;
    KISSState: string;
    KISSFrame: TBytes;
    FrameCount: Integer;
    FPConfig: PTFPConfig;
    procedure ProcessFrame(Frame: TBytes);
    procedure PrintAX25Header(Frame: TBytes);
  protected
    procedure Execute; override;
  public
    Connected: Boolean;
    constructor Create(Config: PTFPConfig);
    destructor Destroy; override;
  end;

implementation

{ TKISSMode }

constructor TKISSMode.Create(Config: PTFPConfig);
begin
  inherited Create(True);
  FPConfig := Config;
  FSerial := TBlockSerial.Create;
  FreeOnTerminate := True;
  Connected := False;
  KISSState := 'non-escaped';
  SetLength(KISSFrame, 0);
  FrameCount := 0;
end;

destructor TKISSMode.Destroy;
begin
  Connected := False;
  FSerial.CloseSocket;
  inherited Destroy;
end;

procedure TKISSMode.Execute;
var
  InputData: Byte;
begin
  repeat
    FSerial.Connect(FPConfig^.ComPort);
    FSerial.Config(FPConfig^.ComSpeed, FPConfig^.ComBits, FPConfig^.ComParity[1], FPConfig^.ComStopBit, False, False);
    sleep (200);
  until FSerial.InstanceActive;

  // init TNC
  repeat
    FSerial.SendString(#17#24#13);
    FSerial.SendString(#13#27+'@k'+#13); // set kiss mode
    sleep (200);
  until FSerial.RecvByte(100) = 0;

  Connected := True;

  while not Terminated do
  begin
    if FSerial.CanRead(1000) then
    begin
      InputData := FSerial.RecvByte(100);

      try
        if KISSState = 'non-escaped' then
        begin
          if InputData = FESC then
            KISSState := 'escaped'
          else if InputData = FEND then
          begin
            if Length(KISSFrame) > 0 then
            begin
              Inc(FrameCount);
              ProcessFrame(KISSFrame);
              SetLength(KISSFrame, 0);
            end;
          end
          else
          begin
            SetLength(KISSFrame, Length(KISSFrame) + 1);
            KISSFrame[High(KISSFrame)] := InputData;
          end;
        end
        else if KISSState = 'escaped' then
        begin
          if InputData = TFESC then
            InputData := FESC
          else if InputData = TFEND then
            InputData := FEND;

          SetLength(KISSFrame, Length(KISSFrame) + 1);
          KISSFrame[High(KISSFrame)] := InputData;
          KISSState := 'non-escaped';
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
    Sleep(5);
  end;
end;

procedure TKISSMode.ProcessFrame(Frame: TBytes);
var
  TimeStamp: string;
  CRCValue: Integer;
begin
  TimeStamp := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  WriteLn('-- ', TimeStamp, ' Frame #', FrameCount);

  // TODO
  CRCValue := $FFFF;
  WriteLn('CRC: ', IntToHex(CRCValue, 4), ' Byte Count: ', Length(Frame));

  PrintAX25Header(Frame);
end;

procedure TKISSMode.PrintAX25Header(Frame: TBytes);
var
  i: Integer;
  Addr: string;
begin
  Addr := '';
  Write('AX.25 Header: ');
  for i := 0 to Min(14, High(Frame)) do
  begin
    Addr := Addr + Chr((Frame[i] shr 1) and $7F);
  end;
  WriteLn(Trim(Addr));
end;

end.

