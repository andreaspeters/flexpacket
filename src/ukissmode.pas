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

  TChannelString = array[0..10] of string;
  TChannelStatus = array[0..10] of TStatusLine;
  TChannelCallsign = array[0..10] of String;

  TAX25Frame = record
    FromCall: String;
    ToCall: String;
    ViaCall: String;
    FrameType: Byte;                   // Frame-Typ: Information, Supervisory, Unnumbered
    NS: Byte;                          // N(S) (only for I Frames)
    NR: Byte;                          // N(R) (Only fpr I- and S-Frames)
    PFBit: Byte;                       // Poll/Final-Bit (I-, S- and U-Frames)
    InformationType: String;
    SupervisoryType: String; // Type of Supervisory Frames (RR, RNR, REJ, SREJ)
    UnnumberedType: String;   // Type of Unnumbered Frames (SABM, UA, UI, ...)
    PID: Byte;                         // PID (only for UI-Frames with Layer-3)
    Payload: string;                   // Payload
    Control: String;
  end;

  TKISSMode = class(TThread)
  private
    FSerial: TBlockSerial;
    KISSState: string;
    KISSFrame: TBytes;
    FrameCount: Integer;
    FPConfig: PTFPConfig;
    function GetFrameData(const Frame: TAX25Frame):String;
    function ReceiveData(const Frame: TBytes):TAX25Frame;
  protected
    procedure Execute; override;
  public
    ChannelStatus: TChannelStatus;
    ChannelBuffer: TChannelString;
    Connected: Boolean;
    constructor Create(Config: PTFPConfig);
    destructor Destroy; override;
  end;

var
  ChannelDestCallsign: TChannelCallsign;

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
  Frame: TAX25Frame;
  Text: String;
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
              Frame := ReceiveData(KISSFrame);
              Text := GetFrameData(Frame);
              // Monitor Data
              if (Frame.FrameType = 1) or (Frame.FrameType = 3) then
                ChannelBuffer[0] := ChannelBuffer[0] + Text + #13 + UTF8Decode(Frame.Payload) + #13;

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

function TKISSMode.GetFrameData(const Frame: TAX25Frame):String;
begin
  writeln('---------------------------------------');
  // Addressing Information
  Result := ' fm ' + Frame.ToCall;
  Result := Result + ' to ' + Frame.FromCall + ' ';
  if Length(Frame.ViaCall) > 0 then
    Result := Result + 'via ' + Frame.ViaCall + ' ';

  // Control Field
  Result := Result + 'ctl ';

  // Frame Type
  case Frame.FrameType of
    0: Result := Result + Frame.InformationType;
    1: Result := Result + Frame.SupervisoryType;
    3: Result := Result + Frame.UnnumberedType;
  end;

  // Sequence numbers and PF Bit
  writeln('N(S): ', Frame.NS, ', N(R): ', Frame.NR, ', P/F: ', Frame.PFBit);


  // PID Information
  Result := Result + ' pid ';

  case Frame.PID of
    $01: Result := Result + '01'; // ISO 8208
    $06: Result := Result + '06'; // Compressed TCP/IP
    $07: Result := Result + '07'; // Uncompressed TCP/IP
    $08: Result := Result + '08'; // Segmentation Fragment
    $C3: Result := Result + 'C3'; // TEXNET
    $C4: Result := Result + 'C4'; // Link Quality Protocol
    $CA: Result := Result + 'CA'; // Appletalk
    $CC: Result := Result + 'CC'; // ARPA Internet Protocol
    $CD: Result := Result + 'CD'; // ARPA Address Resolution
    $CE: Result := Result + 'CE'; // FlexNET
    $CF: Result := Result + 'CF'; // TheNET (NET/ROM)
    $F0: Result := Result + 'F0'; // No Layer 3
    $FF: Result := Result + 'Escape';
  end;

  writeln();
end;

function TKISSMode.ReceiveData(const Frame: TBytes):TAX25Frame;
var
  Count, Index, SubfieldCharacterIndex, SubfieldIndex: Integer;
  AddressExtensionBit, Data, FrameType: Byte;
begin
  Count := Length(Frame);
  Index := 0;
  FrameType := 0;
  Result := Default(TAX25Frame);

  if Count > 15 then
  begin
    AddressExtensionBit := 0;
    Index := 1;
    SubfieldCharacterIndex := 0;
    SubfieldIndex := 0;

    // Address
    while AddressExtensionBit = 0 do
    begin
      Data := Frame[Index];
      if (Data and $01) = 1 then
        AddressExtensionBit := 1;

      Data := Data shr 1;
      Inc(SubfieldCharacterIndex);

      // To, From, Via
      case SubfieldIndex of
        0:
          begin
            // From Callsign without SSID
            if SubfieldCharacterIndex < 7 then
              Result.FromCall := Result.FromCall + Chr(Data)
            else if SubfieldCharacterIndex = 7 then
            begin
              // From Callsign with SSID
              Result.FromCall := Result.FromCall + '-' + IntToStr(Data and $0F);
              if (Data and $80) <> 0 then
                Result.FromCall := Result.FromCall + '*'; // H-Bit prüfen
              Result.FromCall := StringReplace(Result.FromCall, ' ', '', [rfReplaceAll]);
              SubfieldCharacterIndex := 0;
              Inc(SubfieldIndex);
            end;
          end;
        1:
          begin
            // To Callsign without SSID
            if SubfieldCharacterIndex < 7 then
              Result.ToCall := Result.ToCall + Chr(Data)
            else if SubfieldCharacterIndex = 7 then
            begin
              // To Callsign with SSID
              Result.ToCall := Result.ToCall + '-' + IntToStr(Data and $0F);
              if (Data and $80) <> 0 then
                Result.ToCall := Result.ToCall + '*';
              Result.ToCall := StringReplace(Result.ToCall, ' ', '', [rfReplaceAll]);
              SubfieldCharacterIndex := 0;
              Inc(SubfieldIndex);
            end;
          end;
      else
        begin
          // Via Callsign without SSID
          if SubfieldCharacterIndex < 7 then
            Result.ViaCall := Result.ViaCall + Chr(Data)
          else if SubfieldCharacterIndex = 7 then
          begin
            // Via Callsign with SSID
            Result.ViaCall := Result.ViaCall + '-' + IntToStr(Data and $0F);
            if (Data and $80) <> 0 then
              Result.ViaCall := Result.ViaCall + '*';
            Result.ViaCall := StringReplace(Result.ViaCall, ' ', '', [rfReplaceAll]);
            SubfieldCharacterIndex := 0;
            Inc(SubfieldIndex);
          end;
        end;
      end;

      Inc(Index);
      if Index > Count then
        AddressExtensionBit := 1;
    end;

    // Control
    Data := Frame[Index];
    Result.Control := HexStr(Data, 2);

    // Frame-Typ (Information 0, Supervisory 1 , Unnumbered 3)
    FrameType := Data and $03;
    Result.FrameType := FrameType;

    case FrameType of
      0: // Information Frame
      begin
        Result.NS := (Data shr 1) and $07;    // Bits 1-3: N(S)
        Result.NR := (Data shr 5) and $07;    // Bits 5-7: N(R)
        Result.PFBit := (Data shr 4) and $01; // Bit 4: P/F-Bit
      end;

      1: // Supervisory Frame
      begin
        Result.NR := (Data shr 5) and $07;
        Result.PFBit := (Data shr 4) and $01;

        case (Data and $0C) shr 2 of
          0: Result.SupervisoryType := 'RR';   // Receive Ready
          1: Result.SupervisoryType := 'RNR';  // Receive Not Ready
          2: Result.SupervisoryType := 'REJ';  // Reject
          3: Result.SupervisoryType := 'SREJ'; // Selective Reject
        end;
      end;

      3: // Unnumbered Frame
      begin
        Result.PFBit := (Data shr 4) and $01;

        case Data and $EF of
          $6F: Result.UnnumberedType := 'SABME';
          $2F: Result.UnnumberedType := 'SABM';
          $43: Result.UnnumberedType := 'DISC';
          $0F: Result.UnnumberedType := 'DM';
          $63: Result.UnnumberedType := 'UA';
          $87: Result.UnnumberedType := 'FRMR';
          $03: Result.UnnumberedType := 'UI';
          $AF: Result.UnnumberedType := 'XID';
          $E3: Result.UnnumberedType := 'TEST';
        end;
      end;
    end;


    // PID (Only in I Frames)
    if (FrameType = 0) or (Data = $03) then
    begin
      Inc(Index);
      Data := Frame[Index];
      Result.PID := Data;
    end;

    // Payload
    while Index < Count do
    begin
      Result.Payload := Result.Payload + Chr(Frame[Index]);
      Inc(Index);
    end;

    Inc(Index);
  end;
end;


end.

