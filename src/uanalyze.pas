unit uanalyze;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, Types;

type
  TAnalyzerPacket = record
    Stamp: TDateTime;
    Direction: string;
    Size: Integer;
    Status: string;
  end;

  { TFAnalyze }
  TFAnalyze = class(TForm)
    pnlHeader: TPanel;
    lblOnline: TLabel;
    pnlRx: TPanel;
    pnlTx: TPanel;
    pnlRetries: TPanel;
    pnlLink: TPanel;
    lblRxCaption: TLabel;
    lblRxValue: TLabel;
    lblTxCaption: TLabel;
    lblTxValue: TLabel;
    lblRetriesCaption: TLabel;
    lblRetriesValue: TLabel;

    lblLinkCaption: TLabel;
    lblLinkValue: TLabel;
    pnlTraffic: TPanel;
    lblTrafficTitle: TLabel;
    pbTraffic: TPaintBox;
    pnlUtilization: TPanel;
    lblUtilTitle: TLabel;
    lblUtilValue: TLabel;
    lblFramesRate: TLabel;
    lblTxResult: TLabel;
    pbUtilization: TPaintBox;
    pnlLive: TPanel;
    lblLiveTitle: TLabel;
    sgPackets: TStringGrid;
    tmRefresh: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure pbTrafficPaint(Sender: TObject);
    procedure pbUtilizationPaint(Sender: TObject);
    procedure tmRefreshTimer(Sender: TObject);
  private
    FPackets: array[0..31] of TAnalyzerPacket;
    FPacketCount: Integer;
    FLastTick: QWord;
    FRxFrames, FTxFrames: Int64;
    FRetransmits, FTxSuccess, FTxFailed: Int64;
    FPendingTx: Int64;
    FRxPerMinute, FTxPerMinute: Integer;
    FLastRetries, FLastUnacked: Integer;
    FLastLinkState: string;
    FHistoryTick: QWord;
    FHistoryRx, FHistoryTx: array[0..59] of Integer;
    procedure AddPacket(const Direction: string; const DataSize: Integer;
      const Status: string);
    procedure RefreshView;
    procedure UpdateRates;
    procedure AdvanceHistory;
    procedure DrawBar(ACanvas: TCanvas; const R: TRect; Percent: Integer;
      AColor: TColor);
  public
    procedure RecordRx(const Channel: Byte; const Data: AnsiString);
    procedure RecordTx(const Channel: Byte; const DataSize: Integer);
    procedure RecordMonitor(const Data, OwnCallsign: AnsiString);
    procedure RecordStatus(const Channel: Byte; const RetryCount,
      Unacked: Integer; const LinkState: string);
  end;

var
  FAnalyze: TFAnalyze;

procedure AnalyzerRecordRx(const Channel: Byte; const Data: AnsiString);
procedure AnalyzerRecordTx(const Channel: Byte; const DataSize: Integer);
procedure AnalyzerRecordMonitor(const Data, OwnCallsign: AnsiString);
procedure AnalyzerRecordStatus(const Channel: Byte; const RetryCount,
  Unacked: Integer; const LinkState: string);

implementation

{$R *.lfm}

procedure TFAnalyze.FormCreate(Sender: TObject);
begin
  FPacketCount := 0;
  FLastTick := GetTickCount64;
  FHistoryTick := FLastTick;
  sgPackets.RowCount := 1;
  sgPackets.Cells[0, 0] := 'TIME';
  sgPackets.Cells[1, 0] := 'DIR';
  sgPackets.Cells[2, 0] := 'SIZE';
  sgPackets.Cells[3, 0] := 'STATUS';
  RefreshView;
end;

procedure TFAnalyze.AddPacket(const Direction: string; const DataSize: Integer;
  const Status: string);
var
  I: Integer;
begin
  for I := High(FPackets) downto 1 do
    FPackets[I] := FPackets[I - 1];
  FPackets[0].Stamp := Now;
  FPackets[0].Direction := Direction;
  FPackets[0].Size := DataSize;
  FPackets[0].Status := Status;
  if FPacketCount < Length(FPackets) then
    Inc(FPacketCount);
end;

procedure TFAnalyze.UpdateRates;
var
  Elapsed: QWord;
begin
  Elapsed := GetTickCount64 - FLastTick;
  if Elapsed < 1000 then
    Exit;
  FRxPerMinute := Round(FRxFrames * 60000 / Elapsed);
  FTxPerMinute := Round(FTxFrames * 60000 / Elapsed);

end;

procedure TFAnalyze.AdvanceHistory;
var
  I, Steps: Integer;
  NowTick: QWord;
begin
  NowTick := GetTickCount64;
  Steps := (NowTick - FHistoryTick) div 1000;
  if Steps <= 0 then
    Exit;
  if Steps > 60 then
    Steps := 60;
  for I := 1 to Steps do
  begin
    Move(FHistoryRx[1], FHistoryRx[0], 59 * SizeOf(Integer));
    Move(FHistoryTx[1], FHistoryTx[0], 59 * SizeOf(Integer));
    FHistoryRx[59] := 0;
    FHistoryTx[59] := 0;
  end;
  FHistoryTick := NowTick;
end;

procedure TFAnalyze.RefreshView;
var
  I: Integer;
begin
  UpdateRates;
  lblRxValue.Caption := IntToStr(FRxFrames);
  lblTxValue.Caption := IntToStr(FTxFrames);
  lblRetriesValue.Caption := IntToStr(FRetransmits);

  if (FTxSuccess + FTxFailed) > 0 then
    lblLinkValue.Caption := FormatFloat('0.0 %', FTxSuccess * 100 /
      (FTxSuccess + FTxFailed))
  else
    lblLinkValue.Caption := '--';
  lblUtilValue.Caption := '--';
  lblFramesRate.Caption := Format('RX %d/min  TX %d/min',
    [FRxPerMinute, FTxPerMinute]);

  lblTxResult.Caption := Format('OK %d  /  FAIL %d', [FTxSuccess, FTxFailed]);

  sgPackets.RowCount := FPacketCount + 1;
  for I := 0 to FPacketCount - 1 do
  begin
    sgPackets.Cells[0, I + 1] := FormatDateTime('hh:nn:ss', FPackets[I].Stamp);
    sgPackets.Cells[1, I + 1] := FPackets[I].Direction;
    if FPackets[I].Size > 0 then
      sgPackets.Cells[2, I + 1] := IntToStr(FPackets[I].Size)
    else
      sgPackets.Cells[2, I + 1] := '--';
    sgPackets.Cells[3, I + 1] := FPackets[I].Status;
  end;
  pbTraffic.Invalidate;
  pbUtilization.Invalidate;
end;

procedure TFAnalyze.RecordRx(const Channel: Byte; const Data: AnsiString);
begin
  if (Channel = 0) or (Length(Data) = 0) then
    Exit;
  AdvanceHistory;
  Inc(FHistoryRx[59]);
  Inc(FRxFrames);

  AddPacket('RX', Length(Data), 'RECEIVED');
  RefreshView;
end;

procedure TFAnalyze.RecordTx(const Channel: Byte; const DataSize: Integer);
begin
  if (DataSize <= 0) then
    Exit;
  AdvanceHistory;
  Inc(FHistoryTx[59]);
  Inc(FTxFrames);

  Inc(FPendingTx);
  AddPacket('TX', DataSize, 'SENT');
  RefreshView;
end;

procedure TFAnalyze.RecordMonitor(const Data, OwnCallsign: AnsiString);
var
  Lines, Parts: TStringList;
  I, J: Integer;
  Source, Direction: string;
begin
  if Length(Data) = 0 then
    Exit;
  Lines := TStringList.Create;
  Parts := TStringList.Create;
  try
    Lines.Text := StringReplace(Data, #13, '', [rfReplaceAll]);
    for I := 0 to Lines.Count - 1 do
    begin
      Parts.Clear;
      Parts.Delimiter := ' ';
      Parts.StrictDelimiter := False;
      Parts.DelimitedText := Trim(Lines[I]);
      Source := '';
      for J := 0 to Parts.Count - 2 do
        if SameText(Parts[J], 'fm') then
        begin
          Source := Parts[J + 1];
          Break;
        end;
      if Source = '' then
        Continue;
      if SameText(Source, Trim(OwnCallsign)) then
        Direction := 'TX'
      else
        Direction := 'RX';
      AdvanceHistory;
      if Direction = 'TX' then
      begin
        Inc(FHistoryTx[59]);
        Inc(FTxFrames);
      end
      else
      begin
        Inc(FHistoryRx[59]);
        Inc(FRxFrames);
      end;
      AddPacket(Direction, 0, 'MONITOR');
    end;
  finally
    Parts.Free;
    Lines.Free;
  end;
  RefreshView;
end;

procedure TFAnalyze.RecordStatus(const Channel: Byte; const RetryCount,
  Unacked: Integer; const LinkState: string);
var
  Retries: Integer;
begin
  Retries := RetryCount;
  if (FPendingTx > 0) and (Retries > FLastRetries) then
    Inc(FRetransmits, Retries - FLastRetries);
  FLastRetries := Retries;

  if (FPendingTx > 0) and (FLastUnacked > 0) and (Unacked = 0) then
  begin
    Inc(FTxSuccess);
    Dec(FPendingTx);
  end;
  FLastUnacked := Unacked;

  if (UpperCase(Trim(LinkState)) = 'LINK FAILURE') or
     (UpperCase(Trim(LinkState)) = 'DISCONNECTED') then
  begin
    if (FPendingTx > 0) and
       (FLastLinkState <> UpperCase(Trim(LinkState))) then
    begin
      Inc(FTxFailed);
      FPendingTx := 0;
      AddPacket('TX', 0, UpperCase(Trim(LinkState)));
    end;
  end;
  FLastLinkState := UpperCase(Trim(LinkState));
  RefreshView;
end;

procedure TFAnalyze.tmRefreshTimer(Sender: TObject);
begin
  AdvanceHistory;
  RefreshView;
end;

procedure TFAnalyze.DrawBar(ACanvas: TCanvas; const R: TRect; Percent: Integer;
  AColor: TColor);
var
  FillRect: TRect;
begin
  ACanvas.Brush.Color := RGBToColor(43, 52, 63);
  ACanvas.FillRect(R);
  FillRect := R;
  FillRect.Right := FillRect.Left + ((R.Right - R.Left) * Percent) div 100;
  ACanvas.Brush.Color := AColor;
  ACanvas.FillRect(FillRect);
end;

procedure TFAnalyze.pbTrafficPaint(Sender: TObject);
var
  C: TCanvas;
  I, X, MaxValue, PlotHeight, Baseline, RxY, TxY: Integer;
  PrevRxY, PrevTxY, PrevX: Integer;
begin
  C := pbTraffic.Canvas;
  C.Brush.Color := clDefault;
  C.FillRect(pbTraffic.ClientRect);
  C.Pen.Color := clDefault;
  Baseline := pbTraffic.Height div 2;
  C.Line(12, Baseline, pbTraffic.Width - 12, Baseline);
  for I := 1 to 2 do
  begin
    C.Line(12, Baseline - (Baseline * I) div 3,
      pbTraffic.Width - 12, Baseline - (Baseline * I) div 3);
    C.Line(12, Baseline + (Baseline * I) div 3,
      pbTraffic.Width - 12, Baseline + (Baseline * I) div 3);
  end;

  MaxValue := 0;
  for I := 0 to High(FHistoryRx) do
  begin
    if FHistoryRx[I] > MaxValue then
      MaxValue := FHistoryRx[I];
    if FHistoryTx[I] > MaxValue then
      MaxValue := FHistoryTx[I];
  end;
  if MaxValue = 0 then
  begin
    C.Font.Color := clDefault;
    C.TextOut(28, pbTraffic.Height div 2 - 8,
      'Wait for data ...');
    Exit;
  end;

  PlotHeight := (pbTraffic.Height div 2) - 18;
  PrevX := -1;
  PrevRxY := Baseline;
  PrevTxY := Baseline;
  for I := 0 to 59 do
  begin
    X := 18 + (I * (pbTraffic.Width - 36)) div 59;
    RxY := Baseline - (FHistoryRx[I] * PlotHeight) div MaxValue;
    TxY := Baseline + (FHistoryTx[I] * PlotHeight) div MaxValue;
    C.Pen.Color := clGreen;
    C.Pen.Width := 2;
    if PrevX >= 0 then
      C.Line(PrevX, PrevRxY, X, RxY);
    C.Pen.Color := clRed;
    if PrevX >= 0 then
      C.Line(PrevX, PrevTxY, X, TxY);
    C.Brush.Color := clDefault;
    C.Ellipse(X - 2, RxY - 2, X + 3, RxY + 3);
    C.Brush.Color := clDefault;
    C.Ellipse(X - 2, TxY - 2, X + 3, TxY + 3);
    PrevX := X;
    PrevRxY := RxY;
    PrevTxY := TxY;
  end;
  C.Pen.Width := 1;
  C.Font.Color := clDefault;
  C.TextOut(14, 8, 'RX');
  C.TextOut(14, Baseline + 20, 'TX');
  C.TextOut(14, pbTraffic.Height - 16, '-60 s');
  C.TextOut(pbTraffic.Width - 32, pbTraffic.Height - 16, 'now');
end;

procedure TFAnalyze.pbUtilizationPaint(Sender: TObject);
var
  Percent: Integer;
begin
  Percent := 0;
  if (FRxFrames + FTxFrames) > 0 then
    Percent := 50;
  DrawBar(pbUtilization.Canvas, Rect(8, 8, pbUtilization.Width - 8,
    pbUtilization.Height - 8), Percent, RGBToColor(45, 190, 151));
end;

procedure AnalyzerRecordRx(const Channel: Byte; const Data: AnsiString);
begin
  if Assigned(FAnalyze) then
    FAnalyze.RecordRx(Channel, Data);
end;

procedure AnalyzerRecordTx(const Channel: Byte; const DataSize: Integer);
begin
  if Assigned(FAnalyze) then
    FAnalyze.RecordTx(Channel, DataSize);
end;

procedure AnalyzerRecordMonitor(const Data, OwnCallsign: AnsiString);
begin
  if Assigned(FAnalyze) then
    FAnalyze.RecordMonitor(Data, OwnCallsign);
end;

procedure AnalyzerRecordStatus(const Channel: Byte; const RetryCount,
  Unacked: Integer; const LinkState: string);
begin
  if Assigned(FAnalyze) then
    FAnalyze.RecordStatus(Channel, RetryCount, Unacked, LinkState);
end;

end.
