unit ufileupload;

{$mode ObjFPC}{$H+}
{$UNITPATH fileprotocols}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ButtonPanel, RegExpr, uresize, ExtCtrls, utypes, FileUtil, uautobin,
  uyapp, uyappc, ufileprotocol;

type

  { TFFileUpload }

  TFFileUpload = class(TForm)
    BPDefaultButtons: TButtonPanel;
    cbTransfereProtocoll: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    STFilename: TStaticText;
    STFileSize: TStaticText;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FOnUpload: TNotifyEvent;
    procedure GetGoSeven(const Data: AnsiString; const Channel: Byte);
    function GetDateTime(const FileName: string): TDateTime;

    function WriteDataToFile(const FileName: string; const Data: TBytes):Integer;
    function WriteDataToFile(const FileName: string; const Data: AnsiString; const Channel:Integer):Integer;
    function FileEnd(const ChannelBuffer: AnsiString): Boolean;
  public
    AutoBin: String;
    Buffer: TBytes;
    FileName: String;
    TransferProtocol: TFileProtocol;
    procedure FileDownload(const ChannelBuffer: TBytes; const Channel: Byte);
    procedure FileDownload(const ChannelBuffer: AnsiString; const Channel: Byte);
    function FileProtocolDownload(const ChannelBuffer: TBytes;
      const Channel: Byte): Boolean;
    procedure SetConfig(Config: PTFPConfig);
    function IsAutoBin(const Head:string):TStrings;
    function Parse7PlusHeader(const Download: TDownload): TDownload;
    function Default:TDownload;
    function LineContainsKeyword(const Line: String): Integer;
    property OnUpload: TNotifyEvent read FOnUpload write FOnUpload;
  end;

var
  FPConfig: PTFPConfig;
  FFileUpload: TFFileUpload;
  OldWidth, OldHeight: Integer;

implementation

uses
  umain;

{$R *.lfm}

{ TFFileUpload }

function TFFileUpload.Parse7PlusHeader(const Download: TDownload): TDownload;
var Header: String;
    posBase: Integer;
begin
  // Suche die Position von "go_7+." im Header-String
  posBase := Pos('go_7+.', Download.Header);
  Result := Download;
  Header := Download.Header;

  {
    Basierend auf dem Beispiel:
    "go_7+. 009 of 009 3YDX_1.JPG   0028024 0540 036 (7PLUS v2.1)"

    Felder relativ zum gefundenen Start (posBase):
      - posBase .. posBase+5:  "go_7+." (Kennung)
      - posBase+6:             Leerzeichen
      - posBase+7,  3 Zeichen: Part-Nummer ("009")
      - posBase+10, 4 Zeichen: fester String " of "
      - posBase+14, 3 Zeichen: Gesamtanzahl ("009")
      - posBase+17:            Leerzeichen
      - posBase+18, 10 Zeichen: Dateiname ("3YDX_1.JPG")
      - posBase+28, 3 Zeichen: Füllung (Leerzeichen)
      - posBase+31, 7 Zeichen: Dateigröße ("0028024")
      - posBase+38:            Leerzeichen
      - posBase+39, 4 Zeichen: Blockgröße ("0540")
      - posBase+43:            Leerzeichen
      - posBase+44, 3 Zeichen: CRC ("036")
      - posBase+47:            Leerzeichen
      - posBase+48 bis Ende:   Versionsangabe ("(7PLUS v2.1)")
  }

  Result.PartNumber := StrToInt(Copy(Header, posBase + 7, 3));
  Result.TotalParts := StrToInt(Copy(Header, posBase + 14, 3));
  Result.FileSize   := StrToInt(Copy(Header, posBase + 31, 7));
  Result.BlockSize  := StrToInt(Copy(Header, posBase + 39, 4));
  Result.FileCRC    := StrToInt(Copy(Header, posBase + 44, 3));
end;

procedure TFFileUpload.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;

{
  FileDownload

  For TBytes Data

  If user accept filedownload, these procedure will call WriteDataToFile
  and check if the written data equal to the predicted file size.
}
procedure TFFileUpload.FileDownload(const ChannelBuffer: TBytes; const Channel: Byte);
var FName: String;
    Written, Remaining: Integer;
    Content, VerifyData: TBytes;
    VerifyStream: TFileStream;
    CalculatedCRC: Integer;
begin
  if Length(ChannelBuffer) > 0 then
  begin
    Content := ChannelBuffer;
    VerifyStream := TFileStream.Create(
      FPConfig^.Download[Channel].TempFileName, fmOpenRead or fmShareDenyWrite);
    try
      Remaining := FPConfig^.Download[Channel].FileSize - VerifyStream.Size;
    finally
      VerifyStream.Free;
    end;
    if Remaining <= 0 then
      Exit;
    if Length(Content) > Remaining then
      SetLength(Content, Remaining);

    // write data
    Written := WriteDataToFile(FPConfig^.Download[Channel].TempFileName, Content);

    // Set Progressbar
    if Assigned(FMain.ProgressBar) then
    begin
      FMain.ProgressBar.Max := FPConfig^.Download[Channel].FileSize;
      FMain.ProgressBar.Position := Written;
      FMain.ProgressBar.Visible := True;
    end;

    if written >= FPConfig^.Download[Channel].FileSize then
    begin
      VerifyStream := TFileStream.Create(
        FPConfig^.Download[Channel].TempFileName, fmOpenRead or fmShareDenyWrite);
      try
        SetLength(VerifyData, VerifyStream.Size);
        if VerifyStream.Size > 0 then
          VerifyStream.ReadBuffer(VerifyData[0], VerifyStream.Size);
      finally
        VerifyStream.Free;
      end;
      CalculatedCRC := CalculateAutoBinCRC(VerifyData);
      if CalculatedCRC <> FPConfig^.Download[Channel].FileCRC then
      begin
        DeleteFile(FPConfig^.Download[Channel].TempFileName);
        FPConfig^.Download[Channel] := Default;
        Exit;
      end;

      FPConfig^.Channel[Channel].Writeln('Download Done');
      FMain.ProgressBar.Position := 0;
      FMain.ProgressBar.Visible := False;

      FName := FPConfig^.DirectoryAutobin + DirectorySeparator +
        ExtractFileName(FPConfig^.Download[Channel].FileName);
      if not RenameFile(FPConfig^.Download[Channel].TempFileName, FName) then
      begin
        FPConfig^.Channel[Channel].Writeln('Download failed: could not save file');
        Exit;
      end;
      FPConfig^.Download[Channel] := Default;
    end;
  end;
end;

{
  FileDownload

  For String Data

  If user accept filedownload, these procedure will call WriteDataToFile
  and check if the written data equal to the predicted file size.
}
procedure TFFileUpload.FileDownload(const ChannelBuffer: AnsiString; const Channel: Byte);
var FName, Go7Name: String;
    Written, MessageSize: Integer;
begin
  if Length(ChannelBuffer) > 0 then
  begin
    MessageSize := 0;
    FPConfig^.Download[Channel].LinesHeader += LineContainsKeyword(ChannelBuffer);

    // if bcm then + 2 because first line is empty and second is the BCM Header
    if FPConfig^.ConnectInfo[Channel].OpenBCM then
      MessageSize := FPConfig^.Download[Channel].LinesHeader + FPConfig^.Download[Channel].Lines + 2;

    // if bpq then use size not lines
    if FPConfig^.ConnectInfo[Channel].LinBPQ then
      MessageSize := FPConfig^.Download[Channel].FileSize;

    // Check if it's a Go7 File.
    GetGoSeven(ChannelBuffer, Channel);

    // write data
    Written := WriteDataToFile(FPConfig^.Download[Channel].TempFileName, ChannelBuffer, Channel);

    // Set Progressbar
    if Assigned(FMain.ProgressBar) then
    begin
      FMain.ProgressBar.Max := MessageSize;
      FMain.ProgressBar.Position := Written;
      FMain.ProgressBar.Visible := True;
    end;

    // The TempFileName is set in UMain in the SetMail Procedure
    if (Written >= MessageSize) or (FileEnd(ChannelBuffer)) then
    begin
      // change the temporary file name to the real filename
      FName := FPConfig^.DirectoryMail + DirectorySeparator + FPConfig^.Download[Channel].FileName;
      RenameFile(FPConfig^.Download[Channel].TempFileName, FName);

      FMain.ProgressBar.Position := 0;
      FMain.ProgressBar.Visible := False;

      // if it's a Go7 file, copy it into the 7plus directory with the 7plus filename
      if FPConfig^.Download[Channel].Go7 then
      begin
        Go7Name := FPConfig^.Directory7Plus + DirectorySeparator + FPConfig^.Download[Channel].Go7FileName;
        if Length(FPConfig^.Download[Channel].Go7FileName) > 0 then
          if not CopyFile(FName, Go7Name) then
            ShowMessage('Could not create ' + Go7Name);
      end;
      FPConfig^.Download[Channel] := Default;
    end;
  end;
end;

{
  FileEnd

  FileEnd will check if the Mail sending is finish. Thats important for the case,
  that the BBS software does not have the mail size as part of the mail header.
}
function TFFileUpload.FileEnd(const ChannelBuffer: AnsiString): Boolean;
begin
  Result := False;

  // For LinBPQ BBS
  if Pos('[End of', ChannelBuffer) > 0 then
    Result := True;
end;

{
  GetDateTime

  Get the Date and Timestamp of FileName.
}
function TFFileUpload.GetDateTime(const FileName: string): TDateTime;
var
  Info: TSearchRec;
begin
  Result := -1;
  if FileExists(FileName) and (FindFirst(FileName, faAnyFile, Info) = 0) then
  begin
    Result := Info.TimeStamp;
    FindClose(Info);
  end;
end;


procedure TFFileUpload.CancelButtonClick(Sender: TObject);
begin
  Close;
end;

procedure TFFileUpload.FormCreate(Sender: TObject);
begin
  OldWidth := Width;
  OldHeight := Height;
  cbTransfereProtocoll.ItemIndex := 2;
  TransferProtocol := fpAutoBin;
end;

procedure TFFileUpload.FormShow(Sender: TObject);
var FileSize: Int64;
    FileStream: TFileStream;
    CRC: Word;
begin
  // fix for wayland
  Height := OldHeight;
  Width := OldWidth;

  try
    FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
    try
      FileSize := FileStream.Size;
      SetLength(Buffer, FileSize);
      FileStream.ReadBuffer(Buffer[0], FileSize);
      CRC := CalculateAutoBinCRC(Buffer);
    finally
      FileStream.Free;
    end;
  except
    on E: Exception do
    begin
      ShowMessage('Could not read file: ' + E.Message);
      Exit;
    end;
  end;

  STFileName.Caption := ExtractFileName(FileName);
  STFileSize.Caption := IntToStr(FileSize) + ' bytes';

  AutoBin := CreateAutoBinHeader(FileName, FileSize, CRC, GetDateTime(FileName));
end;

procedure TFFileUpload.OKButtonClick(Sender: TObject);
begin
  case cbTransfereProtocoll.ItemIndex of
    0: TransferProtocol := fpYapp;
    1: TransferProtocol := fpYappC;
    2: TransferProtocol := fpAutoBin;
    3: TransferProtocol := fpDidadit;
  else
    TransferProtocol := fpAutoBin;
  end;
  if Assigned(FOnUpload) then
    FOnUpload(Self);
  Close;
end;

{
  IsAutoBin

  Check if the Message "Head" is a AutoBin Header. If it's so,
  return a String array with all header parts.
}
function TFFileUpload.IsAutoBin(const Head:string):TStrings;
begin
  Result := ParseAutoBinHeader(Head);
end;

{
  WriteDataToFile

  Fot TBytes

  This function is writin data into a file (FileName). If the file already
  exist, it append the data.
}
function TFFileUpload.WriteDataToFile(const FileName: String; const Data: TBytes):Integer;
var
  FileStream: TFileStream;
  NumBytes: Integer;
  Content: TBytes;
begin
  NumBytes := Length(Data);

  if FileExists(FileName) then
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    FileStream := TFileStream.Create(FileName, fmCreate);

  Content := Data;
  if Pos('stop_', TEncoding.UTF8.GetString(Content)) > 0 then
     SetLength(Content, Pos('stop_', TEncoding.UTF8.GetString(Content)));

  try
    FileStream.Seek(0, soEnd);
    FileStream.Write(SwapEndian(Content[0]), NumBytes);
  except
    on E: Exception do
    begin
      {$IFDEF UNIX}
      writeln('FileDownload Error: ', E.Message);
      {$ENDIF}
    end;
  end;
  Result := FileStream.Size;
  FileStream.Free;
end;

{
  WriteDataToFile

  For String

  This function is writing data into a file (FileName). If the file already
  exist, it append the data.
}
function TFFileUpload.WriteDataToFile(const FileName: String; const Data: AnsiString; const Channel:Integer): Integer;
var FileStream: TFileStream;
    DataBytes: TBytes;
    LineBuffer: TMemoryStream;
    Line: AnsiString;
    c: AnsiChar;
begin
  Result := 0;

  Line := StringReplace(Data, #13#10, #10, [rfReplaceAll]);  // Unix
  Line := StringReplace(Line, #13, #10, [rfReplaceAll]);     // old mac
  Line := StringReplace(Line, #10, #13#10, [rfReplaceAll]);

  DataBytes := BytesOf(Line);

  if FileExists(FileName) then
    FileStream := TFileStream.Create(FileName, fmOpenReadWrite or fmShareDenyNone)
  else
    FileStream := TFileStream.Create(FileName, fmCreate);

  try
    FileStream.Seek(0, soEnd);
    FileStream.WriteBuffer(DataBytes[0], Length(DataBytes));
  finally
    FileStream.Free;
  end;

  // count lines
  LineBuffer := TMemoryStream.Create;
  try
    LineBuffer.LoadFromFile(FileName);

    // Use number of line with OpenBCP
    if FPConfig^.ConnectInfo[Channel].OpenBCM then
    begin
      LineBuffer.Position := 0;

      while LineBuffer.Position < LineBuffer.Size do
      begin
        Line := '';
        while (LineBuffer.Position < LineBuffer.Size) do
        begin
          LineBuffer.ReadBuffer(c, 1);
          if c = #10 then Break;
          Line := Line + c;
        end;
        Inc(Result);
      end;
    end;
    // Use number of bytes with LinBPQ
    if FPConfig^.ConnectInfo[Channel].LinBPQ then
      Result := LineBuffer.Size;

  finally
    LineBuffer.Free;
  end;
end;

{
  GetGoSeven

  Check if "Data" is an go7+ message.
}
procedure TFFileUpload.GetGoSeven(const Data: AnsiString; const Channel: Byte);
var Regex: TRegExpr;
    FName, FExt: String;
begin
  if (Length(Data) = 0) then
    Exit;

  if Pos('go_7+.',Data) > 0 then
  begin
    FPConfig^.Download[Channel].Go7 := True;
    FPConfig^.Download[Channel].Header := Copy(Data, Pos('go_7+.',Data), Length(Data));

    if Length(FPConfig^.Download[Channel].Header) < 70 then
      Exit;
  end;

  if (FPConfig^.Download[Channel].Go7) and (Length(FPConfig^.Download[Channel].Header) < 70) then
    FPConfig^.Download[Channel].Header := FPConfig^.Download[Channel].Header + Copy(Data, 1, Length(Data));

  Regex := TRegExpr.Create;
  Regex.Expression := 'stop_7.*\((\S+)\/.* ';
  Regex.ModifierI := True;

  if Regex.Exec(Data) then
  begin
    FExt := LowerCase(ExtractFileExt(Regex.Match[1]));
    FName := ChangeFileExt(Regex.Match[1], FExt);
    FPConfig^.Download[Channel].Go7FileName := FName;
  end
end;

{
  LineContainsKeyword

  Count number ob lines in the header
}
function TFFileUpload.LineContainsKeyword(const Line: String): Integer;
var i: Integer;
    HeaderKeywords: TStringList;
    CleanLine: String;
begin
  Result := 0;

  if Length(Line) <= 0 then
    Exit;

  HeaderKeywords := TStringList.Create;
  HeaderKeywords.Add('Read:');
  HeaderKeywords.Add('Subj:');
  HeaderKeywords.Add('Path:');
  HeaderKeywords.Add('Sent:');
  HeaderKeywords.Add('From:');
  HeaderKeywords.Add('To:');
  HeaderKeywords.Add('X-Info:');
  HeaderKeywords.Add('BID:');
  HeaderKeywords.Add('Bid:');
  HeaderKeywords.Add('MID:');
  HeaderKeywords.Add('Title:');
  HeaderKeywords.Add('Date/Time:');   // LinBPQ
  HeaderKeywords.Add('Body:');        // LinBPQ
  HeaderKeywords.Add('Type/Status:'); // LinBPQ

  CleanLine := StringReplace(Line, ' ', '', [rfReplaceAll]);
  CleanLine := StringReplace(CleanLine, #13, '', [rfReplaceAll]);     // Mac Classic
  CleanLine := StringReplace(CleanLine, #10, '', [rfReplaceAll]);  // Unix

  for i := 0 to HeaderKeywords.Count - 1 do
    if Pos(HeaderKeywords[i], CleanLine) > 0 then
      inc(Result);
end;

function TFFileUpload.FileProtocolDownload(const ChannelBuffer: TBytes;
  const Channel: Byte): Boolean;
var
  State: PDownload;
  Frame, Payload, Ack, Remaining: TBytes;
  FrameSize, PayloadSize, I: Integer;
  Kind: TYappPacketKind;
  YappFileName: String;
  Size: Int64;
  Stream: TFileStream;
begin
  Result := False;
  if Length(ChannelBuffer) = 0 then
    Exit;

  State := @FPConfig^.Download[Channel];
  if State^.Protocol = Ord(fpUnknown) then
    State^.Protocol := Ord(DetectFileProtocol('', ChannelBuffer));
  if (State^.Protocol <> Ord(fpYapp)) and
     (State^.Protocol <> Ord(fpYappC)) then
    Exit;
  Result := True;

  I := Length(State^.ProtocolBuffer);
  SetLength(State^.ProtocolBuffer, I + Length(ChannelBuffer));
  Move(ChannelBuffer[0], State^.ProtocolBuffer[I], Length(ChannelBuffer));

  while Length(State^.ProtocolBuffer) >= 2 do
  begin
    if (State^.Protocol = Ord(fpYappC)) and
       YappCNegotiation(State^.ProtocolBuffer) then
    begin
      SetLength(State^.ProtocolBuffer, Length(State^.ProtocolBuffer) - 2);
      Continue;
    end;

    PayloadSize := State^.ProtocolBuffer[1];
    if PayloadSize = 0 then
      PayloadSize := 256;
    FrameSize := PayloadSize + 2;
    if Length(State^.ProtocolBuffer) < FrameSize then
      Break;

    SetLength(Frame, FrameSize);
    Move(State^.ProtocolBuffer[0], Frame[0], FrameSize);
    Remaining := Copy(State^.ProtocolBuffer, FrameSize,
      Length(State^.ProtocolBuffer) - FrameSize);
    State^.ProtocolBuffer := Remaining;

    SetLength(Payload, PayloadSize);
    if PayloadSize > 0 then
      Move(Frame[2], Payload[0], PayloadSize);
    Kind := YappPacketKind(Frame[0], Frame[1]);
    case Kind of
      ypHeader:
        if YappHeaderFields(Payload, YappFileName, Size) then
        begin
          State^.FileName := ExtractFileName(YappFileName);
          State^.FileSize := Size;
          State^.TempFileName := GetTempFileName(FPConfig^.DirectoryAutoBin, 'yapp');
          State^.Enabled := True;
          Ack := YappPacket(YAPP_ACK, TBytes.Create(2));
          FMain.SendByteCommand(Channel, 0, Ack);
        end;
      ypData:
        begin
          if (State^.Protocol = Ord(fpYappC)) and
             (not YappCChecksumValid(Payload)) then
          begin
            Ack := YappPacket(YAPP_CAN, nil);
            FMain.SendByteCommand(Channel, 0, Ack);
            Exit;
          end;
          if State^.Protocol = Ord(fpYappC) then
            SetLength(Payload, Length(Payload) - 1);
          if Length(Payload) > 0 then
          begin
            Stream := TFileStream.Create(State^.TempFileName,
              fmOpenReadWrite or fmShareDenyNone);
            try
              Stream.Seek(0, soEnd);
              Stream.WriteBuffer(Payload[0], Length(Payload));
            finally
              Stream.Free;
            end;
          end;
        end;
      ypEOF:
        begin
          Ack := YappPacket(YAPP_ACK, TBytes.Create(3));
          FMain.SendByteCommand(Channel, 0, Ack);
        end;
      ypEOT:
        begin
          Ack := YappPacket(YAPP_ACK, TBytes.Create(4));
          FMain.SendByteCommand(Channel, 0, Ack);
          if FileExists(State^.TempFileName) and (State^.FileName <> '') then
            RenameFile(State^.TempFileName,
              FPConfig^.DirectoryAutoBin + DirectorySeparator + State^.FileName);
          State^ := Default;
        end;
    end;
  end;
end;

function TFFileUpload.Default:TDownload;
begin
  Result.Enabled := False;
  Result.FileSize := 0;
  Result.BlockSize := 0;
  Result.FileCRC := 0;
  Result.FileName := '';
  Result.Go7FileName := '';
  Result.TempFileName := '';
  Result.PartNumber := 0;
  Result.TotalParts := 0;
  Result.AutoBin := False;
  Result.Mail := False;
  Result.Lines := 0;
  Result.LinesHeader := 0;
  Result.Header := '';
  Result.Go7 := False;
  Result.Protocol := 0;
  Result.ProtocolBuffer := nil;
end;

end.

