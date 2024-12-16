unit uaddressbook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ExtCtrls, Buttons, DB, Clipbrd, ActnList, uagwpeclient,
  utypes;

type

  { TTFAdressbook }

  TTFAdressbook = class(TForm)
    APasswordShortCut: TAction;
    ADeleteShortCut: TAction;
    AQuickCallShortCut: TAction;
    ActionList1: TActionList;
    BBSave: TBitBtn;
    BBNew: TBitBtn;
    BBDel: TBitBtn;
    BBEdit: TBitBtn;
    BBQuickConnect: TBitBtn;
    BBPassword: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CBType: TComboBox;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LPasswordCount: TLabel;
    LEPassword: TLabeledEdit;
    LECity: TLabeledEdit;
    LELocator: TLabeledEdit;
    LEConnectVia: TLabeledEdit;
    LECallSign: TLabeledEdit;
    LBCallsign: TListBox;
    MNote: TMemo;
    Panel1: TPanel;
    SQLC: TSQLConnector;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    TClipboardClean: TTimer;
    procedure BBDelClick(Sender: TObject);
    procedure BBPasswordClick(Sender: TObject);
    procedure BBSaveClick(Sender: TObject);
    procedure BBNewClick(Sender: TObject);
    procedure BBEditClick(Sender: TObject);
    procedure BBQuickConnectClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CheckCallsign(Sender: TObject);
    procedure SelectCall(Sender: TObject);
    procedure ShowAdressbook(Sender: TObject);
    procedure TClipboardCleanTimer(Sender: TObject);
  private
    FOnQuickConnect: TNotifyEvent;
    procedure UpdateList;
    function CallSignExist(callsign: String):Boolean;
  public
    function GetCallsign:String;
    function ExtractPassword(const Password: string; const Positions: String): string;
    property OnQuickConnect: TNotifyEvent read FOnQuickConnect write FOnQuickConnect;
  end;

var
  TFAdressbook: TTFAdressbook;
  FPConfig: PTFPConfig;
  EditCallsign: Boolean;
  TimeBeforeClean: Byte;

implementation

{$R *.lfm}

{ TTFAdressbook }

function TTFAdressbook.GetCallsign:String;
begin
  Result :=  LECallsign.Text;
end;

procedure TTFAdressbook.BtnCloseClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLTransaction.Active := False;
  SQLC.Connected := False;

  Close;
end;

{
  CheckCallsign

  Is the onChange action procedure of the Callsing Edit field.
  This procedure will check if the Callsign already exist and
  enable/disable buttons.
}
procedure TTFAdressbook.CheckCallsign(Sender: TObject);
begin
  if Length(LECallsign.Text) > 0 then
  begin
    BBQuickConnect.Enabled := True;
    BBSave.Enabled := False;
    BBDel.Enabled := True;
    BBEdit.Enabled := True;
    if Length(LEPassword.Text) > 0 then
      BBPassword.Enabled := True;

    if not CallSignExist(LECallsign.Text) then
    begin
      BBSave.Enabled := True;
      BBDel.Enabled := False;
      BBEdit.Enabled := False;
      BBQuickConnect.Enabled := False;
      BBPassword.Enabled := False;
    end;
  end;
end;

procedure TTFAdressbook.SelectCall(Sender: TObject);
var i: Byte;
  fields: array[0..6] of String;
begin
  If LBCallsign.ItemIndex >= 0 then
  begin
    SQLQuery.Close;
    SQLQuery.SQL.Text := 'SELECT callsign, locator, note, type, via, city, password FROM "ADR" WHERE "callsign" LIKE :callsign limit 1';
    SQLQuery.Params.ParamByName('callsign').AsString := LBCallsign.Items[LBCallsign.ItemIndex];
    SQLQuery.Open;

    if not SQLQuery.IsEmpty then
    begin
      SQLQuery.First;

      // There seams to be a bug in TFields. I cannot access fields without
      // access violation or out of bounds via SQLQuery on a propper way.
      for i:=0 to 6 do
      begin
        fields[i] := SQLQuery.Fields[i].AsString;
      end;

      LEPassword.Text := fields[6];
      LECallsign.Text := fields[0];
      LELocator.Text := fields[1];
      LEConnectVia.Text := fields[4];
      LECity.Text := fields[5];


      MNote.Clear;
      MNote.Lines.Add(fields[2]);

      for i := 0 to CBType.Items.Count - 1 do
      begin
        if CBType.Items[i] = fields[3] then
        begin
          CBType.ItemIndex := i;
          Break;
        end;
      end;
      CheckCallsign(Sender);
      SQLQuery.Close;
    end;
  end;
end;

procedure TTFAdressbook.BBSaveClick(Sender: TObject);
begin
  SQLQuery.Close;

  if EditCallsign then
    SQLQuery.SQL.Text := 'UPDATE ADR SET password = :password, locator = :locator, note = :note, type = :type, via = :via, city = :city WHERE callsign = :callsign'
  else
    SQLQuery.SQL.Text := 'INSERT INTO ADR (callsign, password, locator, note, type, via, city) VALUES (:callsign, :password, :locator, :note, :type, :via, :city)';

  SQLQuery.Params.ParamByName('callsign').AsString := LECallsign.Text;
  SQLQuery.Params.ParamByName('password').AsString := LEPassword.Text;
  SQLQuery.Params.ParamByName('locator').AsString := LELocator.Text;
  SQLQuery.Params.ParamByName('type').AsString := CBType.Items[CBType.ItemIndex];
  SQLQuery.Params.ParamByName('via').AsString := LEConnectVia.Text;
  SQLQuery.Params.ParamByName('note').AsString := MNote.Lines.Text;
  SQLQuery.Params.ParamByName('city').AsString := LECity.Text;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;

  BBSave.Enabled := False;
  BBDel.Enabled := False;
  EditCallsign := False;

  UpdateList;
  BBNew.Click;
  CheckCallsign(Sender);
end;

procedure TTFAdressbook.BBPasswordClick(Sender: TObject);
var pwd: String;
begin
  pwd := ExtractPassword(LEPassword.Text, Clipboard.AsText);
  if Length(pwd) > 0 then
  begin
    Clipboard.AsText := pwd;
    TClipboardClean.Enabled := True;
  end;
end;

procedure TTFAdressbook.BBDelClick(Sender: TObject);
begin
  SQLQuery.SQL.Text := 'DELETE FROM ADR WHERE callsign = :callsign';
  SQLQuery.Params.ParamByName('callsign').AsString := LECallsign.Text;
  SQLQuery.ExecSQL;
  SQLTransaction.Commit;
  BBDel.Enabled := False;
  BBSave.Enabled := False;
  BBEdit.Enabled := False;
  BBQuickConnect.Enabled := False;
  UpdateList;
  BBNew.Click;
end;

procedure TTFAdressbook.BBNewClick(Sender: TObject);
begin
  LECallsign.Text := '';
  LELocator.Text := '';
  CBType.ItemIndex := 0;
  LEConnectVia.Text := '';
  MNote.Lines.Text := '';
  LECity.Text := '';
  LEPassword.Text := '';
  LBCallsign.ClearSelection;
end;

procedure TTFAdressbook.BBEditClick(Sender: TObject);
begin
  EditCallsign := True;
  BBSave.Enabled := True;
end;


procedure TTFAdressbook.BBQuickConnectClick(Sender: TObject);
begin
  if Assigned(FOnQuickConnect) then
    FOnQuickConnect(Self);
  Close;
end;

procedure TTFAdressbook.ShowAdressbook(Sender: TObject);
var HomeDir: String;
begin
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'\.flexpacket\';
  {$ENDIF}
  try
    SQLC.Close;
    SQLC.DatabaseName := HomeDir + 'adr.sqlite';
  except
    ShowMessage('Unable to use SQLite.');
    Exit;
  end;

  try
    if not FileExists(SQLC.DatabaseName) then
    begin
      try
        SQLC.Open;
        SQLTransaction.Active := true;

        SQLC.ExecuteDirect('CREATE TABLE "ADR"('+
          ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
          ' "callsign" Char(20) NOT NULL UNIQUE DEFAULT '''','+
          ' "via" Char(20) NULL DEFAULT '''','+
          ' "type" Char(10) NULL DEFAULT '''','+
          ' "city" Char(128) NULL DEFAULT '''','+
          ' "password" Char(100) NULL DEFAULT '''','+
          ' "note" Char(500) NULL DEFAULT '''','+
          ' "locator" Char(10) NULL DEFAULT '''');');

        SQLC.ExecuteDirect('CREATE UNIQUE INDEX "Data_id_idx" ON "ADR"( "id" );');

        SQLTransaction.Commit;
      except
        on E: Exception do
        begin
          ShowMessage('Unable to Create new Database: ' + E.Message);
          Exit;
        end;
      end;
    end;
  except
    ShowMessage('Unable to check if database file exists');
    Exit;
  end;

  SQLC.Connected := True;
  SQLTransaction.Active := True;

  UpdateList;
end;

procedure TTFAdressbook.TClipboardCleanTimer(Sender: TObject);
begin
  LPasswordCount.Caption := IntToStr(10 - TimeBeforeClean);
  if TimeBeforeClean = 10 then
  begin
    TimeBeforeClean := 0;
    TClipboardClean.Enabled := False;
    LPasswordCount.Caption := '';
  end;
  inc(TimeBeforeClean);
end;

procedure TTFAdressbook.UpdateList;
begin
  LBCallsign.Clear;

  // Show Callsignes in LB Callsign
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT callsign FROM "ADR"';
  SQLQuery.Open;
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    LBCallsign.Items.Add(SQLQuery.FieldByName('callsign').AsString);
    SQLQuery.Next;
  end;
  LBCallsign.Repaint;
end;

function TTFAdressbook.CallSignExist(callsign: String):Boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT callsign FROM "ADR" where "callsign" like :callsign';
  SQLQuery.Params.ParamByName('callsign').AsString := callsign;
  SQLQuery.Open;

  Result := False;
  if SQLQuery.RecordCount > 0 then
    Result := True;

end;


function TTFAdressbook.ExtractPassword(const Password: string; const Positions: String): string;
var
  x, i: Integer;
  PositionStrings: TStringArray;
begin
  PositionStrings := Positions.Split([' ']);
  Result := '';
  if Length(PositionStrings) <> 5 then
    Exit;

  for i := 0 to High(PositionStrings) do
  begin
    if not TryStrToInt(Trim(PositionStrings[i]), x) then
      raise Exception.CreateFmt('Password Error: "%s"', [Positions[i]]);

    if (x < 1) or (x > Length(Positions)) then
      raise Exception.CreateFmt('Wrong Password position: %d', [x]);

    Result := Result + Password[x];
  end;
end;
end.



