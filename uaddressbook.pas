unit uaddressbook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ValEdit, ExtCtrls, Buttons, DB, utypes, uhostmode, uagwpeclient;

type

  { TTFAdressbook }

  PTFPConfig = ^TFPConfig;
  PTAGWPEClient = ^TAGWPEClient;
  PTHostmode = ^THostmode;

  TTFAdressbook = class(TForm)
    BBAdd: TBitBtn;
    BBNew: TBitBtn;
    BBDel: TBitBtn;
    BBQuickConnect: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CBType: TComboBox;
    GroupBox1: TGroupBox;
    ImageList1: TImageList;
    Label1: TLabel;
    Label2: TLabel;
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
    procedure BBAddClick(Sender: TObject);
    procedure BBQuickConnectClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CheckCallsign(Sender: TObject);
    procedure SelectCall(Sender: TObject; User: boolean);
    procedure ShowAdressbook(Sender: TObject);
  private
    procedure UpdateList;
    function CallSignExist(callsign: String):Boolean;
  public
    procedure SetChannel(Channel: Byte);
    procedure SetAGWClient(AGW: PTAGWPEClient);
    procedure SetHostmode(HM: PTHostmode);
    procedure SetConfig(Config: PTFPConfig);
  end;

var
  TFAdressbook: TTFAdressbook;
  FPConfig: PTFPConfig;
  Hostmode: PTHostmode;
  AGWClient: PTAGWPEClient;
  CurrentChannel: Byte;

implementation

{$R *.lfm}

{ TTFAdressbook }

procedure TTFAdressbook.SetAGWClient(AGW: PTAGWPEClient);
begin
  AGWClient := AGW;
end;

procedure TTFAdressbook.SetHostmode(HM: PTHostmode);
begin
  Hostmode := HM;
end;


procedure TTFAdressbook.SetConfig(Config: PTFPConfig);
begin
  FPConfig := Config;
end;


procedure TTFAdressbook.SetChannel(Channel: Byte);
begin
  CurrentChannel := Channel;
end;

procedure TTFAdressbook.BtnCloseClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLTransaction.Active := False;
  SQLC.Connected := False;

  Close;
end;

procedure TTFAdressbook.CheckCallsign(Sender: TObject);
begin
  BBQuickConnect.Enabled := True;
  BBAdd.Enabled := False;
  BBDel.Enabled := True;
  if not CallSignExist(LECallsign.Text) then
  begin
    BBQuickConnect.Enabled := False;
    BBAdd.Enabled := True;
    BBDel.Enabled := False;
  end;
end;

procedure TTFAdressbook.SelectCall(Sender: TObject; User: boolean);
var i: Byte;
  fields: array[0..5] of String;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT callsign, locator, note, type, via, city FROM "ADR" WHERE "callsign" LIKE :callsign limit 1';
  SQLQuery.Params.ParamByName('callsign').AsString := LBCallsign.Items[LBCallsign.ItemIndex];
  SQLQuery.Open;

  if not SQLQuery.IsEmpty then
  begin
    SQLQuery.First;

    // There seams to be a bug in TFields. I cannot access fields without
    // access violation or out of bounds via SQLQuery on a propper way.
    for i:=0 to 5 do
    begin
      fields[i] := SQLQuery.Fields[i].AsString;
    end;

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

    SQLQuery.Close;
  end;
end;

procedure TTFAdressbook.BBAddClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'INSERT INTO ADR (callsign, locator, note, type, via, city) VALUES (:callsign, :locator, :note, :type, :via, :city)';
  SQLQuery.Params.ParamByName('callsign').AsString := LECallsign.Text;
  SQLQuery.Params.ParamByName('locator').AsString := LELocator.Text;
  SQLQuery.Params.ParamByName('type').AsString := CBType.Items[CBType.ItemIndex];
  SQLQuery.Params.ParamByName('via').AsString := LEConnectVia.Text;
  SQLQuery.Params.ParamByName('note').AsString := MNote.Lines.Text;
  SQLQuery.Params.ParamByName('city').AsString := LECity.Text;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;

  UpdateList;
end;

procedure TTFAdressbook.BBQuickConnectClick(Sender: TObject);
begin
  if FPConfig^.EnableTNC then
    Hostmode^.SendByteCommand(CurrentChannel, 1, 'C ' + LECallsign.Text);

  if FPConfig^.EnableAGW then
    AGWClient^.SendByteCommand(0, 1, 'C ' + LECallsign.Text);
end;

procedure TTFAdressbook.ShowAdressbook(Sender: TObject);
var HomeDir: String;
begin
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  HomeDir := GetEnvironmentVariable('USERPROFILE')+'/flexpacket/';
  {$ENDIF}

  SQLC.Close;
  SQLC.DatabaseName := HomeDir + '/adr.sqlite';

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
          ' "note" Char(500) NULL DEFAULT '''','+
          ' "locator" Char(10) NULL DEFAULT '''');');

        SQLC.ExecuteDirect('CREATE UNIQUE INDEX "Data_id_idx" ON "ADR"( "id" );');

        SQLTransaction.Commit;
      except
        ShowMessage('Unable to Create new Database');
        Exit;
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

end.

