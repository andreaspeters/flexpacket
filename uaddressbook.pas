unit uaddressbook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ValEdit, ExtCtrls, Buttons;

type

  { TTFAdressbook }

  TTFAdressbook = class(TForm)
    BBAdd: TBitBtn;
    BBNew: TBitBtn;
    BBDel: TBitBtn;
    ButtonPanel1: TButtonPanel;
    CBType: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LECity: TLabeledEdit;
    LELocator: TLabeledEdit;
    LEConnectVia: TLabeledEdit;
    LECallSign: TLabeledEdit;
    LBCallsign: TListBox;
    Panel1: TPanel;
    SQLC: TSQLConnector;
    SQLQuery: TSQLQuery;
    SQLTransaction: TSQLTransaction;
    procedure BBAddClick(Sender: TObject);
    procedure BtnCloseClick(Sender: TObject);
    procedure CheckCallsign(Sender: TObject);
    procedure SelectCall(Sender: TObject; User: boolean);
    procedure ShowAdressbook(Sender: TObject);
  private
    procedure UpdateList;
    function CallSignExist(callsign: String):Boolean;
  public

  end;

var
  TFAdressbook: TTFAdressbook;

implementation

{$R *.lfm}

{ TTFAdressbook }

procedure TTFAdressbook.BtnCloseClick(Sender: TObject);
begin
  SQLQuery.Close;
  SQLTransaction.Active := False;
  SQLC.Connected := False;

  Close;
end;

procedure TTFAdressbook.CheckCallsign(Sender: TObject);
begin
  BBAdd.Enabled := False;
  BBDel.Enabled := True;
  if not CallSignExist(LECallsign.Text) then
  begin
    BBAdd.Enabled := True;
    BBDel.Enabled := False;
  end;
end;

procedure TTFAdressbook.SelectCall(Sender: TObject; User: boolean);
var i: Byte;
begin
  // Get selected call
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT * FROM "ADR" where "callsign" like :callsign';
  SQLQuery.Params.ParamByName('callsign').AsString := LBCallsign.Items[LBCallsign.ItemIndex];
  SQLQuery.Open;
  SQLQuery.First;
  while not SQLQuery.EOF do
  begin
    LECallsign.Text := SQLQuery.FieldByName('callsign').AsString;
    LELocator.Text := SQLQuery.FieldByName('locator').AsString;
    LECity.Text := SQLQuery.FieldByName('city').AsString;
    LEConnectVia.Text := SQLQuery.FieldByName('via').AsString;

    for i := 0 to CBType.Items.Count - 1 do
    begin
      if CBType.Items[i] = SQLQuery.FieldByName('type').AsString then
        CBType.ItemIndex := i;
    end;

    SQLQuery.Next;
  end;

  SQLQuery.Close;
end;

procedure TTFAdressbook.BBAddClick(Sender: TObject);
begin
  SQLQuery.SQL.Text := 'INSERT INTO ADR (callsign, locator, type, via, city) VALUES (:callsign, :locator, :type, :via, :city)';
  SQLQuery.Params.ParamByName('callsign').AsString := LECallsign.Text;
  SQLQuery.Params.ParamByName('locator').AsString := LELocator.Text;
  SQLQuery.Params.ParamByName('type').AsString := CBType.Items[CBType.ItemIndex];
  SQLQuery.Params.ParamByName('via').AsString := LEConnectVia.Text;
  SQLQuery.Params.ParamByName('city').AsString := LECity.Text;

  SQLQuery.ExecSQL;
  SQLTransaction.Commit;

  UpdateList;
end;

procedure TTFAdressbook.ShowAdressbook(Sender: TObject);
var HomeDir: String;
begin
  {$IFDEF UNIX}
  HomeDir := GetEnvironmentVariable('HOME')+'/.config/flexpacket/';
  {$ELSE}
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
  SQLQuery.Close;
end;

function TTFAdressbook.CallSignExist(callsign: String):Boolean;
begin
  SQLQuery.Close;
  SQLQuery.SQL.Text := 'SELECT callsign FROM "ADR" where "callsign" like :callsign';
  SQLQuery.Params.ParamByName('callsign').AsString := callsign;
  SQLQuery.Open;

  writeln( SQLQuery.RecordCount);
  Result := False;
  if SQLQuery.RecordCount > 0 then
    Result := True;

  SQLQuery.Close;
end;

end.

