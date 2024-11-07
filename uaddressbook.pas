unit uaddressbook;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, SQLDB, SQLite3Conn, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ButtonPanel, ValEdit, ExtCtrls;

type

  { TTFAdressbook }

  TTFAdressbook = class(TForm)
    ButtonPanel1: TButtonPanel;
    CBType: TComboBox;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    LECity: TLabeledEdit;
    LELocator: TLabeledEdit;
    LEConnectVia: TLabeledEdit;
    LECallSign: TLabeledEdit;
    LBCallsignes: TListBox;
    Panel1: TPanel;
    SQLC: TSQLConnector;
    SQLTransaction: TSQLTransaction;
    procedure ButtonPanel1Click(Sender: TObject);
    procedure ShowAdressbook(Sender: TObject);
  private

  public

  end;

var
  TFAdressbook: TTFAdressbook;

implementation

{$R *.lfm}

{ TTFAdressbook }

procedure TTFAdressbook.ButtonPanel1Click(Sender: TObject);
begin
  Close;
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

        SQLC.ExecuteDirect('CREATE TABLE "DATA"('+
                      ' "id" Integer NOT NULL PRIMARY KEY AUTOINCREMENT,'+
                      ' "callsign" Char(20) NOT NULL,'+
                      ' "via" Char(20) NULL,'+
                      ' "type" Char(10) NULL,'+
                      ' "city" Char(128) NULL,'+
                      ' "locatior" Char(10) NULL);');

          // Creating an index based upon id in the DATA Table
        SQLC.ExecuteDirect('CREATE UNIQUE INDEX "Data_id_idx" ON "DATA"( "id" );');

        SQLTransaction.Commit;

      except
        ShowMessage('Unable to Create new Database');
      end;
    end;
  except
    ShowMessage('Unable to check if database file exists');
  end;
end;

end.

