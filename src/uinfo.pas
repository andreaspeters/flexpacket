unit uinfo;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ButtonPanel, LCLIntf;

type

  { TTFInfo }

  TTFInfo = class(TForm)
    BPDefaultButtons: TButtonPanel;
    Image1: TImage;
    IGithubDonation: TImage;
    Image2: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LGithubDonation: TLabel;
    LFPSourceCode: TLabel;
    Memo1: TMemo;
    procedure CloseInfo(Sender: TObject);
    procedure IGithubDonationClick(Sender: TObject);
    procedure IFlexPacketSourceCodeClick(Sender: TObject);
  private

  public

  end;

var
  TFInfo: TTFInfo;

implementation

{$R *.lfm}

{ TTFInfo }

procedure TTFInfo.CloseInfo(Sender: TObject);
begin
  Close;
end;

procedure TTFInfo.IGithubDonationClick(Sender: TObject);
begin
  if not OpenURL('https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ') then
    ShowMessage('Could not open URL: https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ');
end;

procedure TTFInfo.IFlexPacketSourceCodeClick(Sender: TObject);
begin
  if not OpenURL('https://github.com/andreaspeters/flexpacket') then
    ShowMessage('Could not open URL: https://github.com/andreaspeters/flexpacket');
end;


end.

