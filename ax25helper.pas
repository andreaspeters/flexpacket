unit AX25Helper;

{$mode objfpc}{$H+}
{$linklib ax25} // Link zu libax25

interface

uses
  SysUtils, Classes, sockets, ctypes, syncobjs,
  Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  SynEdit, SynPluginSyncroEdit, BaseUnix;

const
  MAX_CALLSIGN_LENGTH = 7; // Maximaler Platz für Rufzeichen (6 + 1 für Nullterminierung)
  MAX_PACKET_SIZE = 256;   // Maximale Paketgröße

type
  sockaddr_ax25 = record
    sax25_family: Integer;             // Adressfamilie (AF_AX25)
    sax25_call: array[0..6] of char;   // AX.25 Rufzeichen (6 Zeichen + Nullterminierung)
    sax25_ndigis: Byte;                // Anzahl der Digipeater
  end;

  { TPacketReceiverThread }

  TPacketReceiverThread = class(TThread)
  private
    FSocket: Integer;
    FMemo: TMemo;
    procedure UpdateMemo(const Msg: string);
  protected
    procedure Execute; override;
  public
    constructor Create(AMemo: TMemo);
    destructor Destroy; override;
  end;

procedure StartPacketReceiving(Memo: TMemo);

implementation

function ax25_aton_entry(const callsign: PChar; out addr: array of char): Integer; cdecl; external 'ax25';

procedure StartPacketReceiving(Memo: TMemo);
var
  ReceiverThread: TPacketReceiverThread;
begin
  ReceiverThread := TPacketReceiverThread.Create(Memo);
  ReceiverThread.FreeOnTerminate := True; // Thread automatisch freigeben
  ReceiverThread.Start;
end;

{ TPacketReceiverThread }

constructor TPacketReceiverThread.Create(AMemo: TMemo);
var
  my_addr: sockaddr_ax25;
begin
  inherited Create(True); // Erstelle den Thread im gestoppten Zustand
  FSocket := FpSocket(AF_AX25, SOCK_SEQPACKET, 0);
  if FSocket < 0 then
  begin
    Raise Exception.Create('Error opening socket');
  end;

  // Initialisiere die lokale Adresse
  FillChar(my_addr, SizeOf(my_addr), 0);
  my_addr.sax25_family := AF_AX25;
  ax25_aton_entry(PChar('VK7NTK-1'), my_addr.sax25_call); // Ersetze mit deinem Callsign
  my_addr.sax25_ndigis := 0;

  // Binde das Socket
  if FpBind(FSocket, PPointer(@my_addr)^, SizeOf(my_addr)) < 0 then
  begin
    CloseSocket(FSocket);
    Raise Exception.Create('Error binding');
  end;

  FMemo := AMemo;
end;

destructor TPacketReceiverThread.Destroy;
begin
  CloseSocket(FSocket);
  inherited Destroy;
end;

procedure TPacketReceiverThread.UpdateMemo(const Msg: string);
begin
  // Update des Memo-Feldes im Hauptthread
  if Assigned(FMemo) then
  begin
    FMemo.Lines.Add(Msg);
  end;
end;

procedure TPacketReceiverThread.Execute;
var
  packet: array[0..MAX_PACKET_SIZE - 1] of char;
  recvBytes: ssize_t;
begin
  while not Terminated do
  begin
    // Lese das empfangene AX.25 Paket
    recvBytes := FpRecv(FSocket, @packet, SizeOf(packet), 0);

    // Überprüfe, ob ein Paket empfangen wurde
    if recvBytes > 0 then
    begin
      // Konvertiere das Paket in einen String und aktualisiere das Memo
      UpdateMemo('Paket empfangen: ' + PChar(@packet));
    end;
  end;
end;

end.

