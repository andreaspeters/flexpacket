unit AX25Helper;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BaseUnix, sockets, Classes, StdCtrls, uchannel;

const
  AX25_MTU = 256;  // Maximale Paketgröße für AX.25
  CALLSIGN = 'DC6AP-2';  // Das Rufzeichen, nach dem gefiltert wird

type
  // Struktur für AX.25 Adressen (7 Byte für Rufzeichen + SSID)
  TAX25Address = record
    callsign: array[0..5] of char;  // Rufzeichen (6 Zeichen)
    ssid: Byte;                     // SSID
  end;

  // Struktur für ein AX.25 Paket
  TAX25Packet = record
    dest: TAX25Address;     // Ziel-Rufzeichen
    source: TAX25Address;   // Quell-Rufzeichen
    control: Byte;          // Steuerbyte
    pid: Byte;              // Protokoll-Identifikator
    info: array[0..AX25_MTU-1] of Byte;  // Paketdaten
  end;

  // Thread zur parallelen Überwachung von AX.25 Paketen
  TReadAX25PacketsThread = class(TThread)
  private
    sock: Integer;
    FPacket: TAX25Packet;  // Paketdaten, die vom Thread verarbeitet werden
  protected
    procedure Execute; override;
    procedure DisplayPacketInfo;  // Die Synchronisierungsprozedur darf keine Parameter haben
  public
    constructor Create(CreateSuspended: Boolean; AInterface: string; Memo: TMemo);
    destructor Destroy; override;
  end;

  // Öffne ein AX.25-Socket auf einem Interface
  function OpenAX25Socket(const iface: string): Integer;

  // Hilfsfunktion: Konvertiert ein Rufzeichen aus der AX.25 Struktur in einen String
  function AX25CallsignToString(const addr: TAX25Address): string;

  // Prüft, ob das Rufzeichen dem Ziel-Rufzeichen entspricht
  function IsTargetCallsign(const packet: TAX25Packet): Boolean;


implementation

// Öffne ein AX.25-Socket auf dem angegebenen Interface
function OpenAX25Socket(const iface: string): Integer;
var
  sock: Integer;
  addr: sockaddr;
begin
  sock := FpSocket(AF_AX25, SOCK_DGRAM, 0);
  if sock < 0 then
  begin
    Result := -1;
    Exit;
  end;

  FillChar(addr, SizeOf(addr), 0);
  addr.sa_family := AF_AX25;
  Move(iface[1], addr.sa_data, Length(iface));

  if FpBind(sock, @addr, SizeOf(addr)) < 0 then
  begin
    FpClose(sock);
    Result := -1;
    Exit;
  end;

  Result := sock;
end;

// Hilfsfunktion: Konvertiert ein Rufzeichen aus der AX.25 Struktur in einen String
function AX25CallsignToString(const addr: TAX25Address): string;
var
  i: Integer;
  cs: string;
begin
  cs := '';
  for i := 0 to 5 do
    cs := cs + addr.callsign[i];
  Result := Trim(cs) + '-' + IntToStr((addr.ssid shr 1) and $0F); // SSID extrahieren
end;

// Prüft, ob das Rufzeichen dem Ziel-Rufzeichen entspricht
function IsTargetCallsign(const packet: TAX25Packet): Boolean;
begin
  Result := AX25CallsignToString(packet.dest) = CALLSIGN;
end;

// Thread zum parallelen Lesen von AX.25 Paketen
constructor TReadAX25PacketsThread.Create(CreateSuspended: Boolean; AInterface: string; Memo: TMemo);
begin
  inherited Create(CreateSuspended);
  sock := OpenAX25Socket(AInterface);
  if sock < 0 then
    raise Exception.Create('Fehler beim Erstellen des AX.25 Sockets auf ' + AInterface);
end;

destructor TReadAX25PacketsThread.Destroy;
begin
  if sock >= 0 then
    FpClose(sock);
  inherited Destroy;
end;

// Diese Methode wird im separaten Thread ausgeführt
procedure TReadAX25PacketsThread.Execute;
var
  packet: TAX25Packet;
  recvBytes: ssize_t;
begin
  while not Terminated do
  begin
    recvBytes := FpRecv(sock, @packet, SizeOf(packet), 0);

    //if (recvBytes > 0) and IsTargetCallsign(packet) then
    //begin
      Synchronize(@DisplayPacketInfo);
    //end;
  end;
end;

// Diese Methode zeigt die empfangenen Paketinformationen in der Memo-Komponente an
procedure TReadAX25PacketsThread.DisplayPacketInfo;
begin
  UChannel.FChannel.Mrx.Lines.Add('Paket empfangen von: ' + AX25CallsignToString(FPacket.source));
  UChannel.FChannel.Mrx.Lines.Add('Paketinhalt: ' + PChar(@FPacket.info[0]));
end;

end.

