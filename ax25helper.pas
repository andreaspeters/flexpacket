unit AX25Helper;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, BaseUnix, sockets;

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
    FMemo: TMemo;  // Memo-Referenz für die GUI-Ausgabe
  protected
    procedure Execute; override;
    procedure DisplayPacketInfo(const packet: TAX25Packet);
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

  // Prüft, ob eine AX.25 Verbindung aktiv ist (verbindungsorientiert)
  function AX25ConnectionStatus(const iface, destCallsign: string): Boolean;

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
    Result := -1;  // Fehler beim Erstellen des Sockets
    Exit;
  end;

  // Setze das Interface auf 'ax0'
  FillChar(addr, SizeOf(addr), 0);
  addr.sa_family := AF_AX25;
  Move(iface[1], addr.sa_data, Length(iface));

  // Binde das Socket an das AX.25 Interface
  if FpBind(sock, @addr, SizeOf(addr)) < 0 then
  begin
    FpClose(sock);
    Result := -1;  // Fehler beim Binden des Sockets
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
  FMemo := Memo;
  sock := OpenAX25Socket(AInterface);
  if sock < 0 then
    raise Exception.Create('Fehler beim Erstellen des AX.25 Sockets auf ' + AInterface);
end;

destructor TReadAX25PacketsThread.Destroy;
begin
  if sock >= 0 then
    FpClose(sock);  // Schließe das Socket beim Beenden des Threads
  inherited Destroy;
end;

// Diese Methode wird im separaten Thread ausgeführt
procedure TReadAX25PacketsThread.Execute;
var
  packet: TAX25Packet;
  recvBytes: ssize_t;
begin
  // Endlosschleife zum Lesen der Pakete (nicht blockierend für die Hauptanwendung)
  while not Terminated do
  begin
    // Lese das empfangene AX.25 Paket
    recvBytes := FpRecv(sock, @packet, SizeOf(packet), 0);

    // Überprüfe, ob das empfangene Paket für das Zielrufzeichen bestimmt ist
    if (recvBytes > 0) and IsTargetCallsign(packet) then
    begin
      // GUI-Updates müssen im Haupt-Thread erfolgen
      Synchronize(
        procedure
        begin
          DisplayPacketInfo(packet);
        end
      );
    end;
  end;
end;

// Diese Methode zeigt die empfangenen Paketinformationen in der Memo-Komponente an
procedure TReadAX25PacketsThread.DisplayPacketInfo(const packet: TAX25Packet);
begin
  FMemo.Lines.Add('Paket empfangen von: ' + AX25CallsignToString(packet.source));
  FMemo.Lines.Add('Paketinhalt: ' + PChar(@packet.info[0]));
end;

end.

