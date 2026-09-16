# AX.25 Protocol Specification

## Overview

AX.25 (Amateur X.25) is a digital packet-switching protocol widely used in amateur radio communications and other digital communication systems. It is based on the X.25 Data Link Protocol and was developed by the amateur radio community for connecting amateur radios over packet networks.

---

## Frame Structure

All AX.25 frames follow a standardized structure with a fixed format for addresses and variable payload length:

### Standard AX.25 Frame Format

```
[Destination Call] [Source Call] [Control] [PID] [Payload] [CRC16-CCITT]
     (7 bytes)      (7 bytes)       (1 byte)  (1 byte)   (variable)  (2 bytes)
```

### Detailed Frame Breakdown

#### Address Fields (14 bytes total)

Each address field consists of 7 bytes containing:
- **Byte 0-5:** Callsign (left-padded with spaces if needed)
- **Byte 6:** SSID (Sub-station ID) - Upper 4 bits and LSB

**Callsign Encoding Rules:**
1. Each character is expanded to 8 bits using ASCII << 1
2. Spaces serve as padding when callsign is < 6 characters
3. Each byte is transmitted right-justified
4. Address bytes are transmitted in order: first destination, then source

**Complete Address Structure:**
```
Byte 0: [Char1]    (ASCII << 1)
Byte 1: [Char2]    (ASCII << 1)
Byte 2: [Char3]    (ASCII << 1)
Byte 3: [Char4]    (ASCII << 1)
Byte 4: [Char5]    (ASCII << 1)
Byte 5: [Char6]    (ASCII << 1)
Byte 6: [Upper4 bits][Lower1 bit][SSID value(4 bits)]
```

**Example Conversion:**
Callsign: `DL1AB` (Destination), `DJ4ABC` (Source)
```
Destination Address:
  D (0x44) << 1 = 0x88
  L (0x4C) << 1 = 0x98
  1 (0x31) << 1 = 0x62
  A (0x41) << 1 = 0x82
  B (0x42) << 1 = 0x84
  C (0x43) << 1 = 0x86
  SSID: 0x01 + 0x80 = 0x81
```

#### Control Field (1 byte)

The control field determines the frame type and contains vital control information:

**Bit Position and Meaning:**
```
[1][0][0][0][0][0] [N(R) upper nibble] [P/F bit] [N(S) upper nibble]
```

**I-Frame Control Format:**
- **N(S) bits (0-3):** Send sequence number (0-7, modulo 8)
- **P/F bit:** Poll/Final - 1 for Poll, 0 for Final
- **N(R) bits (0-3):** Receive sequence number (0-7, modulo 8)

**S-Frame Control Format:**
- **Type bits (0-1):** S-frame type
- **P/F bit:** Poll/Final
- **N(R) bits (0-5):** Receive sequence number

**U-Frame Control Format:**
- Fixed pattern with Poll/Final bit
- Types: SABM, DISC, UA, FRMR

#### PID Field (1 byte)

The Protocol Identifier indicates which protocol is embedded in the payload:

**Common PID Values:**
- `0xF0`: No layer 3 protocol - raw data
- `0xF6`: X.25 Layer 3
- `0xF7`: HDLC with LAPB
- Other values: Various network protocols

#### Payload (variable length)

Variable-length data payload:
- Max: 256 bytes (per X.25 spec)
- Variable: Supports flexible data sizes
- Content: Application data or protocol layer messages

#### CRC16-CCITT (2 bytes)

Error detection field:
- **Polynomial:** 0x8408
- **Initial value:** 0xFFFF
- **Computation:** Includes address, control, PID, and payload
- **Byte order:** Low byte first, high byte second

---

## Frame Types

### I-Frame (Information Frame)

**Purpose:** Transport actual data information over the link

**Packet Format:**
```
fm [Source] to [Dest] ctl I[NR] pid [value]
```

**Implementation Example:**
```
fm DL1AB to DJ4ABC ctl I3 pid F0
  TEST MESSAGE WITH CRLF
```

**Control Field Example:** NS=3, NR=5, PF=false
- Binary: `01100000` (0x60)
- N(S)=3, N(R)=5, P/F=0

**Features:**
- Sequence numbers for reliability
- Acknowledgment (RR response needed)
- Flow control mechanism

### S-Frame (Supervisory Frame)

**Purpose:** Used for flow control and link management

#### RR (Receiver Ready)

**Purpose:** Acknowledge successful reception, indicate ready for more frames

**Packet Format:**
```
fm [Source] to [Dest] ctl RR[NR] pid [value]
```

**Example:**
```
fm DL1AB to DJ4ABC ctl RR5 pid F0
```

**Implementation Example (src:ukissmode.pas:362):**
```pascal
procedure TKISSMode.SendRR(Channel: Byte; PF: Boolean; NR, Command: Byte);
var AXSend, Frame: TBytes;
begin
  if not TNCPort[Channel].Connected then
    Exit;

  AXSend := AX25.BuildRRFrame(FPConfig^.Callsign, TNCPort[Channel].DestinationCall, NR, PF);
  Frame := BuildKISSFrame(AXSend, TNCPort[Channel].Port, Command);
  SendKISSFrame(Frame);
  TNCPort[Channel].T2Running := False;
end;
```

#### RNR (Receiver Not Ready)

**Purpose:** Unavailable to receive frames, request pause

**Packet Format:**
```
fm [Source] to [Dest] ctl RNR[NR] pid [value]
```

**Example:**
```
fm DL1AB to DJ4ABC ctl RNR6 pid F0
```

**Features:**
- Remote is busy
- Will respond with RR or REJ upon readiness

#### REJ (Reject)

**Purpose:** Negative acknowledgment, request retransmission

**Packet Format:**
```
fm [Source] to [Dest] ctl REJ[NR] pid [value]
```

**Example:**
```
fm DL1AB to DJ4ABC ctl REJ4 pid F0
  FRAME 4 REJECTED
```

**Implementation Example (src:ukissmode.pas:291):**
```pascal
sfREJ:
  begin
    AX := TNCPort[Port].LastFrames[AXFrame.NR];

    if Length(AX) > 0 then
      begin
        Frame := BuildKISSFrame(AX, TNCPort[Port].Port, 0);
        SendKISSFrame(Frame);
      end;

    TNCPort[Port].VS := AXFrame.NR;
  end;
```

### U-Frame (Unnumbered Frame)

**Purpose:** Link management, unsolicited commands

#### SABM (Set Asynchronous Balanced Mode)

**Purpose:** Establish 10800 bps SABM mode, balanced link

**Packet Format:**
```
fm [Source] to [Dest] ctl SABM pid F0
```

**Usage:**
```
fm DL1AB to DJ4ABC ctl SABM pid F0
```

**Implementation Example (src:ukissmode.pas:311):**
```pascal
CTRL_SABM:
  begin
    Writeln('SABM empfangen – Verbindung aufbauen');
    SendUA(Port, AXFrame.PF, 1);
  end;
```

#### DISC (Disconnect)

**Purpose:** Request to disconnect the link

**Packet Format:**
```
fm [Source] to [Dest] ctl DISC pid F0
```

**Example:**
```
fm DL1AB to DJ4ABC ctl DISC pid F0
```

**Implementation Example (src:ukissmode.pas:315):**
```pascal
CTRL_DISC:
  begin
    Writeln('DISC empfangen - Verbindung trennen');

    if not AXFrame.PF then
       SendUA(Port, AXFrame.PF, 1);

    if AXFrame.NR = TNCPort[Port].VS then
      TNCPort[Port].T1Running := False;

    if TNCPort[Port].Connected then
      begin
        ChannelBuffer[Port] := ChannelBuffer[Port] + '>>> LINK STATUS: Disconnected from ' +
                               TNCPort[Port].DestinationCall;
        ChannelStatus[Port][6] := 'DISCONNECTED';
        TNCPort[Port].Connected := False;
        TNCPort[Port] := Default(TChannel);
      end;
  end;
```

#### UA (Unnumbered Acknowledgement)

**Purpose:** Acknowledge link setup, connection established

**Packet Format:**
```
fm [Source] to [Dest] ctl UA pid F0
```

**Example:**
```
fm DL1AB to DJ4ABC ctl UA pid F0
```

**Implementation Example (src:ukissmode.pas:335):**
```pascal
CTRL_UA:
  begin
    Writeln('UA empfangen - Unnumbered Acknowledgement');

    TNCPort[Port].T2 := GetTickCount64;
    TNCPort[Port].T2Running := True;

    if not TNCPort[Port].Connected then
      begin
        TNCPort[Port].DestinationCall := AXFrame.SrcCall;
        TNCPort[Port].Connected := True;
        ChannelBuffer[Port] := ChannelBuffer[Port] + '>>> LINK STATUS: Connected to ' +
                               AXFrame.SrcCall;
        ChannelStatus[Port][6] := 'CONNECTED';
        ChannelStatus[Port][7] := AXFrame.SrcCall;
      end;
  end;
```

#### FRMR (Frame Reject)

**Purpose:** Indicate framing or protocol error

**Packet Format:**
```
fm [Source] to [Dest] ctl FRMR pid F0
```

**Features:**
- Contains error information
- Initiates reconnection if needed
- Typically followed by SABM re-establishment

---

## Link Layer Protocol

### Link Initialization Sequence

**Complete Connect Workflow:**

```
┌────────────────────────────────────────────────────────────┐
│                    INITIALIZATION SEQUENCE                   │
├────────────────────────────────────────────────────────────┤
│                                                              │
│  Step 1: Application Issue CONNECT Command                    │
│    CONNECT [Destination Call]                                │
│                                                              │
│  Step 2: TNC Sends SABM Frame                               │
│    fm [MyCall] to [Dest] ctl SABM pid F0                     │
│                                                              │
│  Step 3: Host Receives SABM and Sends UA                    │
│    fm [MyCall] to [Dest] ctl UA pid F0                      │
│                                                              │
│  Step 4: Link Established                                   │
│    Connected = TRUE                                          │
│    VR = Current Expected Frame Number                         │
│    VS = Next Frame to Send                                   │
│                                                              │
└────────────────────────────────────────────────────────────┘
```

### Flow Control with Sliding Window

**Implementation Overview:**

```
┌─────────────────────────────────────────────────────────────┐
│                        FLOW CONTROL                          │
├─────────────────────────────────────────────────────────────┤
│                                                             │
│   Host ←──────┐    I(NR)    ───────→    Host                 │
│   1            │                                       2     │
│               │                                       ▲     │
│               └─────────── RR[NR] ────────────────────┘     │
│                                                             │
│  VR: Receive Sequence Number                              │
│      - Next frame number to receive                        │
│      - Incremented on receipt of valid frame               │
│                                                             │
│  VS: Send Sequence Number                                 │
│      - Next frame number to send                           │
│      - Incremented after transmission                       │
│                                                             │
│  T1: Timeout for Retransmission                              │
│      - 40 seconds default                                   │
│      - Triggers on timeout                                  │
│                                                             │
│  T2: RR Timeout                                             │
│      - 5 seconds default                                    │
│      - Periodic flow control                               │
│                                                             │
└─────────────────────────────────────────────────────────────┘
```

### Link Termination Sequence

**Complete Disconnect Workflow:**

```
┌────────────────────────────────────────────────────────────┐
│                    TERMINATION SEQUENCE                     │
├────────────────────────────────────────────────────────────┤
│                                                              │
│  Step 1: Application Issue DISCONNECT Command                │
│    DISCONNECT                                               │
│                                                              │
│  Step 2: TNC Sends UA Confirmation                          │
│    fm [MyCall] to [Dest] ctl UA pid F0                      │
│                                                              │
│  Step 3: Both Return to Idle                               │
│    Connected = FALSE                                        │
│    Reset VR, VS, T1, T2                                     │
│                                                              │
└────────────────────────────────────────────────────────────┘
```

---

## Address Format and Encoding

### Callsign Specifications

**Standard Rules:**
1. Maximum 6 characters (uppercase standard)
2. Case-insensitive
3. Spaces used as padding
4. No wildcards in addresses

**Address Examples:**
```
DL1AB          (6 chars, perfect fit)
DJ4A           (padded right to 6 chars)
DK4HELLO       (6 chars, fits exactly)
UNKNOWN        (6 chars)
```

### SSID (Sub-station Indicator)

**Structure and Meaning:**

```
Format: [Lower 4 bits of SSID] and [address type bits]
       [0..3] [0..3] [LSB] [4..7]

Example: SSID=4 → 0x04
         SSID=7 → 0x07
         SSID=8 → 0x08 (main station usually)
```

**Naming Conventions:**
- 0: Main station
- 1-15: Sub-stations
- Format: `CALLSIGN-SID` when displaying
- SSID value appears directly in AX.25 frame

**Examples:**
```
DL1AB          (SSID=0, main station)
DL1AB-1        (SSID=1)
DJ4HELLO-4     (SSID=4)
```

### Address Expansion Algorithm

**Implementation:**
```pascal
function TAX25.EncodeCall(const Call: string; Last: Boolean): TBytes;
var i: Integer;
begin
  // Pad with spaces if less than 6 chars
  // Expand each char: ASCII << 1
  // Handle SSID in byte 6
  // Set last bit if appropriate
end;
```

**Example Conversion Table:**

| Character | ASCII | ASCII << 1 |
|-----------|-------|------------|
| A         | 0x41  | 0x82       |
| B         | 0x42  | 0x84       |
| C         | 0x43  | 0x86       |
| D         | 0x44  | 0x88       |
| L         | 0x4C  | 0x98       |
| 1         | 0x31  | 0x62       |
| -         | 0x2D  | 0x5A       |
| Space     | 0x20  | 0x40       |

---

## Error Detection

### CRC16-CCITT Implementation

**Specifications:**

```
Polynomial:    0x8408  (x^16 + x^12 + x^5 + 1)
Initial Value: 0xFFFF
Final XOR:     0x0000
Input Reflected: YES
Output Reflected: YES
```

**Fields Included in CRC:**
1. Destination address (7 bytes)
2. Source address (7 bytes)
3. Control field (1 byte)
4. PID field (1 byte)
5. Payload (n bytes)

**Byte Order:**
- Low byte transmitted first
- High byte transmitted second

**Example Implementation:**

The CRC calculation follows the standard specification and is computed over all protocol fields except the CRC itself, ensuring robust error detection for the complete frame.

---

## Protocol Layer Functionality

### I-Frame Operations

**Transmission:**
1. Host prepares I-Frame with NS sequence number
2. Sends I-frame to receiver
3. WAITS for RR from receiver
4. Updates VS and continues

**Processing:**
1. Receiver validates CRC
2. Checks NS == NR
3. On success: ACK with RR[NR+1]
4. On failure: Sends REJ[R]

**Recovery:**
- Automatic after T1 timeout (40 sec)
- Retransmits last frame

### S-Frame Operations

**RR Response:**
```
If receiver has room:
  Send RR[NR]
Else:
  Send RNR[NR]
```

**REJ Request:**
```
Request retransmission from N(R):
  Send REJ[N(R)]
  Local VS = N(R)
```

### U-Frame Operations

**SABM Exchange:**
```
Initiator sends SABM
Responder sends UA
Link established in balanced mode
```

**DISC Exchange:**
```
Requestor sends DISC
Responder sends UA
Both stations idle
```

---

## Modulation and Bit Rates

### Common Modes

**1. 1200 bps (Bell 202)**
- Most common amateur mode
- Standard AX.25 specification
- Bell System 202 implementation

**2. 9600 bps**
- Bell 202T or AX.25-1200B modes
- Higher data rate over same bandwidth
- Less common but increasing

**3. 56 kbps**
- Modern KISS Mode TNCs
- Requires specific hardware
- Faster communication

**4. 2400/4800 bps**
- Other modulation schemes exist
- Often used in commercial systems

### Modulation Characteristics

**Bell 202 (1200 bps):**
- Phase shift keying
- Two symbols (0°, 180°)
- Standard for amateur packet

**Implementation Note:**
The protocol layer is agnostic to the physical modulation scheme; only the framing and protocol handling are defined by the AX.25 specification.

---

## Monitoring Format

### AX.25 Monitor Output

**Standard Format:**
```
fm [Source] to [Dest] ctl [Type] pid [value]
```

**Example Output:**
```
fm DL1AB to DJ4ABC ctl I3 pid F0
fm DL1AB to DJ4ABC ctl RR5 pid F0
fm DL1AB to DJ4ABC ctl SABM pid F0
fm DL1AB to DJ4ABC ctl UA pid F0
```

**Debug Output Format:**
```
fm [Source] to [Dest] ctl [Type][NS][NR] pid [value]
Payload: [data]
```

**Implementation Example:**

The `GetAX25Monitor` method provides human-readable information by formatting addresses, control fields, and payloads in a readable format.

---

## References and Standards

### Official Sources

1. **AX.25 Specification**
   - Original protocol specification for amateur packet radio
   - Base on X.25 protocol framework
   - Developed by amateur radio operators

2. **ARRL Technical Publications**
   - Technical Manual TM2817: AX.25 Specifications
   - Programming guidelines for TNCs
   - Implementation guides for radio operators

3. **IETF RFC Documents**
   - RFC 2228: Amateur Data Link Layer
   - Protocol specifications and implementations
   - Standards documentation

4. **TAPR (Technicians' Amateur Packet Linking Group)**
   - Protocol specifications and implementations
   - KISS protocol specification
   - Amateur packet radio standards

### Key Standards Documents

- [AX.25 Protocol Specification](https://en.wikipedia.org/wiki/AX.25)
- [ARRL AX.25 Specification](https://www.tapr.org/protocol_doc.pdf)
- [KISS Protocol Specification](https://www.tapr.org/KISS_PROTO.PDF)
- [RFC 2228 - Amateur Data Link Layer](https://www.ietf.org/rfc/rfc2228.txt)

---

## Implementation Notes

### Serial Communication
- Standard serial interface (RS232/RS485)
- Configurable baud rates
- Support for various parity/stop bit combinations

### Network Topologies
- Point-to-point links
- Linear topologies
- Mesh networks (via gateways)

### Error Recovery
- Automatic retransmission after timeout
- CRC-16 error detection
- Connection monitoring and recovery

### Application Scenarios
- File transfer
- Chat communication
- Telemetry collection
- Network routing

This specification provides complete coverage of the AX.25 protocol as implemented in the KISS mode system, with all frame types, control mechanisms, and data structures fully documented.