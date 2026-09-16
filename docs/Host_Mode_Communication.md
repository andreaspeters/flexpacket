# Host Mode Communication Specification

## Overview

Host Mode is a communication protocol used in advanced amateur radio systems for communication between a host device (computer, smartphone, or microcontroller) and a TNC (Terminal Node Controller) or other radio equipment. The system can operate both in KISS Mode and Host Mode for different use cases.

## Host Mode Architecture

```
┌─────────────┐         ┌─────────────┐
│ Host Device │◄────────┤   TNC/      │
│  (Client)   │  Serial/│  Radio     │
│             │  Bus    │ Equipment  │
│             │         │            │
└─────────────┘         └─────────────┘
```

## Implementation Structure

### Class Hierarchy

```
THostmode (Base Class)
    ↓
TKISSMode
    ↓
[Specific Implementation]
```

### Base Class: THostmode

Abstract parent class providing fundamental communication capabilities.

**Key Properties:**
- Socket/Serial connection handling
- Network configuration management
- Base communication methods

### Derived Class: TKISSMode

Implements KISS protocol on top of physical layer.

**Key Features:**
- KISS frame encapsulation/decapsulation
- Bluetooth RFCOMM support
- AX.25 frame processing
- Port/channel management

---

## Communication Protocol

### Physical Layer Options

1. **Serial Port (UART)**
   - RS232/RS485 hardware interface
   - Baud rates: 9600, 19200, 38400, 57600, 115200
   - Parity/Stop bits configurable

2. **Bluetooth RFCOMM**
   - Wireless serial connection
   - Bluetooth address and channel configuration
   - Auto-reconnect on socket loss

3. **TCP/IP Socket**
   - Network-based communication
   - Supports remote TNC access

---

## Host Mode Data Flow

### Transmission (Host → TNC)

```
Application Data
       ↓
[Encode AX.25 Frame]
       ↓
[KISS Encapsulation]
       ↓
[Escape Special Characters]
       ↓
[Add KISS FEND]
       ↓
[Send via Serial/Bluetooth]
```

### Reception (TNC → Host)

```
[Receive Raw Bytes]
       ↓
[FIND KISS Escape Sequences]
       ↓
[Unescape Special Characters]
       ↓
[Extract KISS Header]
       ↓
[Remove KISS FEND]
       ↓
[Parse AX.25 Frame]
       ↓
[Extract Payload]
       ↓
[Deliver to Application]
```

---

## TX FIFO (Transmit FIFO) System

### Design

The system uses a First-In-First-Out (FIFO) queue for transmission management:

```
┌─────────────────────────────────────────────────────────────────┐
│                     Transmission Pipeline                         │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│   [Application]    [TX FIFO]     [Output Queue]      [Serial]   │
│       ↓              ↓              ↓            ↓            │
│    Send Data      Buffer Frames   Timer-based    Bluetooth/   │
│    Send Frame     (Add Here)     Transmission   Serial Port    │
│                                                                 │
│                                                                  │
│                                                                TX   │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

### FIFO Operations

**Add Frame:**
```pascal
function TKISSMode.SendKISSFrame(const Data: TBytes): Boolean;
begin
  FTXFIFO.Add(Data);
  Result := True;
end;
```

**Process FIFO:**
```pascal
procedure TKISSMode.ProcessTXFIFO;
var Data: TBytes;
begin
  Data := FTXFIFO[0];
  FTXFIFO.Delete(0);
  SendSerialData(Data);
end;
```

**Timer-Based Processing:**
- Process FIFO every 2000ms
- Prevents flooding with individual byte writes
- Reduces serial port congestion

---

## Retransmission Mechanism

### T1 Timeout (Send Retransmission)

**Purpose:** Ensure reliable transmission without ACK

**Algorithm:**
```pascal
T1Running := True
T1 := CurrentTime

if T1Timeout (40 seconds) and T1Running then
  begin
    // Retransmit last frame
    SendFrame(TNCPort[Channel].Last);
    T1 := CurrentTime;
  end
```

**Implementation:**
- Triggers after 40 seconds without ACK
- Retransmits the last transmitted I-Frame
- Resets timer

### T2 Timeout (RR Response)

**Purpose:** Periodic flow control and keep-alive

**Algorithm:**
```pascal
T2Running := True
T2 := CurrentTime

if T2Timeout (5 seconds) and T2Running then
  begin
    // Send RR with current VR
    SendRR(Channel, False, TNCPort[Channel].VR, 0);
  end
```

---

## Connection Management

### Connection States

```
DISCONNECTED → CONNECTING → CONNECTED → DISCONNECTED
   TNCPort[Port].T1Running = True
```

### Connection Establishment

1. **Send SABM Frame**
   ```
   Command: CONNECT [Destination]
   Action:   Send SABM(SET ASYNC BALANCED MODE)
   ```

2. **Receive UA**
   ```
   Response: UA from destination
   State:     Connection established
   ```

3. **Link Ready for Data**
   ```
   State:      Channel connected
   VR:         Receive sequence = Next expected
   VS:         Send sequence = Next to send
   Buffer:     Ready for data
   ```

### Connection Termination

1. **Send DISC Frame**
   ```
   Command: DISCONNECT
   Action:   Send DISC request
   ```

2. **Receive UA**
   ```
   Response: UA with disconnect confirmation
   State:     Link reset
   ```

3. **Return to Idle**
   ```
   TNCPort[Port].Connected := False
   ```

---

## Channel State Management

### TNCPort Structure

```pascal
TChannel = Record
  Connected: Boolean;
  DestinationCall: String;
  T1: Cardinal;              // Timeout for retransmission
  T2: Cardinal;              // Timeout for RR response
  T1Running: Boolean;        // Retransmission timer active
  T2Running: Boolean;        // RR timer active
  LastFrames: array[0..7] of TBytes;  // TX retransmission buffer
  Last: TBytes;                      // Last sent frame
  VS: Byte;                              // Send sequence number
  VR: Byte;                               // Receive sequence number
  RXBuffer: array[0..7] of AnsiString; // Received data buffer
  Port: Byte;                           // KISS port number
end;
```

### State Variables

**VR (Receive Sequence):**
- Next expected frame number from remote
- Incremented after successful reception

**VS (Send Sequence):**
- Next frame number to send
- Incremented after each transmission
- Used for retransmission buffer lookup

**Lagged State Management:**
```pascal
if (TNCPort[Port].VS <> AXFrame.NR) then
  begin
    TNCPort[Port].LastFrames[NS] := nil;
    TNCPort[Port].VS := (TNCPort[Port].VS + 1) mod 8;
  end;
```

---

## Command Processing

### String Command Parser

**Command Pattern:**
```
[C] [Callsign] [via [Destination]]?
```

**Examples:**

Connect command:
```
CONNECT DL1AB
```

Direct connect (without via):
```
CONNECT DL1AB
```

With destination/through:
```
CONNECT DJ4ABC via DK4HEL
```

**Processing:**
```pascal
Regex: '^(\S) (\S*)(?:\svia (\S+))?'
Case:
  C (CONNECT): if not connected → Build SABM frame
  D (DISCONNECT): if connected → Build DISC frame
  Text: Send as I-Frame payload
```

### RegEx Pattern

```pascal
Regex.Expression := '^(\S) (\S*)(?:\svia (\S+))?';
```

Breakdown:
- `^(\S)`: Starts with single word (C or D) → matched as [1]
- `(\S*)`: Rest of connection identifier → matched as [2]
- `(?:\svia (\S+)?)`: Optional "via" destination clause → matched as [3]

---

## Error Handling

### Connection Errors

**Handle Disconnect Requests:**
```pascal
if (AXFrame.Control = CTRL_DISC) and (not AXFrame.PF) then
  SendUA(Port, AXFrame.PF, 1);

if (AXFrame.NR = TNCPort[Port].VS) then
  TNCPort[Port].T1Running := False;
```

**Handle Errors:**
```pascal
on E: Exception do
  Writeln('AX25 Parse Error: ', E.Message);
```

---

## Monitor and Debugging

### Channel Status Messages

**Status Display:**
```pascal
ChannelBuffer[Port] := ChannelBuffer[Port] + '>>> LINK STATUS: ' + StatusMsg;
```

### Debug Output

**AX25 Frame Display:**
```pascal
procedure TAX25.PrintAX25Frame(const Frame: TAX25Frame);
// Outputs detailed frame information
```

**KISS Frame Display:**
```pascal
DebugAX25FromKISS(Data);
// Outputs hex dump of KISS frames
```

---

## References

- [AX.25 RFC 2228](https://tools.ietf.org/html/rfc2228)
- [KISS Protocol Specification](https://www.tapr.org/KISS_PROTO.PDF)
- [Hamlib Documentation](https://hamlib.sourceforge.io/)