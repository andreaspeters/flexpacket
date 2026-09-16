# AutoBin-Protokollvertrag

## Wire format

The sender transmits an ASCII control header followed by CR:

```text
#BIN#<file-size>#|<crc>#$<timestamp>#<filename>\r
```

The receiver answers:

```text
#OK#\r
```

After acceptance, the sender transmits the file bytes without an application-level CR between blocks. The sender must not transmit payload before `#OK#`.

An aborted transfer uses:

```text
#ABORT#\r
```

The reference AutoBin state machine finishes with `BIN-TX OK #<crc>` and transitions to its terminal state.

## Reference state sequence

```text
WAIT_FOR_FREE_TX
  -> SEND_HEADER
  -> WAIT_FOR_OK
  -> SEND_DATA / SEND_DATA_STEPS
  -> SEND_END
  -> DONE
```

`SEND_DATA_STEPS` is entered only when the transport TX buffer is available. The reference implementation passes data into an internal connection buffer; the AX.25 layer controls packetization, outstanding I-frames, ACK/RR, RNR, REJ and retransmission.

## `/data` boundary

- `src/uautobin.pas` creates and parses the AutoBin header.
- `src/umain.pas` handles `#BIN#`, `#OK#` and `#ABORT#`.
- `src/uhostmode.pas` and `src/ukissmode.pas` encode transport frames.
- `src/ufileupload.pas` verifies received size and CRC.

The `/data` application must therefore keep AutoBin control parsing separate from binary payload buffering and must not confuse serial write readiness with AX.25 TX-window readiness.

## Compatibility risks

1. The reference `get_crc()` uses CRC-16-CCITT with an initial value of zero. `/data` must use the same value before claiming wire compatibility.
2. The reference queues data in the AX.25 connection. Direct repeated calls to hostmode/KISS send functions do not provide equivalent ACK/window/retry semantics.
3. `REJ`, `RNR` and abort states must stop the AutoBin sender in `/data`.
4. The end marker and final CRC result must be implemented and tested against the reference implementation and a real TNC.
