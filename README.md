# Flexpacket - Cross Platform Packet Radio Client.

[![](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ)a

Thanks to 

- Kelvin

for your Donation. :-) 

## Important Infos

- Hostmode (via HW TNC) is currently not working under windows. I'm still debugging this issue.

## Features

- Support for Hostmode TNC's (with TF2.x Firmware) 
- Multichannel (only in Hostmode) 
- Addressbook for quick connections and BayCom password
- 7Plus generator
- Support for the AGW Protocol 
- 7Plus FileUp/Download (only in Hostmode) 
- Common ANSI color codes (in testing)
- APRS Map as external Software.

## Planned Features

- Support forms to send simple structured information in an emergency case. 
- Support for KISS Mode

## Requirements 

- Windows Users need the [sqlite3.dll](https://www.sqlite.org/download.html).

## How to compile

- Install Lazarus 3.6
- Install TRichMemo, LazSerial

## How to use

### How to connect with a station

In the command and message field (CMF), hit the `<ESC>` key to enter the command mode.
You will get visual feedback via a red line above CMF. Then type
`c <DESTINATION_CALL>` and then hit the Enter/Return key. At the same time,
you will exit the command mode.

![image_2024-12-11-22-57-18](vx_images/image_2024-12-11-22-57-18.png)

### Baycom Password

To use the Baycom password functionality, add your baycom password into the 
Addressbook (be aware that it will be stored in playtext right now). 

If you connect to a BBS with enabled password, you will get a couple of numbers.

![image_2024-12-12-17-49-42](vx_images/image_2024-12-12-17-49-42.png)

Copy it with your mouse, open the Addressbook an hit the key button:

![image_2024-12-12-17-50-48](vx_images/image_2024-12-12-17-50-48.png)

The calculated password string will be in your clipboard for the next 10 seconds.
You can paste it in the CMF field.

### Change the Commad Memo Field (CMF) size

If you want to change the size of the CMF, grab the gab with your Mouse.
The size will be stored in the configuration file.

![image_2024-12-17-21-01-51](vx_images/image_2024-12-17-21-01-51.png)

## Shortcuts

- `ALT+0 to 9` Show Monitor or Channel 1 to 9
- `ALT+A` Open Addressbook
- `ALT+C` In the Addressbook, execute QuickCall at the current choosen Callsign
- `ALT+P` In the Addressbook, get the calculated BayCom password string
- `ALT+D` In the Addressbook, delete the current choosen Callsign


## Screenshots

![FlexPacket](vx_images/image_2024-12-08-16-22-18.png)
![Terminal Color Settings](vx_images/image_2024-12-08-16-23-26.png)
