# Flexpacket - Cross Platform Packet Radio Client.

[![](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/donate/?hosted_button_id=ZDB5ZSNJNK9XQ)a

## Important Infos

Hostmode (via HW TNC) is currently not working under windows. I'm still debugging this issue.

## Features

- Support for Hostmode TNC's 
- Multichannel (only in Hostmode) 
- Addressbook for quick connections and BayCom password
- 7Plus generator
- Support for the AGW Protocol 
- 7Plus FileUp/Download (only in Hostmode) 
- Common ANSI color codes (in testing)
- APRS Map as external Software.

## Planned Features

- Support forms to send simple structured information in an emergency case. 

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


## Screenshots

![FlexPacket](vx_images/image_2024-12-08-16-22-18.png)
![Terminal Color Settings](vx_images/image_2024-12-08-16-23-26.png)
