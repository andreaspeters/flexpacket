# Changelog

## master

## v0.6.0

- FIX: NO CMD Output
- CHANGE: Since the new Terminal Box does not support Copy 'n Paste, I
          had to change the BayCom password usage. Please take a look into then
          Readme file.
- ADD: Autostore Mails for later reading (Details under How To User).
- CHANGE: The BayCom password will be sent automatically after pressing the button.
- CHANGE: QuickConnect will select the next disconnected channel.
- FIX: Download mails.
- ADD: Basic Support for LinBPQ BBS to store Mails and list them. (untested)
- FIX: [tray] Exit Button
- FIX: listmail does not show subject of private mails.
- FIX: QuickConnect choose next free channel.
- ADD: TNC Status Message into StatusBar.
- CHANGE: Bring more stability into TNC Init process.
- ADD: Copy'n Paste content of the terminal window.
- ADD: Save mail reader window size.


## v0.5.0

- ADD: Configuration for FlexPacket Forms (fp-forms).
- CHANGE: Application Icons.
- CHANGE: Replace Memo with CmdBox for have a better ANSI
          Support.

## v0.4.0

- ADD: TFKISS Support. More Details in the README.
- FIX: create default config if no config file exist.
- FIX: tnc parity error.
- FIX: Select TFKISS Socket file Dialog.
- CHANGE: Save config befor restart Software.
- FIX: Some ansi collor issues.
- ADD: Shortcut to close the addressbook (ESC).
- FIX: Window height has increased automatically.
- FIX: AGW missing Monitor and APRS Data.
- FIX: CRC for FileUpload/Download.

## v0.3.0

- ADD: Restart menu item to restart the application.
- ADD: Shortcuts! More details in the README.
- FIX: Save configuration also during Software Error.
- ADD: Restore the current window size after restart.
- ADD: Terminal and Command Window are resizable.


## v0.2.1

- FIX: Connect string with UZ7HO Soundmodem.

## v0.2.0

- FIX: Quickconnect in AGW mode.
- FIX: Exception during close in agw mode.
- CHANGE: Change into Command Mode from every where not only from the 
  CMF (Command Memo Field).
- ADD: Delete Item from Addressbook.  

## v0.1.1

- CHANGE: check tnc/agw state before send command.
- CHANGE: improve thread handling.
- FIX-WINDOWS: named pipes to send data to APRSMap.
