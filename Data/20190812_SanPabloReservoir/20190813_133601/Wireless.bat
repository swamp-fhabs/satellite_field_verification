@ECHO OFF
REM
REM  Windows Vista and 7
REM  Create a wireless adhoc profile with the name and ssid equal to the instrument number
REM  Change the line below to match the instrument number. 
REM
netsh wlan connect name=16113 ssid=16113 interface="Wireless Network Connection"