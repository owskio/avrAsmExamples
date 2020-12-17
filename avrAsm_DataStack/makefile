
default: flash

serial:
	sudo putty /dev/ttyUSB0 -serial -sercfg 8,2,9600,n,N # "-cs ISO-8859-1" not necessary

compile:
	wine avrasm2.exe -fI -l readable.lst readable.asm

flash: compile
	avrdude -c usbtiny -p m328p -U flash:w:readable.hex
#	avrdude -c usbtiny -p atmega1284 -U flash:w:readable.hex

run: compile flash serial

