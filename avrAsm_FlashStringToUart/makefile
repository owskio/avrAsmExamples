
#comment
serial:
	sudo putty /dev/ttyUSB0 -serial -sercfg 8,2,9600,n,N # "-cs ISO-8859-1" not necessary

compile:
	wine avrasm2.exe -fI -l test.lst test.asm

flash: compile
	avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex

run: compile flash serial
