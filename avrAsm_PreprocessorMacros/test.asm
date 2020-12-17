;
;  Output file in intel hex format with -fI
;
;  wine avrasm2.exe -fI -l test.lst test.asm and; avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;

.nolist
.include "./m1284def.asm"
.list

;.macro outi
;  push r30
;  ldi r30,@1
;  out @0,r30
;  pop r30
;.endm

#define outi(destination,value) \
  push r30                      \
  ldi r30,(value)               \
  out (destination),r30         \
  pop r30

.cseg ;Code (flash) segment ;This is added by default if not written down

  outi(DDRA,0xff)  ; Data direction register
  outi(PortA,0x56)    ; set leds at port

  outi(DDRB,0xff)   ; Data direction register
  outi(PortB,0x56)    ; set leds at port

Main:
  rjmp Main


