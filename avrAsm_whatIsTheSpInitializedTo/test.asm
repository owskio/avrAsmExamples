;
;  Output file in intel hex format with -fI
;
;  wine avrasm2.exe -fI -l test.lst test.asm and; avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;

.nolist
.include "./m1284def.asm"
.list

.macro outi
  push r30
  ldi r30,@1
  out @0,r30
  pop r30
.endm

.cseg ;Code (flash) segment
  
  ;Toggle as comment to see 0xf0ff decrement to 0x40fe
  push r12

  outi DDRA,0xff   ; Data direction register
  ;outi PortA,0x88 ; For port orientation
  in r15,SPL       ; in for low mem below 'memory mapped'; lds for 'memory mapped'; lpm for flash/prog
  out PortA,r15    ; set leds at port

  outi DDRB,0xff   ; Data direction register
  ;outi PortB,0x88 ; For port orientation
  in r15,SPH       ; in for low mem below 'memory mapped'; lds for 'memory mapped'; lpm for flash/prog
  out PortB,r15    ; set leds at port

Main:
  rjmp Main


