;
;  Output file in intel hex format with -fI
;
;  wine avrasm2.exe -fI -l test.lst test.asm \
;    and;\
;    avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;

.nolist
.include "./m1284def.asm"
.list

.macro outi
  ;out immediate
  push r30   ;Preserve contents of register
  ldi r30,@1 ;Load immediate value into register
  out @0,r30 ;Must use out because are usually setting memory mapped port address which is in the low memory where mappings happen or whatever
  pop r30    ;Restore contents of register
.endm

.cseg ;Code (flash) segment; would be defaulted to code segment without declaration

  ;Port initialization
  ;should be necessary but is apparently not, also makes leds too bright on the eyes
  outi DDRA,0xff

  ;Output a value with both repetition (to verify the non accidental nature of the assignment)
  ;and assymetry (to verify the orientation of the bits/nibbles with respect to the port pins)
  outi PortA,0b_0101_0110 

Main:
  rjmp Main
