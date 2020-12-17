;
;  Output file in intel hex format with -fI
;
;  wine avrasm2.exe -fI -l test.lst test.asm \
;    and;\
;    avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;

.nolist
.include "./m1284def.asm"
.include "./macros.asm"
.list

.dseg ;Data (RAM) segment
  data_var: .byte 2

  ;For now limit the string size of a combinator to 32 uchars
  data_serial_input_buffer_index: .byte 2
  data_serial_input_buffer: .byte 32
  data_serial_input_buffer_max: 


.cseg ;Code (flash) segment

  ;Events/Interrupt Vectors
  .org 0x0000 ; Reset Vector, just go to init
  rjmp Reset

  .org URXC0addr ; USART0, Rx Complete
  rjmp uart_receive_complete

  .org UDRE0addr ; USART0 Data register Empty
  rjmp uart_data_register_empty

  ;Code Includes
  .nolist
  .include "uart.asm"
  .list

  test_message: .db "ted does assembly"


Reset:
  code_var: .db 0x99,0x66,0x55,0xaa ; just testing 

  ;Read from flash/program mem
  ;lpmHi r16,code_var
  ;lpmLo r16,code_var

  ;Just doing some manipulations to see how manipulating the data space goes
  ;readDataLo r16,data_var
  ldi r16,0x4c
  sts data_var,r16
  lds r1,data_var
  inc r1
  sts data_var,r1
  lds r16, data_var

  ;Port initialization
  outi DDRB,0xff;should be necessary but is apparently not, also makes leds too bright
  outi PortB,0x36
  outi DDRA,0xff;should be necessary but is apparently not, also makes leds too bright
  outi PortA,0x11
  ;outi DDRD,0x00 ;input
  ;outi PortD,0xff ;pullups

  ;lpmLo r20,code_var
  outi PortA,SPL 

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call UART_Init

  ;Enable global interrupts
  sei

ldi outChar, 0x6A
push outChar
ldi outChar, 0x6B
push outChar
ldi outChar, 0x6C
push outChar
ldi outChar, 0x6D
push outChar
ldi outChar, 0x6E
push outChar

setZto test_message
lpm outChar,Z 
call uart_transmit_byte
incZ
lpm outChar,Z 
call uart_transmit_byte
inc ZL
lpm outChar,Z 
call uart_transmit_byte

/* MAIN */
Main:

  ;Put-out some input
  ;in r20, PinD   ;Read port d
  ;out PORTC, r20 ;output to portc

  ;Print 'A' (= decimal 65)
  ;ldi outChar, 65
  ;call uart_transmit_byte
  
  ;lds outChar,test_message 
  ;call uart_transmit_byte


  lds r20,SPH
  cpi r20,high(RAMEND)
  brne skipLow
  lds r21,SPL
  cpi r21,low(RAMEND)
  breq dontPrint
skipLow:

  lds r22,UCSR0B
  andi r22, (1<<UDRIE0)
  cpi r22,0
  brne dontPrint
  ;okPrint:
    pop outChar
    call uart_transmit_byte
  dontPrint:

  rjmp Main

uart_transmit_byte:
  ;ldi outChar, 0x6F
  setIoBit UCSR0B, UDRIE0 
  ret

uart_receive_complete:

  ;Get the char
  lds inChar, UDR0
  
  ;Display the received char on some LEDs
  out PortB, inChar

  ;Prep the output char
  mov outChar, inChar
  ;enable the output interrupt just for this character echo
  call uart_transmit_byte

  ;Used to be a direct call to transmit
  ;using the interupt mechanism now to avoid polling
  ;call UART_Transmit

  reti

uart_data_register_empty:

  ; transmit the byte
  sts UDR0,outChar 

  ;From the datasheet: 
  ; "The ISR must either write new data to UDRn to clear UDRE, or disable the interrupt altogether"
  ; So, disable the interrupt as instructed
  clearIoBit UCSR0B, UDRIE0 

  ;If was carriage-return, load line-feed follow-up
  cpi outChar,0x0D 
  brne skipLineFeed
    ldi outChar, 0x0A ; <LF>
    call uart_transmit_byte
  skipLineFeed:
  reti




