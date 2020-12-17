;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;MAKE TARGETS - DONT FORGET THE TAB CHARS FOR MAKE
;
;serial:
;	sudo putty /dev/ttyUSB0 -serial -sercfg 8,2,9600,n,N # "-cs ISO-8859-1" not necessary
;
;  Output file in intel hex format with -fI
;compile:
;	wine avrasm2.exe -fI -l test.lst test.asm
;
;flash: compile
;	avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INCLUDES
.nolist
.include "./m1284def.asm"
.list

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MACROS
.macro stsi
  ;store direct to dataspace - using an immediate value
  push r30
  ldi r30,@1
  sts @0,r30
  pop r30
.endm

.macro setIoBit
  ;This is just for memory-mapped ("IO") addresses
  ;ie it uses the lds/sts instead of ld instruction
  ;
  ;USAGE:
  ;  setIoBit UCSR0B,UDRIE0
  ;
  push r21
  lds  r21,@0      ; Load the io register contents
  ori  r21,(1<<@1) ; set the bit in question
  sts  @0 ,r21     ; store the new value
  pop  r21
.endm

.macro clearIoBit
  ;This is just for memory-mapped ("IO") addresses
  ;ie it uses the lds/sts instead of ld instruction
  ;
  ;USAGE:
  ;  clearIoBit UCSR0B,UDRIE0
  ;
  push r21
  lds  r21,@0         ; Load the io register contents
  andi r21,(~(1<<@1)) ; clear the bit in question
  sts  @0 ,r21        ; store the new value
  pop  r21
.endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INTERRUPT VECTORS
.cseg ;Code (flash) segment

  .org 0x0000 ; Reset Vector, just go to init
  rjmp Reset

  .org URXC0addr ; USART0, Rx Complete
  rjmp uart_receive_complete

  .org UDRE0addr ; USART0 Data register Empty
  rjmp uart_data_register_empty

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INIT
Reset:

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uart_init

  ;Enable global interrupts
  sei

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MAIN LOOP
Main:
  rjmp Main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SUBROUTINES
.def outChar = r16
.def inChar = r19
uart_init:
  ;USAGE:
  ;  
  ;  .equ FOSC = 16000000
  ;  .equ BAUD = 9600
  ;  call uart_init
  ;

  ;Set baud rate 
  ;NOTE: ubrr is a 12 bit register so 
  ;      take the 16bit clocks/baud-period value and (<< 2^4) aka (/16)
  ;      but not sure what the -1 is for
  ;      the -1 just comes from Atmel's documentation without explanation
  .equ ubrr = (((FOSC/BAUD)/16)-1)
  stsi UBRR0H, ubrr >> 8 ;High byte
  stsi UBRR0L, ubrr 

  ; Enable receiver and transmitter ;Enable the rx complete interrupt
  stsi UCSR0B, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0)

  ; Set frame format: 8data, 2stop bit
  stsi UCSR0C, (1<<USBS0) |(1<<UCSZ01)|(1<<UCSZ00)

  ret

uart_transmit_byte:
  ;enable the output interrupt 
  ;enablement and triggering seem to be synonymous
  setIoBit UCSR0B, UDRIE0 
  ret

uart_receive_complete:

  ;Get the char
  lds inChar, UDR0
  
  ;Prep the output char
  mov outChar, inChar
  ;enable the output interrupt just for this character echo
  call uart_transmit_byte

  reti

uart_data_register_empty:

  ; transmit the data
  sts UDR0,outChar 

  ;From the datasheet: 
  ; "The ISR must either write new data to UDRn to clear UDRE, or disable the interrupt altogether"
  ; So, disable the interrupt as instructed
  clearIoBit UCSR0B, UDRIE0 

  ;If output char/byte was carriage-return, load line-feed follow-up
  cpi outChar,0x0D 
  brne skipLineFeed
    ldi outChar, 0x0A  ;<LF>
    call uart_transmit_byte
  skipLineFeed:
  ;Otherwise omit a line-feed suffix
  reti




