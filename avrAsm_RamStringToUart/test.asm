;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;MAKEFILE EXCERPT
;
;#Output file in intel hex format with -fI
;
;serial:
;	sudo putty /dev/ttyUSB0 -serial -sercfg 8,2,9600,n,N # "-cs ISO-8859-1" not necessary
;
;compile:
;	wine avrasm2.exe -fI -l test.lst test.asm
;
;flash: compile
;	avrdude -c usbtiny -p atmega1284 -U flash:w:test.hex
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INCLUDES
.nolist
.include "./m1284def.asm"
.list


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MACROS

#define compare16(bigRegLeft,bigRegRight) \
  cp  bigRegLeft##L,bigRegRight##L        \
  cpc bigRegLeft##H,bigRegRight##H

;We need to be able to increment these compound registers (X=r27:r26;Y=r29:r28;Z=r31:r30)
;and to my knowledge 'inc' does not work on them, only on single byte registers
#define inc16(reg16) \
  push r0            \
  ld r0,Z+           \
  pop r0

;Old implementation - UNUSED
;break up the task into 2 parts, increment the low byte, 
;then add 0 with any resulting carry to the high byte,
;#define inc16(reg16)  \
;  push r12            \
;  clr r12             \
;  inc reg16##L        \
;  adc reg16##H,r12    \
;  pop r12              

;Set big compound register to address in data/ram/data-segment/memory(as opposed to 'program')
#define set16(reg16,addr)   \
  ldi reg16##H,high((addr)) \
  ldi reg16##L,low((addr))   

;Actually storing anything in flash at a word address should
;first result in a lowbyte being populated, if high bytes are not supplied
;with data, then they are 'padded' as a single null byte (0x00)
;Scale by 2*X+1 because flash is byte addressable
#define setFlash16(reg16,addr)   \
  ldi reg16##H,high(2*(addr))    \
  ldi reg16##L,low(2*(addr) + 0)  

#define immediate(operation,destination,value) \
  push r19                                     \
  ldi r19, value                               \
  operation destination,r19                    \
  pop r19 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DATA
.dseg

.org RAMEND/2
  word_buffer: 
    .byte 32
  word_buffer_end:
    .byte 1; this line exists just to see address in lst file as 32 bytes later than word_buffer:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFS
.def outChar = r16

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CODE

.cseg ;Code (flash) segment

;VECTORS

  ; Reset Vector, just go to init
  .org 0x0000 
  rjmp Reset

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INIT
Reset:

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uart_init

  ;Port initialization
  immediate(out,DDRB,0xff) ; should be necessary but is apparently not, also makes leds too bright
  immediate(out,PortB,0x36)
  immediate(out,DDRA,0xff) ; should be necessary but is apparently not, also makes leds too bright
  immediate(out,PortA,0x11)

  ;I'm going to let X point to the 'write' position
  ;and let Y point to the 'read' position of the buffer
  ;for now I'm not implementing buffer wrapping the way I should
  set16(X,word_buffer)

  immediate(st, X+,'\r')
  immediate(st, X+,'\n')

  immediate(st, X+,'t')
  immediate(st, X+,'e')
  immediate(st, X+,'d')
  immediate(st, X+,' ')
  immediate(st, X+,'d')
  immediate(st, X+,'o')
  immediate(st, X+,'e')
  immediate(st, X+,'s')
  immediate(st, X+,' ')
  immediate(st, X+,'a')
  immediate(st, X+,'s')
  immediate(st, X+,'s')
  immediate(st, X+,'e')
  immediate(st, X+,'m')
  immediate(st, X+,'b')
  immediate(st, X+,'l')
  immediate(st, X+,'y')
  immediate(st, X+,'!')

  immediate(st, X+,'\r')
  immediate(st, X+,'\n')

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PRINT TEST_MESSAGE 

  ;Initialize compound register Z (=R31:R30)to point to "TEST_MESSAGE" location
  set16(Z,word_buffer)
  call printString

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MAIN
Main:
  ;do nothing, respond to events only
  rjmp Main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SUBROUTINES

printString:
  ;Loop through each byte location and print out the character found to the uart connection
  printChar:
    ;load the current char for uart transmission
    ld outChar,Z 
    ;Do not necessarily rely on null to quit printing string, but if exists because assembler inserts it, then treat it as a terminating char
    cpi outChar,0x00
    breq exitPrintString    
    ;Print the current character
    call uart_transmit
    ;Increment the 16 bit "pointer"
    inc16(Z)
    ;Check if we have reached the end of the "string"
    set16(Y,word_buffer_end)
    compare16(Y,Z)
    ;compare passed so stop printing
    breq exitPrintString
    ;If not done printing, print the next char
    jmp printChar
  exitPrintString:
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;UART STUFF

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
  immediate(sts, UBRR0H, ubrr >> 8) ;High byte
  immediate(sts, UBRR0L, ubrr )

  ; Enable receiver and transmitter ;Enable the rx complete interrupt
  immediate(sts, UCSR0B, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0))

  ; Set frame format: 8data, 2stop bit
  immediate(sts, UCSR0C, (1<<USBS0) |(1<<UCSZ01)|(1<<UCSZ00))

  ret


uart_transmit:
  ;USAGE:
  ;  mov outChar, rXX
  ;  call uart_transmit
  push r17
  UART_Transmit_Check:
    ; Wait for empty transmit buffer
    lds r17, UCSR0A
    sbrs r17, UDRE0 ; SBRS = SkipifBitinRegisterSet
    ; UDRE0 not set, data not ready, check again
    rjmp UART_Transmit_Check
  ; Put data (r16) into buffer, sends the data
  sts UDR0,outChar
  pop r17
  ret

;END EXAMPLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





