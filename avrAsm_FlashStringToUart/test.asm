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

.macro initialiseZregister
  ;Load from Program Memory (flash) Low-byte
  ;Use the low byte since that is where our string starts in this program
  ;Actually storing anything in flash at a word address should
  ;first result in a lowbyte being populated, if high bytes are not supplied
  ;with data, then they are 'padded' as a single null byte (0x00)
  ldi ZH,high(2*@0)
  ldi ZL,low(2*@0 + 0)
.endm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CODE

.cseg ;Code (flash) segment

;Events/Interrupt Vectors

  ; Reset Vector, just go to init
  .org 0x0000 
  rjmp Reset


;FLASH BYTE STRING 
;must come after 0x0000 (first address of flash)

  TEST_MESSAGE: .db "ted does assembly!",'\r','\n'
  TEST_MESSAGE_END:

;INIT
Reset:

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uart_init

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PRINT TEST_MESSAGE 

;Initialize compound register Z (=R31:R30)to point to "TEST_MESSAGE" location
initialiseZregister TEST_MESSAGE

;Loop through each byte location and print out the character found to the uart connection
printCharFromFlash:
  ;load the current char for uart transmission
  lpm outChar,Z 
  ;Do not necessarily rely on null to quit printing string, but if exists because assembler inserts it, then treat it as a terminating char
  cpi outChar,0x00
  breq stopPrinting    
  ;Print the current character
  call uart_transmit
  ;Increment the 16 bit "pointer"
  call incZ
  ;Check if we have reached the end of the "string"
  call testIfAtEnd
  ;I should have found a better way than using a specific register and specific pair of 'signal values' (false = 0x00, true = 0xff)
  cpi r20,0xff
  ;testIfAtEnd passed so stop printing
  breq stopPrinting
  ;If not done printing, print the next char
  jmp printCharFromFlash
stopPrinting:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

/* MAIN */
Main:
  ;do nothing, respond to events only
  rjmp Main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SUBROUTINES

incZ:
  ;We need to be able to increment these compound registers (X=r27:r26;Y=r29:r28;Z=r31:r30)
  ;and to my knowledge 'inc' does not work on them, only on single byte registers
  ;so I break up the task into 2 parts, increment the low byte, 
  ;then add 0 with any resulting carry to the high byte,
  push r11
  in r11,sreg
  push r12
  clr r12    ; should be 0 now, need below
  clc        ; clear the carry bit in SREG
  inc ZL     ; should generate carry, if needed
  adc ZH,r12 ; should add 0+C, yielding effectively "inc Z"
  pop r12
  out sreg,r11
  pop r11
  ret

testIfAtEnd:
  ;Since end address is address after last string address
  ;and since address increment goes low byte, high byte, next word
  ;compare incremented address with low byte of "end" address
  ;that is compare ZL to 2N+0 not 2N+1
  cpi ZH,high(TEST_MESSAGE_END*2)
  brne notEqual
  cpi ZL,low(TEST_MESSAGE_END*2+0)
  brne notEqual
    ldi r20,0xff
  jmp equal
  notEqual:
    ldi r20,0x00
  equal:
  ret

.macro stsi
  ;store direct to "dataspace" immediate
  push r30
  ldi r30,@1
  sts @0,r30
  pop r30
.endm


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  stsi UBRR0H, ubrr >> 8 ;High byte
  stsi UBRR0L, ubrr 

  ; Enable receiver and transmitter ;Enable the rx complete interrupt
  stsi UCSR0B, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0)

  ; Set frame format: 8data, 2stop bit
  stsi UCSR0C, (1<<USBS0) |(1<<UCSZ01)|(1<<UCSZ00)

  ret


.def tmp = r17
.def outChar = r16
uart_transmit:
  ;USAGE:
  ;  mov outChar, rXX
  ;  call uart_transmit
  push tmp
  UART_Transmit_Check:
    ; Wait for empty transmit buffer
    lds tmp, UCSR0A
    sbrs tmp, UDRE0 ; SBRS = SkipifBitinRegisterSet
    ; UDRE0 not set, data not ready, check again
    rjmp UART_Transmit_Check
  ; Put data (r16) into buffer, sends the data
  sts UDR0,outChar
  pop tmp
  ret

;END EXAMPLE

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;





