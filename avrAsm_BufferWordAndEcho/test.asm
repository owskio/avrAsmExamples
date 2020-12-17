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

;Compare the low byte, (cp rA, rB) -> rA-rB
;This will set Z & C in SREG
;Z=1 if rA=rB, and C=1 if |rA|<|rB|; 
;Compare the high byte: cpc rA,rB -> rA-rB-C
#define compare16(bigRegLeft,bigRegRight) \
  cp  bigRegLeft##L,bigRegRight##L        \
  cpc bigRegLeft##H,bigRegRight##H

;We need to be able to increment these compound registers (X=r27:r26;Y=r29:r28;Z=r31:r30)
;and to my knowledge 'inc' does not work on them, only on single byte registers
#define inc16(reg16) \
  push r0            \
  ld r0,Z+           \
  pop r0

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
  word_buffer_begin: 
    .byte 32
  word_buffer_end:
  ; points to the first word in the dictionary
  ; so chosen because 'Aardvark' is the first *real* word in the only *real* dictionary
  aardvark: 
    .byte 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFS
.def outChar = r16
.def inChar  = r17

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CODE

.cseg ;Code (flash) segment

;VECTORS

  ; Reset Vector, just go to init
  .org 0x0000 
  rjmp Reset

  ; USART0, Rx Complete
  .org URXC0addr 
  rjmp uart_receive_complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INIT
Reset:

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uart_init

  immediate(out,DDRA,0xff)
  immediate(out,DDRB,0xff)
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PRINT TEST_MESSAGE 

  ;Initialize compound register Z (=R31:R30)to point to "TEST_MESSAGE" location
  set16(Z,word_buffer_begin)
  
  ;Reset buffer 'write' pointer to beginning of buffer now
  set16(X,word_buffer_begin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MAIN
sei ; SEt_global_Interrupt_flag
Main:
  ;do nothing, respond to events only
  rjmp Main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;SUBROUTINES


uart_receive_complete:

  ;Get the char
  lds inChar, UDR0
  
  call processUartInputCharacter
  
  reti


processUartInputCharacter:
  ;Write the input character to the current 'write' position in the RAM buffer
  ;, and then increment the 'write' pointer
  st X+,inChar

  ;If we have typed and typed until the end of the buffer
  ;without typing a word, then just print the contents
  ;of the buffer so that we can see a little more about
  ;what is going on
  set16(Z,word_buffer_end)
  compare16(X,Z)
  brlo skipInputBufferReset ; BRanch_if_LOwer_unsigned
    set16(Z,word_buffer_begin)
    call printString
    set16(X,word_buffer_begin);reset
  skipInputBufferReset:    

  ;If I type in any white-space char at all, then 'consume the word'
  ;which, for now, just means to print the word in the word buffer,
  ;which is why we add a NULL(0x00) byte to the buffer to signify 
  ;the end of the word, at least to the print-string subroutine,
  ;which terminates printing upon encountering 0x00
  cpi inchar,'\t'
  breq callConsumeWord
  cpi inchar,' '
  breq callConsumeWord
  cpi inchar,'\r'
  breq callConsumeWord
  cpi inchar,'\n'
  breq callConsumeWord
  ;Otherwise, word is still incoming (via keyboard or uart), 
  ;so skip 'consumption'
  rjmp skipWordConsumption
  callConsumeWord:
    call consumeWord
  skipWordConsumption:

  ;Echo the input character, also to make using the uart terminal connection easier
  mov outChar, inChar
  call printChar

  ret

;The beginnings of a dictionary
;I am concerned about the mere 16 bittedness of prev_def, may need to expand
;the c-preprocessors don't seem to have a .set analog so
;macro-ized dictionary entry declarations will need to be done with
;avr assembler macros
;we could also just increment the return address and jump to it, 
;instead of popping the stack, advancing the pc, and then pushing/calling
;
;Also, the assembler may pad the highest byte of an odd byted dictionary-key with 0x00, 
;so the search algorithm will need to just stop string comparing as soon as it hits a null byte, 
;and either commit to executing that entry, or move once up the dictionary chain to start comparing the 
;lookup value with the next key
;(high-byte of highest addressed word for the memory range containing the string-key/dictionary-entry-key) 
;
.set prev_def=0x0000

test_entry:
  .db prev_def
  .set prev_def=test_entry
  .db "test",0x00,0x00
    immediate(out,PortA,0xf1)
    ret

test2_entry:
  .db prev_def
  .set prev_def=test2_entry
  .db "test2",0x00,0x00
    immediate(out,PortA,0xf2)
    ret

;Now to find a word, start with prev_def and look back
set16(Z,aardvark)
immediate(st,Z,prev_def)

consumeWord:

  ;Now that the end of the word has been signalled,
  ;store a null byte in the next empty position in the 
  ;word buffer to signal the end of the word string
  immediate(st,X+,0x00)

  ;Search the dictionary for the word and if found, execute it
  set16(Z,word_buffer_begin)
  call dictionarySearch

  ;Convenience newline for printing out the word-buffer's contents
  ldi outChar,'\r'
  call printChar

  ;'Consume the word' - Print it out from RAM
  set16(Z,word_buffer_begin)
  call printString

  ;Convenience newline for printing out the word-buffer's contents
  ldi outChar,'\r'
  call printChar

  ;Reset the 'write' pointer to the beginning of the word buffer
  ;to start recording the next word being typed in
  set16(X,word_buffer_begin)

  ret

dictionarySearch:
  set16(Y,aardvark)
  ld r12,Y
  inc16(Y)
  ld r13,Y
  immediate(out,PortB,0x22)
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


printString:
  ;Loop through each byte location and print out the character found to the uart connection
  ;Don't know if I want to keep using Z like this in the future
  printStringNextChar:
    ;load the current char for uart transmission
    ld outChar,Z 
    ;Do not necessarily rely on null to quit printing string, 
    ;but if exists because assembler inserts it, 
    ;then treat it as a terminating char
    cpi outChar,0x00
    breq exitPrintString    
    ;Print the current character
    call printChar
    ;Increment the 16 bit "pointer"
    inc16(Z)
    ;Check if we have reached the end of the "string"
    set16(Y,word_buffer_end)
    compare16(Y,Z)
    ;compare passed so stop printing
    breq exitPrintString
    ;If not done printing, print the next char
    jmp printStringNextChar
  exitPrintString:
  ret

printChar:
  ;Prints a byte to uart, but also appends an <LF> to any outgoing <CR>s.
  ;(Useful when working/debugging uart at the terminal)
  ;USAGE:
  ;  mov outChar, rXX
  ;  call printChar

  ;Print the current character
  call uart_transmit_byte_sync

  ;If was carriage-return, load line-feed follow-up
  cpi outChar,0x0D    ; <CR>
  brne skipLineFeed
    ldi outChar, 0x0A ; <LF>
    call printChar
  skipLineFeed:

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


uart_transmit_byte_sync:
  ;USAGE:
  ;  mov outChar, rXX
  ;  call uart_transmit_byte_sync
  push r17
  uart_transmit_check:
    ; Wait for empty transmit buffer
    lds r17, UCSR0A
    sbrs r17, UDRE0 ; SBRS = SkipifBitinRegisterSet
    ; UDRE0 not set, data not ready, check again
    rjmp uart_transmit_check
  ; Put data (r16) into buffer; this sends the data
  sts UDR0,outChar

  pop r17
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
