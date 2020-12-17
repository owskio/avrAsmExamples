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

;.nolist is actually really useful, now when I look at the list file, I can easily see only my own code/memory-structures
.nolist
;.include "./m1284def.asm"
.include "./m328Pdef.asm"
.list

#ifdef _M328PDEF_INC_
  #define URXC0addr URXCaddr
#endif

#ifndef _M1284DEF_INC_
#endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; /* I really dislike this style of using specialty constructs             */
; /* clearly designed by atmel not to manipulate 16 bit data               */
; /* but to provide 'nice features' which only serve to lock in the vendor */
; /* eg the H/L suffix (btw only works with X/Y/Z registers)               */
; /* so note to self: try to use the macros that only use 8-bit operations */
; /* because then, it should be easier to port this                        */
;
; #define ldi16(reg16,val16)                                                \
;   loadImmediate16(reg16##H,reg16##L,high(val16),low(val16))
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DATA
.dseg ;Data (ram) segment

;I wasn't sure where to put this buffer, but thankfully the assembler can evaluate expressions like RAMEND/2
.org RAMEND/3
  word_buffer_begin:
    .byte 16
  word_buffer_end:
  play_area:

.org (2*RAMEND)/3
  parameterStack:
  parameterStackPointer:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFS
;Not sure if this is the best place for this ;declarations/initializations should always be next to the code that uses them
;.def outChar = r16
;.def inChar  = r17
.def outChar = r24
.def inChar  = r25

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CODE

.cseg ;Code (flash) segment

;VECTOR ASSIGNMENTS

  ; Reset Vector, just go to init
  .org 0x0000
  rjmp Reset

  ; USART0, Rx Complete
  .org URXC0addr
  rjmp uart_receive_complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#define immediate(operation,destination,value) \
  push r22                                     \
  ldi r22, value                               \
  operation destination,r22                    \
  pop r22

;Set big compound register to address in data/ram/data-segment/memory(as opposed to 'program')
#define loadImmediate16(regH,regL,valH,valL)   \
  ldi regH,valH \
  ldi regL,valL

;Useful for inc-ing and adding
#define addImmediate16(regH,regL,valH,valL) \
  push r20                                  \
  push r21                                  \
  loadImmediate16(r21,r20,valH,valL)        \
  add regL,r20                              \
  adc regH,r21                              \
  pop r21                                   \
  pop r20

#define printCharImmediate(ch) \
  ldi outChar,ch    \
  call printChar

;INIT

  ;ldi outChar,0x05
  ;call printHexNibble
  ;printCharImmediate('\n')

; DATA STACK

#define dataStackPointerHigh r19
#define dspH dataStackPointerHigh
#define dataStackPointerLow r18
#define dspL dataStackPointerLow

#define dataStackPush(reg)         \
  push ZH \
  push ZL \
  mov ZH,dspH\
  mov ZL,dspL\
  st Z,reg      \
  pop ZL\
  pop ZH\
  mov outChar,dspH\
  call printHexByte\
  mov outChar,dspL\
  call printHexByte\
  addImmediate16(dspH,dspL,0,1)

;#define pu(r)\
;  dataStackPush(r)


#define dataStackPop(reg)         \
  /*addImmediate16(dspH,dspL,0,-1) */\
  /*Neither subi, nor sub have entries in the docs, but sbci does, which is how i found out about them, in the sbci example*/\
  mov outChar,dspH\
  call printHexByte\
  mov outChar,dspL\
  call printHexByte\
\
  subi dspL,1 \
  sbci dspH,0 \
\
  mov outChar,dspH\
  call printHexByte\
  mov outChar,dspL\
  call printHexByte\
\
  push ZH \
  push ZL \
  mov ZH,dspH\
  mov ZL,dspL\
  ld reg,Z \
  pop ZL\
  pop ZH

;#define po(r) \
;  dataStackPop(r)
  



;This way we can avoid using the X,Y,Z expressions
;since X and XH:XL are not interchangeable expressions
#define setX(val16) loadImmediate16(XH,XL,high(val16),low(val16))
#define setY(val16) loadImmediate16(YH,YL,high(val16),low(val16))
#define setZ(val16) loadImmediate16(ZH,ZL,high(val16),low(val16))

Reset:

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uart_init

  ;Setup port directions
  immediate(out,DDRB,0xff)
  immediate(out,DDRC,0xff)

  ;Initialize the data-stack pointer
  ldi dspH,high(parameterStack)
  ldi dspL,low(parameterStack)

  mov outChar,dspH\
  call printHexByte\
  mov outChar,dspL\
  call printHexByte\
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;Initialize compound register Z (=R31:R30)to point to "TEST_MESSAGE" location
  ;setZ(word_buffer_begin)

  ;Reset buffer 'write' pointer to beginning of buffer now
  setX(word_buffer_begin)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MAIN
sei ; SEt_global_Interrupt_flag
Main:
  ;do nothing, respond to events only
  rjmp Main

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INTERRUPT ROUTINES

uart_receive_complete:

  ;Get the char
  lds inChar, UDR0
  ;And process it
  call processUartInputCharacter

  reti

;INPUT PROCESSING ROUTINES

#define compare16(regA_H,regA_L,regB_H,regB_L) \
  cp  regA_L,regB_L                            \
  cpc regA_H,regB_H


#define incX addImmediate16(XH,XL,0,1)
#define incY addImmediate16(YH,YL,0,1)
#define incZ addImmediate16(ZH,ZL,0,1)

processUartInputCharacter:
  ;TODO: process back-space delete most recent char in input buffer. down to buffer_begin that is
  ;TODO: process spaces as no-ops when the input buffer is empty
  ;      or the input buffer pointer points to the beginning of the input buffer

  ;If I type in any white-space char at all, then 'consume the word'
  ;which is why we add a NULL(0x00) byte to the buffer to signify
  ;the end of the word
  call isWhiteSpace
  brne skipWordConsumption
    ;Now that the end of the word has been signalled,
    ;store a null byte in the next empty position in the
    ;word buffer to signal the end of the word string
    immediate(st,X,0x00)
    ;Dont use that X+ trash
    incX
    ;Reset the 'write' pointer to the beginning of the word buffer
    ;to start recording the next word being typed in
    setX(word_buffer_begin)
    call consumeWord
  rjmp skipCharStorage
  skipWordConsumption:
    ;Otherwise, word is still incoming (via keyboard or uart),
    ;so skip 'consumption'
    ;Write the input character to the current 'write' position in the RAM buffer
    ;, and then increment the 'write' pointer
    st X,inChar
    ;Dont use that X+ trash
    incX
  skipCharStorage:

  ;Echo the input character, also to make using the uart terminal connection easier
  mov outChar, inChar
  call printChar

  ret


#define double16(regH,regL)                          \
  /* Mostly for flash word-2-byte addr conversion */ \
  /* times 2 lowByte, generate carry */              \
  lsl regL                                           \
  /* Rotate_Over_Left_with_carry     */              \
  /* times 2 plus prior carry bit    */              \
  rol regH


#define dereferenceZ                                        \
  /* I played around with many ways but isolating this   */ \
  /* macro to Z only due to lpm's reliance on it was the */ \
  /* simplest way to go in the end.                      */ \
  push r12                                                  \
  push r13                                                  \
  /* Load low,then high */                                  \
  lpm r12,Z                                                 \
  incZ                                                      \
  lpm r13,Z                                                 \
  mov ZL,r12                                                \
  mov ZH,r13                                                \
  pop r13                                                   \
  pop r12                      

;DICTIONARY AND DICTIONARY ROUTINES

;I am concerned about the mere 16 bittedness of prev_def, may need to expand
;
;the c-preprocessors don't seem to have a .set analog so
;macro-ized dictionary entry declarations will need to be done with
;avr assembler macros
;
;we could also just increment the return address and jump to it,
;instead of popping the stack, advancing the pc, and then pushing/calling
;-> Ahh yes, this would be 'direct threaded' code, 
;-> I am still debating this in my mind since it would technically be faster
;-> but would require additional cognitive/implementation overhead
;-> to any novice picking up this code since they will have to implement 
;-> a stack and indirection before interpreting anything
;
;Also, the assembler may pad the highest byte of an odd byted dictionary-key with 0x00,
;so if the key string is not 'aligned', we will have a null word (0x0000)
;between the dictionary key and the entry code's first instruction
;The result is a no-op first instruction for some 'words'.
;I'm ok with that.
;->Or I could build in some logic to skip the null bytes before dispatching?
;->-> Nah, that would be extra lines of code for the same effect
;

;prev_def is a variable the assembler allows us to set/read
;while the assembly process is taking place but without affecting
;the memory footprint of the program
.set prev_def=0x0000

;This expands to the flash memory location whereever this macro is called 
;it never gets used directly in the code, it just allows us to set up the 
;lookup/prototype chain by assigning it to prev_def and then expanding    
;prev_def to the current value in the next definition                     
#define fun(combinatorName)                                                                      \
  combinatorName##_label:                                                                        \
    /* Apparently the assembler considers prev_def a 1byte expression, but is 2byte ref,so  */   \
    .db low(prev_def),high(prev_def)                                                             \
    .set prev_def=combinatorName##_label                                                         \
    /* the # wraps the parameter in quotes because apparently "combinatorName" doesn't work */   \
    .db #combinatorName ,'\0'

;FYI: the above macro expands to something like this: 
;
;  myFun_label:
;    .db prev_def
;    .set prev_def=myFun_label
;    .db "myFun",'\0'
;
;Which in turn gets assembled to something like this:
;
;  0x0001 0x0000
;  0x0002 "y","m"
;  0x0003 "u","F"
;  ...
;  0x0005 0x0001
;  0x0006 "r","u"
;  0x0007 "u","F"
;  ...
;
;  myFun_label:
;    .db prev_def
;    "myFun\0"
;

#define romFun(funName) \
  fun(funName)          \
    call evalRom           \
    .db 
#define romFunRet \
  ," ",'\0','\0'       \
  ret

;AMAZING, YES!!

fun(asdf)
  immediate(out,PortC,0x33)
  ret

fun(qwer)
  immediate(out,PortC,0x55)
  ret

fun(poiu)
  immediate(out,PortC,0x99)
  ret

fun(hi)
  push ZH
  push ZL

  setZ(play_area)
  immediate(st,Z+,'\n')
  immediate(st,Z+,'H')
  immediate(st,Z+,'e')
  immediate(st,Z+,'l')
  immediate(st,Z+,'l')
  immediate(st,Z+,'o')
  immediate(st,Z+,' ')
  immediate(st,Z+,'W')
  immediate(st,Z+,'o')
  immediate(st,Z+,'r')
  immediate(st,Z+,'l')
  immediate(st,Z+,'d')
  immediate(st,Z+,'!')
  immediate(st,Z+,'\n')
  immediate(st,Z+, 0 )
  
  setZ(play_area)
  call printRamString

  pop ZL
  pop ZH
  ret

fun(one)
  ldi r20,'1'
  mov outChar,r20
  call printChar
  ret

fun(lsl)
  lsl r20
  mov outChar,r20
  call printChar
  ret

fun(add)
  ;immediate(add,r20,1)
  ;push r16                                     
  ;ldi r16, 1                               
  ;add r20,r16                    
  ;pop r16
  ;nop
  ;nop
  out PortC,r20
  ret

fun(printHexByte)
  ldi outChar,0x55
  call printHexByte
  ret

fun(quad)
  call evalRom
  .db \
  "one led lsl"\
  ," " ,'\0' ,'\0'
  ret

;fun(twice)          
  ;call evalRom      
  ;.db \
  ;," ",'\0','\0'
  ;ret

;NICE!
romFun(twice)\
  "one lsl lsl led"\
  romFunRet
  
fun(print)
  mov outChar,r20
  call printChar
  ret


nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop
nop

fun(stack)
  dataStackPush(r20)
  ret

fun(unstack)
  dataStackPop(r20)
  ret

; points to the first word in the dictionary
; so chosen because 'Aardvark' is the first *real* word in the only *real* dictionary
aardvark:
  .db low(prev_def),high(prev_def)


evalRom:
  ;I like this technique, it is very readable and compact
  ;I may expand this to a 'map' implmentation which would
  ;give me the ability to say simple things like this:
  ;
  ;  callPrintRomStringLiteral
  ;    "print me",'\n',0
  ;



  ;AVRs apparently use a 'post decrement scheme during call'
  ;I'm going to assume that means that SPH:SPL points to empty space 
  ;because it was decremented after the return address was pushed
  ;(recall SP 'grows' downward in address number, but upwards in line number when reading)
  ;It also appears after some inspection that the return-address-low-byte is pushed first
  ;and the return-address-high-byte is pushed second, 
  ;which is why the first read value goes into the 'high' register
  ;and the second addressed value goes into the 'low' register

  in ZH,SPH
  in ZL,SPL

  ;ZH:ZL should now point to the empty space where the 
  ;next return address (as the result of a call)  would go

  ;Point Z to the return address high byte
  incZ                                                      
  ;and dereference to get the return address high byte
  ld r13,Z                                                 
  ;Point Z to the return address low byte
  incZ                                                    
  ;and dereference to get the return address low byte
  ld r12,Z                                               
  
  ;We need the pointer to the string-body in Z 
  mov ZH,r13                                            
  mov ZL,r12                                           

  double16(ZH,ZL)
  ;Initially I was just printing out the string to see if I had the right place
  ;call printFlashString

  ;TODO: find out if we need an equivalent/comparable version for ram
  ;      and if we do need an 'evalRam', the break this routine into two
  ;      one for dereffing which will always be the same since SP is in IO
  ;      and the data in SP is always in the stack which is always in RAM
  ;      and then create another routine like the one below but which uses
  ;      ld instead of lpm to get the next char out of ram and not rom
  emitCharForProcessing:

      ; mov r6, ZH
      ; mov r5, ZL
      ; lsr ZH 
      ; ror ZL
      ; mov outChar,ZH
      ; call printHexByte
      ; printCharImmediate(',')
      ; mov outChar,ZL
      ; call printHexByte
      ; printCharImmediate('\n')
      ; mov ZH,r6
      ; mov ZL,r5

    lpm inChar, Z
    ;Are we looking at a null-byte terminator?
    cpi inChar,0x00
    breq skipEmitNextChar
      ;Save Z
      mov r10,ZH  
      ;  r7, and r8 did not work here wtf (even though this would have been the only use site)
      ;  r9, & r10 seem to work, and they are also non r16-r31 (needed for ldi's)
      ;  I guess you just cant trust the registers in this fucking machine
      mov r9,ZL
      ;Process the char (ruins Z's state, for now)
      call processUartInputCharacter
      ;Restore Z for the increment
      mov ZH,r10
      mov ZL,r9
      incZ

      rjmp emitCharForProcessing
  skipEmitNextChar:

  ret



;Now to find a word, start with prev_def and look back
;Storing prev_def because it is an assembler 'symbol' and
;don't know what it's value will be if used inside a
;subroutine later,so I will just use a memory address,
;which I am somewhat familiar with now

consumeWord:

  call echoInputBuffer

  ;Search the dictionary for the word and if found, execute it
  ;Q: Maybe I should have a 2*8 register self-de-reference macro? Because that's what I'm doing here
  ;A: Turns out you can't really do that since only lpm can read memory and lpm only uses Z, so you need secondary temp registers, but if you try to dereference using those then you will just get push-popped values, and there is no way to validate macros for a specific set of registers that I know of
  setZ(aardvark)
  ; word-2-byte address conversion
  double16(ZH,ZL) 
  ;Z now has byte address, not word address

  ;Z will keep track of the flash dictionary traversal location
  ;Y will keep track of the ram buffer traversal location
  setY(word_buffer_begin)

  ;;Z now points to first definition-entry in the 'dictionary'
  call dictionarySearch

  ret


echoInputBuffer:
  ;Print out the ram buffer's contents just to show the user (me) what was entered

  printCharImmediate('\n')
  printCharImmediate('"')

  ;Echo it out from RAM
  setZ(word_buffer_begin)
  call printRamString

  printCharImmediate('"')
  printCharImmediate('\n')

  ret


#define compareImmediate16(regH,regL,valH,valL) \
  push r24                                      \
  push r25                                      \
  ldi r24 ,valL                                 \
  ldi r25 ,valH                                 \
  compare16(regH,regL,r25,r24)                  \
  pop r25                                       \
  pop r24


#define halve16(regH,regL) \
  lsr regH                 \
  ror regL


#define compareZ(expression)         \
  compareImmediate16(ZH,ZL,high(expression),low(expression))


#define printCharReg(reg) \
  mov outChar,reg    \
  call printChar


dictionarySearch:
  dictionarySearchBegin:

    ;Z contains pointer to first prev_ref, 
    ;or the prev_word pointer
    ;so need to dereference
    dereferenceZ

    ;  ;Used this to debug the dictionary chain
    ;  ;apparently the .db directive only burns 1 byte,
    ;  ;even if you try to persist a 16bit reference
    ;  ;like this: .db prev_ref
    ;  mov outChar,ZH
    ;  call printHexByte
    ;  printCharImmediate(',')
    ;  mov outChar,ZL
    ;  call printHexByte
    ;  printCharImmediate('\n')

    ;word addr to byte addr 
    double16(ZH,ZL)            

    ;TODO: keep r13:r12 initialized incase the comparison fails
    ;      and we need to go to the 'previous word' to find code to execute
    mov r0,ZH
    mov r1,ZL
    ;movw r0:r1,ZH:ZL ; would work, but only for instructions like movw and adiw, and is actually kind of specific to this ass/arch

    ;skip the 'previous word' reference ; skip low byte ; skip high byte
    incZ
    incZ
    ;Z should now point to the first character in the dictionary key

    ;Restore Y to where it was before advancing it during comparison
    setY(word_buffer_begin)
    call stringComparison

    ;If the Z flag is still set, it means we got to the end of the strings and both are the same value (null)
    brne skipExecuteWord

      ;So jump to the desired word code
      ; need word address for icall, not byte address so div by 2
      halve16(ZH,ZL)
      icall ; Indirect_CALL_to_z

    rjmp skipRecurse
    skipExecuteWord:

      ;If the string comparison failed:
      ;Restore Z to the beginning of the dictionary-entry where we have the pointer to the previous dictionary-entry
      mov ZH, r0
      mov ZL, r1
      ; If the pointer to the previous definition is null then we are at the top of the dictionary and can go no further
      compareZ(0x0000)
      breq skipDereferenceAndPrintError
        ;Otherwise we are good to go to the next definition-entry,
        ;and "recurse" into what is currently called 'dictionary search' but which would be more accurately described as 'entryCheck' or so
        rjmp dictionarySearchBegin
      skipDereferenceAndPrintError:
        ;TODO: make this a macro
        ;If at end of dictionary chain, print 'Definition not found'
        message_definition_not_found:
          .db '\n',"Definition not found",'\n',0
        setZ(message_definition_not_found)
        ;word-2-byte address conversion
        double16(ZH,ZL)
        ;print it
        call printFlashString


    skipRecurse:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STRING UTILITIES

isWhiteSpace:
  ;Test if the char in 'inChar' is a whitespace character or not
  cpi inchar,'\t'
  breq yesWhite
  cpi inchar,' '
  breq yesWhite
  cpi inchar,'\r'
  breq yesWhite
  cpi inchar,'\n'
  breq yesWhite
    clz ; CLear_Z_flag
    rjmp notWhite
  yesWhite:
    sez ; SEt_Z_flag
  notWhite:
  ret

stringComparison:
  ;This is not qute a general string comparison routine,
  ;to do that I suppose I would have to read both strings into ram and then compare those
  ;but here, one string is in ram, and one is in rom
  ;(I guess until we start creating words at run time)
  ;As an aside, there should never be a need for a rom-rom string comparision 
  ;because all rom strings would ideally be known at compile time
  ;however maybe we will want to persist combinator definitions built in ram to rom at some point

  ;Compare null terminated strings pointed to by Y, and Z
  ;Return value is Z flag as usual for comparisons

  ;Still not sure how I feel about the interpreter and the interpreted sharing the data-stack
  push r19
  push r20
  stringComparisonBegin:

    ;Now we need to compare Z(rom) and Y(ram)
    ;Lets just use the Z bit as the output

    ;now put the first char of the dict key in r19
    lpm r19,Z
    ;load r20 with the first char of the word buffer
    ld r20,Y
    ;now compare the two chars
    cp r20,r19
    ;if two chars are not equal, then this dictionary key is bad, go to the next dictionary key
    brne stringComparisonEnd

      ;Otherwise continue comparing the next pair of characters
      ;if both chars are null then break but also check if both strings are empty
      cpi r19,0x00 ; both chars equal so only need to check that one is 0x00
      breq stringComparisonEnd; SREG Z bit should be unaffected by previous comparison

      ; Increment ram buffer by one byte
      incY
      ; Increment 16 bits containing flash BYTE(!) address by one (byte! not word)
      incZ

      rjmp stringComparisonBegin
      ;this would be the 'else' in the comparison above,
      ;that is we would check if either char is null since we know the chars are not equal,
      ;but we are exiting anyway since the chars are not equal
      ;so only need to check if both chars are null
  stringComparisonEnd:
  pop r20
  pop r19
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PRINTING UTILITIES

printFlashString:
  ;Loop through each byte location and print out the character found to the uart connection
  printCurrentFlashChar:
    ;load the current char for uart transmission
    lpm outChar,Z
    call printNonNullCharAndAdvance
    ;Is the char not null? If not, try the next char
    cpi outChar,0x00
    brne printCurrentFlashChar
  ret

printRamString:
  ;Loop through each byte location and print out the character found to the uart connection
  printCurrentRamChar:
    ;load the current char for uart transmission
    ld outChar,Z
    call printNonNullCharAndAdvance
    ;Is the char not null? If not, try the next char
    cpi outChar,0x00
    brne printCurrentRamChar
  ret

printNonNullCharAndAdvance:
  ;This factored-out subroutine might be pathological
  cpi outChar,0x00
  breq skipPrintCharAndAdvance
    ;Otherwise, print the current character
    call printChar
    ;and Increment the 16 bit "pointer" to the next char
    ;I could have used the Z+ 'post increment' facility here but that seems awfully specific to this particular chip/assembler
    incZ
  skipPrintCharAndAdvance:
  ret

printChar:
  ;     
  ;  Prints a byte to uart
  ;    Turn \n's into \r\n's and simply ignore \r's
  ;    that way we can store strings in a more typical style with \n's
  ;    (Useful when working/debugging uart at the terminal)
  ;
  ;  USAGE:
  ;    mov outChar, rXX
  ;    call printChar
  ;

  ;  NOTE: do this synchronously, since doing this 
  ;        asynchronously without a queue will simply drop data
  ;        Change this later? No, too needlessly complex

  ;Is it a carriage return?
  cpi outChar,'\r'
  breq skipCarriageReturn
    ;Is it a newline?
    cpi outChar,'\n'
    brne skipCarriageReturnPrepend
      ;If is newline, prepend a carriage return -> '\r\n'
      ldi outChar,'\r'
      ;Print the carriage return
      call uart_transmit_byte_sync
      ;You overwrote this above, so restore it for the print char invoke below
      ldi outChar,'\n' ; <LF> 
    skipCarriageReturnPrepend:
    ;Print the char whatever it is
    call uart_transmit_byte_sync
  skipCarriageReturn:

  ret

printHexByte:
  ;Print the hex byte in outChar. ie: 0b5C -> '5','C'
  push r4
  mov r4,outChar

  ;Get high nibble
  andi outChar,0xf0
  ;Divide high nibble by 16
  lsr outChar
  lsr outChar
  lsr outChar
  lsr outChar
  call printHexNibble

  ;Get low nibble
  mov outChar,r4
  andi outChar,0x0f
  call printHexNibble

  pop r4
  ret

printHexNibble:
  ;Have to print a hex byte one nibble (4bits) at a time since F is 0b1111
  push r23
  ;Is the nibble an alpha value (A-F) ?
  cpi outChar,0x0A
  brlt skipAddingAlphaOffset
    ;add 0x41 to get alpha offset
    ldi r23,0x37 ; 0x37 = 0x41 ('A') - 10 (0x0A)
    add outChar,r23
  rjmp skipAddingNumericOffset
  skipAddingAlphaOffset:
    ;add 0x30 to get numeric offset
    ldi r23,0x30
    add outChar,r23
  skipAddingNumericOffset:
  pop r23

  ;Print the nibble char
  call printChar

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
  push r2
  uart_transmit_check:
    ; Wait for empty transmit buffer
    lds r2, UCSR0A
    ; Skip_if_Bit_in_Register_Set
    sbrs r2, UDRE0
    ; UDRE0 not set, data not ready, check again
    rjmp uart_transmit_check
  ; Put data (r16) into buffer; this sends the data
  sts UDR0,outChar

  pop r2
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
