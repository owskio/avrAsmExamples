;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; NOTE:
;
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
  ;Apparently 328p's only have one UART port 
  ;and therefore no distinction for channel 0,
  ;but they do have teh URXC0addr vector?
  #define URXC0addr URXCaddr
#endif

;This is the 1284 mcaro if ever needed
;#ifndef _M1284DEF_INC_
;#endif

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DATA/RAM
.dseg 

;I wasn't sure where to put this buffer, 
;but thankfully the assembler can evaluate expressions like RAMEND/2
;TODO: consider removing this buffer in favor of just advancing pointers and altering a state register on each input char
;      but this would require an event driven serial protocol when streaming programs to the chip
;
.org RAMEND/3
  word_buffer_begin:
    .byte 16
  word_buffer_end:

;reset the ram location counter to the first viable location 
;(right after the io/memory-mapped region in low ram)
.org SRAM_START 
parameterStack:
parameterStackPointer:

;Unused
;.org (2*RAMEND)/3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEFS
;Not sure if this is the best place for this ;declarations/initializations should always be next to the code that uses them
.def rTmpH                = r25
.def rTmpL                = r24
.def rIn                  = r21
.def rOut                 = r20
.def dataStackPointerHigh = r17 //r23
.def dataStackPointerLow  = r16 //r22

.def rIsInterpreting      = r3
.def rDictionaryRestoreH  = r5
.def rDictionaryRestoreL  = r4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;CODE

.cseg ;Code (flash) segment

;VECTOR ASSIGNMENTS

  ; Reset Vector, just go to init
  .org 0x0000
  rjmp Reset
  ;NOTE: no code will run after this line before Reset: after flashing as the chip is reset at that point

  ; USART0, Rx Complete
  .org URXC0addr
  rjmp uart_receive_complete

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MACROS THAT DECOUPLE US FROM AVR SPECIAL OPS

#define immediate8(operation,destination,value) \
  push rTmpH                                    \
  ldi rTmpH, value                              \
  operation destination,rTmpH                   \
  pop rTmpH                                   

;Set big compound register to address in data/ram/data-segment/memory(as opposed to 'program')
#define loadImmediate16(regH,regL,valH,valL) \
  ldi regH,valH                              \
  ldi regL,valL

#define add16(regAH,regAL,regBH,regBL)\
  add regAL,regBL                     \
  adc regAH,regBH                              
  
;UNUSED
;#define sub16(regAH,regAL,regBH,regBL)\
;  sub  regAL,regBL                    \
;  subc regAH,regBH                              

#define subImmediate16(regAH,regAL,valH,valL)\
  /* Neither subi, nor sub have entries in the docs, */\
  /* but sbci does, which is how i found out about   */\
  /* them, in the sbci example                       */\
  subi regAL,valL                                      \
  sbci regAH,valH                             

#define double16(regH,regL)                          \
  /* Mostly for flash word-2-byte addr conversion */ \
  /* times 2 lowByte, generate carry */              \
  lsl regL                                           \
  /* Rotate_Over_Left_with_carry     */              \
  /* times 2 plus prior carry bit    */              \
  rol regH

#define halve16(regH,regL) \
  lsr regH                 \
  ror regL

#define compare16(regAH,regAL,regBH,regBL) \
  cp  regAL,regBL                          \
  cpc regAH,regBH

;Useful for inc-ing and adding
#define addImmediate16(regH,regL,valH,valL) \
  push rTmpL                                \
  push rTmpH                                \
  loadImmediate16(rTmpH,rTmpL,valH,valL)    \
  add16(regH,regL,rTmpH,rTmpL)              \
  pop rTmpH                                 \
  pop rTmpL

#define compareImmediate16(regH,regL,valH,valL) \
  push rTmpL                                    \
  push rTmpH                                    \
  loadImmediate16(rTmpH,rTmpL,valH,valL)        \
  compare16(regH,regL,rTmpH,rTmpL)              \
  pop rTmpH                                     \
  pop rTmpL

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MACROS THAT GO ALONG WITH SUBROUTINES

#define uartPrintHexByteImmediate(val) \
  ldi rOut,val                         \
  call uartPrintHexByte

#define printUartCharImmediate(ch) \
  ldi rOut,ch                      \
  call uartPrintChar

#define uartPrintCharFromReg(reg) \
  mov rOut,reg                    \
  call uartPrintChar

;Used this to debug the dictionary chain once
;apparently the .db directive only burns 1 byte 'per expression',
;even if you try to persist a 16bit reference
;like this: .db prev_ref
;
;#define printHexByteFromReg(reg) \
;  mov rOut,reg
;  call uartPrintHexByte

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;MACROS THAT ACT AS SIMPLE SYNTACTIC SUGAR (ie 1-to-1 non recursive mappings, LL(0) ?)

#define compareZ(expression)         \
  compareImmediate16(ZH,ZL,high(expression),low(expression))

;These macros allow us to avoid using the X,Y,Z expressions
;since X and XH:XL do not seem to be generally interchangeable 
;expressions for some reason and we want our code to be dependent 
;upon data/data-structures, not magical hardware/build-chain facilities
#define setX(val16) loadImmediate16(XH,XL,high(val16),low(val16))
#define setY(val16) loadImmediate16(YH,YL,high(val16),low(val16))
#define setZ(val16) loadImmediate16(ZH,ZL,high(val16),low(val16))

;Increment the pointer registers
#define incX addImmediate16(XH,XL,0,1)
#define incY addImmediate16(YH,YL,0,1)
#define incZ addImmediate16(ZH,ZL,0,1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; DATA STACK

; TODO: consider turning these macros into subroutines

#define dspH dataStackPointerHigh
#define dspL dataStackPointerLow

#define dataStackPushReg(reg)    \
  push ZH \
  push ZL \
  mov ZH,dspH\
  mov ZL,dspL\
  st Z,reg      \
  pop ZL\
  pop ZH\
  addImmediate16(dspH,dspL,0,1)

#define dataStackPopReg(reg)         \
  /* addImmediate16(dspH,dspL,0,-1) ; did not work */\
  subImmediate16(dspH,dspL,0,1)\
  push ZH \
  push ZL \
  mov ZH,dspH\
  mov ZL,dspL\
  ld reg,Z \
  pop ZL\
  pop ZH

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; INIT

Reset:

  ;Setup port directions
  immediate8(out,DDRB,0xff)
  immediate8(out,DDRC,0xff)

  ;16MHz crystal ;CKDIV8 not set
  .equ FOSC = 16000000
  .equ BAUD = 9600
  call uartInit

  ;Initialize the data-stack pointer
  ldi dspH,high(parameterStack)
  ldi dspL,low(parameterStack)

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
  lds rIn, UDR0
  ;And process it
  call processInputCharacter

  reti

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;INPUT PROCESSING ROUTINES

processInputCharacter:
  ;TODO: process back-space delete most recent char in input buffer. down to buffer_begin that is
  ;TODO: process spaces/subsequent-spaces as no-ops when the input buffer is empty
  ;      or the input buffer pointer points to the beginning of the input buffer

  ;If I type in any white-space char at all, then 'consume the word'
  ;which is why we add a NULL(0x00) byte to the buffer to signify
  ;the end of the word
  call isWhiteSpace
  brne skipWordConsumption

    ;Now that the end of the word has been signalled,
    ;store a null byte in the next empty position in the
    ;word buffer to signal the end of the word string
    immediate8(st,X,0x00)

    ;Reset the 'write' pointer to the beginning of the word buffer
    ;to start recording the next word being typed in
    setX(word_buffer_begin)

    ;Just for debugging/interactivity, print the current buffer to uart
    call printInputBuffer

    ;So we have a 'word'combinator in the input buffer (*X), so try to execute it
    call executeInputBufferWord

  rjmp skipCharStorage
  skipWordConsumption:
    ;Otherwise, word is still incoming (via keyboard or uart), so skip 'consumption'

    ;Write the input character to the current 'write' position in the RAM buffer
    ;, and then increment the 'write' pointer
    st X,rIn
    ;Dont use that X+ trash
    incX
  skipCharStorage:

  ;Echo the input character, also to make using the uart terminal connection easier
  uartPrintCharFromReg(rIn)

  ret

executeInputBufferWord:
  ;Search the dictionary for the word and if found, execute it

  ;Q: Maybe I should have a 2*8 register self-de-reference macro? Because that's what I'm doing here
  ;A: Turns out you can't really do that since only lpm can read memory and lpm only uses Z, so you need secondary temp registers, but if you try to dereference using those then you will just get push-popped values, and there is no way to validate macros for a specific set of registers that I know of
  setZ(aardvark)
  ; word-2-byte address conversion
  double16(ZH,ZL) 
  ;Z now has byte address, not word address
  ;Z now points to first definition-entry in the 'dictionary'

  ;Z -> flash dictionary traversal location
  ;Y ->ram buffer traversal location
  call dictionarySearch

  ret


printInputBuffer:
  ;Print out the ram buffer's contents just to show the user (me) what was entered

  printUartCharImmediate('\n')
  printUartCharImmediate('"')

  ;Echo it out from RAM
  setZ(word_buffer_begin)
  call uartPrintRamString

  printUartCharImmediate('"')
  printUartCharImmediate('\n')

  ret


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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


;This expands to the flash memory location whereever this macro is called 
;it never gets used directly in the code, it just allows us to set up the 
;lookup/prototype chain by assigning it to prev_def and then expanding    
;prev_def to the current value in the next definition                     
;
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
;Which is sort of like this:
;
;  myFun_label:
;    .db prev_def
;    "myFun\0"
;
;Which in turn gets assembled to something like this:
;
;  0x0001 0x0000
;  0x0002 "y","m"
;  0x0003 "u","F"
;  ...
;  //"urFun" expansion below
;  0x0005 0x0001
;  0x0006 "r","u"
;  0x0007 "u","F"
;  ...
;
;

;These macros are just syntactic sugar 
;which allow us to place rom strings at the site of invocation
;which we will immediatly interpret with evalThisRomString

#define romFun(funName) \
  fun(funName)          \
    call evalThisRomString        \
    .db 

#define romFunRet \
  ," ",'\0' \
  ret

;prev_def is a variable the assembler allows us to set/read
;while the assembly process is taking place but without affecting
;the memory footprint of the program
.set prev_def=0x0000


;AMAZING, YES, IT WORKS!!

fun(asdf)
  immediate8(out,PortC,0x33)
  ret

fun(qwer)
  immediate8(out,PortC,0x55)
  ret

fun(poiu)
  immediate8(out,PortC,0x99)
  ret


fun(hi)
  .dseg
    play_area:
  .cseg
  push ZH
  push ZL

  setZ(play_area)
  immediate8(st,Z+,'\n')
  immediate8(st,Z+,'H')
  immediate8(st,Z+,'e')
  immediate8(st,Z+,'l')
  immediate8(st,Z+,'l')
  immediate8(st,Z+,'o')
  immediate8(st,Z+,' ')
  immediate8(st,Z+,'W')
  immediate8(st,Z+,'o')
  immediate8(st,Z+,'r')
  immediate8(st,Z+,'l')
  immediate8(st,Z+,'d')
  immediate8(st,Z+,'!')
  immediate8(st,Z+,'\n')
  immediate8(st,Z+, 0 )
  
  setZ(play_area)
  call uartPrintRamString

  pop ZL
  pop ZH
  ret

fun(one)
  ldi r19,'1'
  uartPrintCharFromReg(r19)
  ret

fun(lsl)
  lsl r19
  uartPrintCharFromReg(r19)
  ret

fun(add)
  ;immediate8(add,r19,1)
  ;push rTmpH                                     
  ;ldi rTmpH, 1                               
  ;add r19,rTmpH                    
  ;pop rTmpH
  ;nop
  ;nop
  out PortC,r19
  ret

fun(hex2)
  uartPrintHexByteImmediate(0x0f)
  ret

fun(hex)
  uartPrintHexByteImmediate(0x55)
  ret

fun(quad)
  call evalThisRomString
  .db \
  "one led lsl"\
  ," " ,'\0' 
  ret

;fun(twice)          
  ;call evalThisRomString      
  ;.db \
  ;," ",'\0','\0'
  ;ret

;NICE!
romFun(won)\
  "one"\
  romFunRet

romFun(twice)\
  "one lsl lsl led"\
  romFunRet
  
fun(print)
  uartPrintCharFromReg(r19)
  ret

fun(put)
  dataStackPushReg(r19)
  ret

fun(get)
  dataStackPopReg(r19)
  ret

fun(dspL)
  mov rOut,dspL
  call uartPrintHexByte
  ret

fun(dspH)
  mov rOut,dspH
  call uartPrintHexByte
  ret

; points to the first word in the dictionary
; so chosen because 'Aardvark' is the first *real* word in the only *real* dictionary
aardvark:
  .db low(prev_def),high(prev_def)


;AVRs apparently use a 'post decrement scheme during call'
;I'm going to assume that means that SPH:SPL points to empty space 
;because it was decremented after the return address was pushed
;(recall SP 'grows' downward in address number, but upwards in line number when reading)
;It also appears after some inspection that the return-address-low-byte is pushed first
;and the return-address-high-byte is pushed second, 
;which is why the first read value goes into the 'high' register
;and the second addressed value goes into the 'low' register

#define dereferenceZrom                                        \
  /* may want to turn this into a routine at some point  */ \
  /* I played around with many ways of abstracting this  */ \
  /* code but restricting this                           */ \
  /* macro to Z only due to lpm's reliance on it was the */ \
  /* simplest way to go, in the end.                     */ \
  push rTmpL                                                  \
  push rTmpH                                                  \
  /* Load low,then high */                                  \
  lpm rTmpL,Z                                                 \
  incZ                                                      \
  lpm rTmpH,Z                                                 \
  mov ZL,rTmpL                                                \
  mov ZH,rTmpH                                                \
  pop rTmpH                                                   \
  pop rTmpL                      

#define dereferenceStackPointer\
  /* This CANNOT be placed into a subroutine */ \
  /* because then it would return the wrong value*/ \
  in ZH,SPH                                                                              \
  in ZL,SPL                                                                              \
  /* ZH:ZL should now point to the empty space where the                              */ \
  /* next return address (as the result of another call) would go                     */ \
  /* Point Z to the return address high byte                                          */ \
  incZ                                                                                   \
  /*  NB: push alters SPH/SPL  */                                               \
  push rTmpL                                                      \
  push rTmpH                                                                               \
  /* and dereference to get the return address high byte                              */ \
  ld rTmpH,Z                                                                               \
  /* Point Z to the return address low byte                                           */ \
  incZ                                                                                   \
  /* and dereference to get the return address low byte                               */ \
  ld rTmpL,Z                                                                              \
  double16(rTmpH,rTmpL)\
  mov ZH,rTmpH                                   \
  mov ZL,rTmpL                                   \
  pop rTmpH                                                   \
  pop rTmpL                      


evalThisRomString:
  ;TODO: CONVERT THIS AWAY FROM STORING VALS ON THE RETURN STACK
  ;
  ;I like this technique, it is very readable and compact
  ;I may expand this to a 'map' implmentation which would
  ;give me the ability to say simple things like this:
  ;
  ;  callPrintRomStringLiteral
  ;    "print me",'\n',0
  ;
  ;or this:
  ;  testCharSetMembership
  ;    "\t\r\n ",0
  ;
  ;or this:
  ;  loadIntoRam
  ;    "someRomString",0
  ;
  ;  call map
  ;    mapperRoutine
  ;    .db "asdf",0

  dereferenceStackPointer

  ;TODO: find out if we need an equivalent/comparable version for ram
  ;      and if we do need an 'evalRam', then break this routine into two
  ;      one for dereffing which will always be the same since SP is in IO
  ;      and the data in SP is always in the stack which is always in RAM
  ;      and then create another routine like the one below but which uses
  ;      ld instead of lpm to get the next char out of ram and not rom

  ;TODO: evaluate the necessity of storing/restoring Z below, it bothers me, should not be necessary if we were to use our own data stack maybe?

  ;We need the pointer to the string-body in Z 
  emitCharForProcessing:

    ;Get the char
    lpm rIn, Z
    ;Have we reached a null-byte terminator?
    cpi rIn,0x00
    breq skipEmitNextChar

      ;Save Z
      mov r10,ZH  
      mov r9 ,ZL
      ;push ZH
      ;push ZL
      
      ;  r7, and r8 did not work here wtf (even though this would have been the only use site)
      ;  r9, & r10 seem to work, and they are also non rDictionaryRestoreH6-r31 (needed for ldi's)
      ;  I guess you just cant trust the registers in this fucking machine
      ;  -> Much later, this issue appears to no longer exist. But I know what I saw.
      ;  -> Actually, this issue still exists, I think r8 is being cleared as a side-effect of some other operation
      ;     One can see this erroneous behavior in the evalThisRomString combinators (at least on the 328p I'm using)

      ;Process the char (ruins Z's state, for now)
      call processInputCharacter
      ;Restore Z for the increment
      ;pop ZL
      ;pop ZH
      mov ZH,r10
      mov ZL,r9

      ;Go to the next rom char
      incZ

      rjmp emitCharForProcessing
  skipEmitNextChar:
  
  ;Z now points to the last literal char \0
  ;We want it to point to the next instruction, so increment
  halve16(ZH,ZL)
  ;mov rOut,ZH
  ldi rOut,0x23
  call uartPrintHexByte
  ;mov rOut,ZL
  ldi rOut,0x55
  call uartPrintHexByte
  incZ
  ;mov rOut,ZH
  ;call uartPrintHexByte
  ;printUartCharImmediate(' ')
  ;mov rOut,ZL
  ;call uartPrintHexByte
  ;If the null char was mis-aligned, then Z doesn't point to the next instruction
  ;it merely points to the padding byte, which is also null
  ;So increment again and clear the last bit (effectively add word instead of add byte)
  ;incZ
  ;andi ZL,0xfe
  ;Now Z should definitely point to the next instruction (rom word address)
  ;so we need to replace the return stack values before ret'ing
  mov r10,XH
  mov r9,XL
  in XH,SPH
  in XL,SPL
  ;X points to an 'empty' space, increment to point to the return pointer high byte
  incX
  ;Store the pointer to the place after the literal in the place ret expects
  st X,ZH
  ;Point x to the low byte position of the return pointer on the return stack
  incX
  ;Store the pointer to the place after the literal in the place ret expects
  st X,ZL
  mov XH,r10
  mov XL,r9

  ret


;Now to find a word, start with prev_def and look back
;Storing prev_def because it is an assembler 'symbol' and
;don't know what it's value will be if used inside a
;subroutine later,so I will just use a memory address,
;which I am somewhat familiar with now


dictionarySearch:
  dictionarySearchBegin:

    ;Restore Y to where it was before advancing it during comparison
    setY(word_buffer_begin)

    ;Z contains pointer to first prev_ref, 
    ;or the prev_word pointer
    ;so need to dereference
    dereferenceZrom

    ;word addr to byte addr 
    double16(ZH,ZL)            

    ;Keep rDictionaryRestoreH:rDictionaryRestoreL initialized incase this comparison fails
    ;and we need to go to the 'previous word' to find code to execute
    mov rDictionaryRestoreL,ZH
    mov rDictionaryRestoreH,ZL
    ;movw rDictionaryRestoreL:rDictionaryRestoreH,ZH:ZL ; would work, but only for instructions like movw and adiw, and is actually kind of specific to this ass/arch
    ;so note to self: do not use the colon syntax

    ;skip the 'previous word' reference ; skip low byte ; skip high byte
    incZ
    incZ
    ;Z should now point to the first character in the dictionary key

    ;If the Z flag is still set, it means we got to the end of the strings and both are the same value (null)
    call dictionaryKeyComparision
    brne skipExecuteWord

      ;So jump to the desired word code
      ; need word address for icall, not byte address, so div by 2
      halve16(ZH,ZL)
      icall ; Indirect_CALL_to_z

    rjmp skipRecurse
    skipExecuteWord:

      ;If the string comparison failed:
      ;Restore Z to the beginning of the dictionary-entry where we have the pointer to the previous dictionary-entry
      mov ZH, rDictionaryRestoreL
      mov ZL, rDictionaryRestoreH

      ; If the pointer to the previous definition is null then we are at the top of the dictionary and can go no further
      compareZ(0x0000)
      breq skipDereferenceAndPrintError
        ;Otherwise we are good to go to the next definition-entry,
        ;and "recurse" into what is currently called 'dictionary search' but which would be more accurately described as 'entryCheck' or so
        rjmp dictionarySearchBegin
      skipDereferenceAndPrintError:
        ;TODO: make this a macro? or an EvalRomString routine?
        ;If at end of dictionary chain, print 'Definition not found'
        setZ(message_definition_not_found)
        ;word-2-byte address conversion
        double16(ZH,ZL)
        ;print it
        call uartPrintRomString
        rjmp thisIsNotCode
        message_definition_not_found:
          .db '\n',"Definition not found",'\n',0
        thisIsNotCode:

    skipRecurse:
    ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;STRING UTILITIES

isWhiteSpace:
  ;Test if the char in 'rIn' is a whitespace character or not
  cpi rIn,'\t'
  breq yesWhite
  cpi rIn,' '
  breq yesWhite
  cpi rIn,'\r'
  breq yesWhite
  cpi rIn,'\n'
  breq yesWhite
    clz ; CLear_Z_flag
    rjmp notWhite
  yesWhite:
    sez ; SEt_Z_flag
  notWhite:
  ret

dictionaryKeyComparision:
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
  stringComparisonBegin:

    ;Now we need to compare Z(rom) and Y(ram)
    ;Lets just use the Z bit as the output

    ;now put the first char of the dict key in rTmpL
    lpm rTmpL,Z
    ;load rTmpH with the first char of the word buffer
    ld rTmpH,Y
    ;now compare the two chars
    cp rTmpH,rTmpL
    ;if two chars are not equal, then this dictionary key is bad, go to the next dictionary key
    brne stringComparisonEnd

      ;Otherwise continue comparing the next pair of characters
      ;if both chars are null then break but also check if both strings are empty
      cpi rTmpL,0x00 ; both chars equal so only need to check that one is 0x00
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
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;PRINTING UTILITIES

uartPrintRomString:
  ;Loop through each byte location and print out the character found to the uart connection
  printCurrentFlashChar:
    ;load the current char for uart transmission
    lpm rOut,Z

    cpi rOut,0x00
    breq exitPrintRomString
      ;Otherwise, print the current character
      call uartPrintChar
      ;and Increment the 16 bit "pointer" to the next char
      ;I could have used the Z+ 'post increment' facility here but that seems awfully specific to this particular chip/assembler
      incZ
      rjmp printCurrentFlashChar
    exitPrintRomString:
  ret

uartPrintRamString:
  ;Loop through each byte location and print out the character found to the uart connection
  printCurrentRamChar:
    ;load the current char for uart transmission
    ld rOut,Z

    cpi rOut,0x00
    breq exitPrintRamString
      ;Otherwise, print the current character
      call uartPrintChar
      ;and Increment the 16 bit "pointer" to the next char
      ;I could have used the Z+ 'post increment' facility here but that seems awfully specific to this particular chip/assembler
      incZ
      rjmp printCurrentRamChar
    exitPrintRamString:
  ret

uartPrintHexByte:
  ;Print the hex byte in rOut. ie: 0b5C -> '5','C'

  ;Store rOut for restoration below
  push rOut

  ;Get high nibble
  ;Divide high nibble by 16 to get it into the printable low nibble position
  lsr rOut
  lsr rOut
  lsr rOut
  lsr rOut
  ;Print the high nibble
  call uartPrintHexNibble

  ;Restore outcharOriginal value
  pop rOut

  ;Get low nibble
  andi rOut,0x0f
  call uartPrintHexNibble

  ret

uartPrintHexNibble:
  ;Have to print a hex byte one nibble (4bits) at a time since F is 0b1111

  ;Is the nibble a numeric value (ie less than 'A') ?
  cpi rOut,0x0A
  brlt skipAddingAlphaOffset
    ;add 0x41 to get alpha offset
    ; 0x37 = 0x41 ('A') - 10 (0x0A)
    immediate8(add,rOut,0x37)
  rjmp skipAddingNumericOffset
  skipAddingAlphaOffset:
    ;add 0x30 to get numeric offset
    immediate8(add,rOut,0x30)
  skipAddingNumericOffset:

  ;Print the nibble char
  call uartPrintChar

  ret

uartPrintChar:
  ;     
  ;  Prints a byte to uart
  ;    Turn \n's into \r\n's and simply ignore \r's
  ;    that way we can store strings in a more typical style with \n's
  ;    (Useful when working/debugging uart at the terminal)
  ;
  ;  USAGE:
  ;    mov rOut, rXX
  ;    call uartPrintChar
  ;

  ;  NOTE: do this synchronously, since doing this 
  ;        asynchronously without a queue will simply drop data
  ;        Change this later? No, too needlessly complex

  ;Is it a carriage return?
  cpi rOut,'\r'
  breq skipCarriageReturn
    ;Is it a newline?
    cpi rOut,'\n'
    brne skipCarriageReturnPrepend
      ;If is newline, prepend a carriage return -> '\r\n'
      ldi rOut,'\r'
      ;Print the carriage return
      call uartTransmitByteSync
      ;You overwrote this above, so restore it for the print char invoke below
      ldi rOut,'\n' ; <LF> 
    skipCarriageReturnPrepend:
    ;Print the char whatever it is
    call uartTransmitByteSync
  skipCarriageReturn:

  ret
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;UART STUFF

uartInit:
  ;USAGE:
  ;
  ;  .equ FOSC = 16000000
  ;  .equ BAUD = 9600
  ;  call uartInit
  ;

  ;Set baud rate
  ;NOTE: ubrr is a 12 bit register so
  ;      take the 16bit clocks/baud-period value and (<< 2^4) aka (/16)
  ;      but not sure what the -1 is for
  ;      the -1 just comes from Atmel's documentation without explanation
  .equ ubrr = (((FOSC/BAUD)/16)-1)
  ;.equ kind of like #define except you can forward reference .equ expressions, which I guess is bad
  immediate8(sts, UBRR0H, ubrr >> 8) ;High byte
  immediate8(sts, UBRR0L, ubrr )

  ; Enable receiver and transmitter ;Enable the rx complete interrupt
  immediate8(sts, UCSR0B, (1<<RXEN0)|(1<<TXEN0)|(1<<RXCIE0))

  ; Set frame format: 8data, 2stop bit
  immediate8(sts, UCSR0C, (1<<USBS0) |(1<<UCSZ01)|(1<<UCSZ00))

  ret


uartTransmitByteSync:
  ;USAGE:
  ;  mov rOut, rXX
  ;  call uartTransmitByteSync
  push rTmpH
  uart_transmit_check:
    ; Wait for empty transmit buffer
    lds rTmpH, UCSR0A
    ; Skip_if_Bit_in_Register_Set
    sbrs rTmpH, UDRE0
    ; UDRE0 not set, data not ready, check again
    rjmp uart_transmit_check
  ; Put data (rOut) into buffer; this sends the data
  sts UDR0,rOut

  pop rTmpH
  ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
