
.macro lpmHi
  ;Load from Program Memory (flash) Hi-byte
  ldi ZH,high(2*@1)
  ldi ZL,low(2*@1 + 1)
  lpm @0,Z
.endmacro

.macro lpmLo
  ;Load from Program Memory (flash) Low-byte
  ldi ZH,high(2*@1)
  ldi ZL,low(2*@1 + 0)
  lpm @0,Z
.endmacro

.macro outi
  ;out immediate
  push r30
  ldi r30,@1
  out @0,r30
  pop r30
.endm

.macro stsi
  ;store direct to dataspace immediate
  push r30
  ldi r30,@1
  sts @0,r30
  pop r30
.endm

.macro sti
  ;store direct to dataspace immediate
  push XH
  push XL
  ldi XH,high(@0)
  ldi XL,low(@0)
  push r21
  ldi r21,@1
  st X,r21
  pop r21
  pop XH
  pop XL
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

