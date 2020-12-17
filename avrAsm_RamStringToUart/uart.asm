
rjmp END_UART_DEF

  UART_Init:
    ;USAGE:
    ;  
    ;  .equ FOSC = 16000000
    ;  .equ BAUD = 9600
    ;  call UART_Init
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
  UART_Transmit:
    ;USAGE:
    ;  mov outChar, rXX
    ;  call UART_Transmit
    push tmp
    UART_Transmit_Check:
      ; Wait for empty transmit buffer
      lds tmp, UCSR0A
      sbrs tmp, UDRE0
      ; UDRE0 not set, data not ready, check again
      rjmp UART_Transmit_Check
    ; Put data (r16) into buffer, sends the data
    sts UDR0,outChar
    pop tmp
    ret


  .def inChar = r19
  UART_Receive:
    ;USAGE:
    ;  call UART_Receive
    ;  mov rXX, inChar
    push tmp
    UART_Receive_Check:
      ; Wait for data to be received
      lds tmp, UCSR0A
      sbrs tmp, RXC0
      ; RXC0 not set, data not ready, check again
      rjmp UART_Receive_Check
    ; Get and return received data from buffer
    lds inChar, UDR0
    pop tmp
    ret


  UART_Flush:
    ; UNUSED
    ;Not sure if is useful; last instruction looks unreachable
    lds r16, UCSR0A
    sbrs r16, RXC0
    ret
    rjmp UART_Flush
    lds r16, UDR0


END_UART_DEF:
