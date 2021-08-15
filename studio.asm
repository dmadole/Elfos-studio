; This software is copyright 2021 by David S. Madole.
; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.


           ; Include kernel API entry points

           include bios.inc
           include kernel.inc


           ; convenience definitions

null       equ     0                   ; sometimes this is more expressive
o_ivec     equ     v_ivec-1            ; make this look more like a vector


           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start


           ; Build information

start:     br      main
           db      8+80h              ; month
           db      7                 ; day
           dw      2021               ; year
           dw      0                  ; build

           db      'Written by David S. Madole',0


           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for the heap manager to allocate
           ; memory for the persistent module to use.

main:      ldi     high k_ver          ; get pointer to kernel version
           phi     r7
           ldi     low k_ver
           plo     r7

           lda     r7                  ; if major is non-zero we are good
           lbnz    allocmem

           lda     r7                  ; if major is zero and minor is 4
           smi     4                   ;  or higher we are good
           lbdf    allocmem

           sep     scall               ; if not meeting minimum version
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0

           sep     sret


           ; Allocate memory from the heap for the driver code block, leaving
           ; address of block in register R8 and RF for copying code and
           ; hooking vectors and the length of code to copy in RB.

allocmem:  ldi     high end-module     ; size of permanent code module
           phi     rb
           phi     rc
           ldi     low end-module
           plo     rb
           plo     rc

           ldi     255                 ; request page-aligned block
           phi     r7
           ldi     4                   ; request permanent block
           plo     r7

           sep     scall               ; allocate block on heap
           dw      o_alloc
           lbnf    copycode

           sep     scall               ; if unable to get memory
           dw      o_inmsg
           db      'ERROR: Unable to allocate heap memory',13,10,0

           sep     sret


           ; Copy the code of the persistent module to the memory block that
           ; was just allocated using RF for destination and RB for length.
           ; This burns RF and RB but R8 will still point to the block.

copycode:  ldi     high module         ; get source address to copy from
           phi     rd
           ldi     low module
           plo     rd

           glo     rf                  ; make a copy of block pointer
           plo     r8
           ghi     rf
           phi     r8

copyloop:  lda     rd                  ; copy code to destination address
           str     rf
           inc     rf
           dec     rb
           glo     rb
           lbnz    copyloop
           ghi     rb
           lbnz    copyloop

           ghi     r8                  ; put offset between source and
           smi     high module         ;  destination onto stack
           str     r2


           ; Disable interrupts while we manipulate the interrupt service
           ; chain, R1, and other hooks, so that we don't get an interrupt
           ; when things are half-baked like msb is changed but not lsb.

           sex     r3                  ; disable interrupts, leaving x=2
           dis
           db      23h


           ; Update kernel hooks to point to our module code. Use the offset
           ; to the heap block at M(R2) to update module addresses to match
           ; the copy in the heap. If there is a chain address needed for a
           ; hook, copy that to the module first in the same way.

           ldi     high patchtbl      ; Get point to table of patch points
           phi     r7
           ldi     low patchtbl
           plo     r7

ptchloop:  lda     r7                 ; get address to patch, a zero
           lbz     intenabl           ;  msb marks end of the table
           phi     rd
           lda     r7
           plo     rd
           inc     rd

           lda     r7                 ; if chain needed, then get address,
           lbz     notchain           ;  adjust to heap memory block
           add
           phi     rf
           ldn     r7
           plo     rf
           inc     rf

           lda     rd                 ; patch chain lbr in module code
           str     rf                 ;  to existing vector address
           inc     rf
           ldn     rd
           str     rf
           dec     rd

notchain:  inc     r7                 ; get module call point, adjust to
           lda     r7                 ;  heap, and update into vector jump
           add
           str     rd
           inc     rd
           lda     r7
           str     rd

           lbr     ptchloop


           ; Set R1 to point to the Elf/OS interrupt service routine and
           ; re-enable interrupts; since all the interrupt-level stuff has
           ; been changed it should be safe to do now.

intenabl:  ldi     high i_serve        ; set interrupt program counter
           phi     r1
           ldi     low i_serve
           plo     r1

           sex     r3                  ; enable interrupts, leaving x=2
           ret
           db      23h


           ; We are almost done, but process any command-line arguments.

skipspc1:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc1

           smi     '-'-'!'            ; if next character is not a dash,
           lbnz    getbaud            ; then no option

           lda     ra                 ; if option is not 'k' then it is
           smi     'k'                ; not valid
           lbnz    notvalid

skipspc2:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc2

getbaud:   dec     ra                 ; back up to non-whitespace character

           ghi     ra                 ; move input pointer to rf
           phi     rf
           glo     ra
           plo     rf

           sep     r4                 ; parse input number
           dw      f_atoi
           lbdf    notvalid           ; if not a number then abort

           ; baud rate here

notvalid:  sep     r4                 ; if any argument not valid, then
           dw      o_setbd            ; just auto-baud instead

           sep     scall              ; display identity to indicate success
           dw      o_inmsg
           db      '1854 UART Driver Build 2 for Elf/OS',13,10,0

           sep     sret


           ; Table giving addresses of jump vectors we need to update to
           ; point to us instead, and what to point them to. The patching
           ; code adjusts the target address to the heap memory block.

patchtbl:  dw      o_ivec, intnext, intserv
           dw      o_boot, goboot, reboot
           dw      o_type, null, type
           dw      o_tty, null, type
           dw      o_readkey, null, read
           dw      o_msg, null, msg
           dw      o_inmsg, null, inmsg
           dw      o_input, null, input
           dw      o_inputl, null, inputl
           dw      o_setbd, null, setbd
           db      null


           ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.

           org     (($ + 0ffh) & 0ff00h)

module:    ; Memory-resident module code starts here


; Within this module, a simplified subroutine convention is used to save
; overhead of SCRT and also saving and restoring registers needlessly. The
; way a call is done is to load the return address within the page into the
; D register, and then do a short branch to the subroutine:
;
;          ldi     retaddr
;          br      subroutine
;
; The subroutine pushes the return address to the stack, does it's work,
; then returns by popping the return address and setting it into the low
; byte of the program counter, effecting a branch:
;
;          stxd
;          ...
;          irx
;          ldx
;          plo     r3
;
; This takes just 6 instructions as compared to SCRT which takes 32 as it is
; implemented in Elf/OS. Obviously this only works within a single page of
; memory. It is also not possible to pass values in the D register; this
; uses RE.0 to pass values which works well with the Elf/OS SCRT anyway.
;
; Because the return address is arbitrary and does not have to be in-line
; to execution, this can sometimes replace a branch instruction, so the
; overhead can be only 5 instructions rather than 6.
;
; This is derived from a technique described by Wayne Bowdish on page 22 of
; IPSO FACTO issue 12 (June 1979).


           ; Initialize port

setbd:     glo     rd                  ; save a register to use as pointer
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; get pointer to the recvhead variable
           phi     rd
           ldi     low recvhead
           plo     rd

           inp     6                   ; read stale input and clear status
           inp     7

           sex     r3                  ; set default baud rate
           out     7
           db      00h

           sex     rd                  ; set rd as index register

           ldi     recvbuff            ; initialize buffer pointers to clear
           stxd                        ;  buffer, move to uartecho
           stxd

           ldi     1                   ; set re.1 and uartecho to hardware
           phi     re                  ;  uart with echo, move to uartctrl
           stxd

           ldi     11h                 ; put control-q xon into flow
           stxd

           out     7                   ; set uart config from uartctrl

           inc     r2                  ; restore saved register
           lda     r2
           phi     rd
           ldn     r2
           plo     rd

           sep     sret                ;  return to caller


           ; This interrupt service routine is called by the Elf/OS stub
           ; service routine which saves the X, P, D, and DF registers. It
           ; also decrements R2, so it's safe to use the top of the stack.

intserv:   inp     7                   ; get status register, if da bit
           ani     01h                 ;  not set, then pass to next isr
           bz      intnext

           glo     rd                  ; we need a register to do much of
           stxd                        ;  anything so save rd on stack
           ghi     rd
           stxd

           ghi     r1                  ; set msb of rd to variables page
           phi     rd

           inp     6                   ; get input character to d and
           ori     02h                 ;  to stack, if not xon or xoff,
           xri     13h                 ;  skip ahead to check buffer
           bnz     recvchk

           ldi     low uartflow        ; otherwise, point to flow control
           plo     rd
 
           ldn     r2                  ; get xon or xoff character and
           str     rd                  ;  store, and finish interrupt
           br      intdone

recvchk:   ldi     low recvtail        ; set pointer to tail variable
           plo     rd

           lda     rd                  ; get tail pointer, advance to head,
           adi     1                   ;  increment tail, if wrapped around,
           bnf     recvinc             ;  reset to beginning
           ldi     recvbuff

recvinc:   sex     rd                  ; compare tail+1 to head,
           xor                         ;  if equal, then buffer is full
           bz      intdone             ;  and we are done

           xor                         ; recover original tail value,
           dec     rd                  ;  store into tail variable
           str     rd                  ;  and set pointer to tail of queue
           plo     rd

           ldn     r2                  ; put received character into queue
           str     rd

           ; Done, restore and return from interrupt

intdone:   inc     r2                  ; restore saved rd register before
           lda     r2                  ;  jumping to return
           phi     rd
           ldn     r2
           plo     rd
 
           sex     r2                  ; be pessimistic about next isr,
intnext:   lbr     intnext             ;  then jump to it


           ; This is hooked into o_boot to reset the UART to the same
           ; condition as if the machine was reset. This is especially
           ; important to disable interrupts so an unexpected one does not
           ; arrive after the reboot before the handler has been installed.

reboot:    sex     r3                  ; make arguments inline values

           out     7                   ; disable interrupts from uart
           db      10h
           out     7                   ; reset default baud rate
           db      00h

goboot:    lbr     f_boot              ; continue down boot chain
           

           ; The varaiables here control the driver and and UART config.

uartctrl:  db      39h                 ; control value to configure uart
uartflow:  db      11h                 ; flow control character xon/xoff
uartecho:  db      01h                 ; copy of re.1 from o_read context


           ; The receive buffer extends from wherever it starts up through
           ; the end of the page. It's size can only be changed by changing
           ; the starting point, since the carry flag is used to wrap it.

recvtail:  db      recvbuff            ; pointer to location of last byte
recvhead:  db      recvbuff            ; pointer to just before first byte
recvbuff:  db      0                   ; buffer data up until end of page


           org     (($ + 0ffh) & 0ff00h)


           ; This is the replacement for the standard f_type call in BIOS.
           ; It outputs a single character through the UART via polling and
           ; is called with SCRT, but falls through to the "thin call" send
           ; subroutine, to save a branch. Every instruction counts here!

type:      glo     rd                  ; save rd to use as an index to data
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; load data page address into rd.1
           smi     1
           phi     rd

           ldi     return              ; return to standard return block


           ; This "thin call" subroutine sends one character through the
           ; UART. Is assumes that RD.1 has been set with the data page
           ; address and that it can freely modify RD.0 ; The character to
           ; send is passed in RE.0 and is left unchanged on exit.

send:      stxd                        ; push return address

           ldi     low uartflow        ; point to xon/xoff flow control flag
           plo     rd

sendwait:  inp     7                   ; get uart status register, check if
           xri     80h                 ;  thre is set and es- is cleared,
           ani     90h                 ;  if not, wait until it is so
           bnz     sendwait

           ldn     rd                  ; check the flow2 control flag, if it
           xri     13h                 ;  is xoff then wait until its not
           bz      sendwait

           glo     re                  ; get character to send, store on
           str     r2                  ;  stack, output, and inc r2
           out     6

           ldn     r2                  ; get return address and jump
           plo     r3


           ; This is a standard return block that is used in common by
           ; several of the routines. It can either be set as the return
           ; address in a "thin call" subroutine or branched to directly.

return:    inc     r2                  ; restore saved rd register
           lda     r2
           phi     rd
           ldn     r2
           plo     rd
           
           glo     re                  ; get result and return via scrt
           sep     sret


           ; This is the replacement for the f_msg call which sends a null-
           ; terminated string pointed to by RF. It calls the "thin call"
           ; send subroutine for each character.

msg:       glo     rd                  ; save r3 for data pointer
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; set r3 to point to data page
           smi     1
           phi     rd

msgloop:   lda     rf                  ; get next character, incrementing
           bz      return              ;  pointer, if end, then return
           plo     re

           ldi     msgloop             ; call the send subroutine, returning
           br      send                ;  to above to effect the needed loop


           ; This is the replacement for the f_inmsg call which is just like
           ; f_msg but uses R6 as an index instead of RF and so sends a null-
           ; terminated string that is inline to the SCALL.

inmsg:     glo     rd                  ; save r3 for data pointer
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; set r3 to point to data page
           smi     1
           phi     rd

inmsglp:   lda     r6                  ; get next character, incrementing
           bz      return              ;  pointer, if end, then return
           plo     re

           ldi     inmsglp             ; call the send subroutine, returning
           br      send                ;  to above to effect the needed loop


           ; This is the replacement for the f_read call which gets a
           ; character from the input. This reads from the input buffer that
           ; the interrupt routing populates received data into. This has a
           ; tiny wrapper around the "thin call" recv subroutine that it falls
           ; through to that actually does all the work.

read:      glo     rd                  ; we need data page pointer, save rd
           stxd 
           ghi     rd
           stxd

           ghi     r3                  ; point rd to the data page
           smi     1
           phi     rd

           ldi     chkecho             ; get next input character
           br      recv

chkecho:   ghi     re                  ; if echo flag not set, return
           shr
           bnf     return

           ldi     return              ; send character and return
           br      send


           ; This "thin call" subroutine receives characters by de-queing
           ; them from the buffer the interrupt routine populates. It needs
           ; RD.1 set to the data page address and returns the received
           ; character in RE.0. This also chains to the send "thin call"
           ; subroutine to echo input characters when that is enabled.

recv:      stxd                        ; push return address

           ldi     low uartecho        ; get echo flag in re.1, save into
           plo     rd                  ;  data page for interrupt routine use
           ghi     re
           str     rd

           sex     rd                  ; need to use rd value in comparisons

readwait:  ldi     low recvtail        ; get index to tail pointer
           plo     rd

           lda     rd                  ; if head pointer is same as tail,
           xor                         ;  then buffer is empty, wait for data
           bz      readwait

           ldn     rd                  ; increment head pointer, wrapping
           adi     1                   ;  back to start if overflows
           bnf     readkeep
           ldi     recvbuff

readkeep:  str     rd                  ; update head pointer, then read
           plo     rd                  ;  the data byte the head pointer
           ldn     rd                  ;  points to and put in re.0
           plo     re

           sex     r2                  ; reset x register to r2

           inc     r2                  ; return to caller
           ldn     r2
           plo     r3










input:     ldi     high 256            ; preset for fixed-size version
           phi     rc
           ldi     low 256
           plo     rc

inputl:    dec     rc                  ; space for terminating zero

           glo     rd                  ; use rd for data page pointer
           stxd
           ghi     rd
           stxd

           glo     rb                  ; use rb for counting input
           stxd
           ghi     rb
           stxd

           ghi     r3                  ; point rd to the data page
           smi     1
           phi     rd

           ldi     0                   ; zero input count
           phi     rb
           plo     rb

getchar:   ldi     gotchar             ; read input character
           br      recv

gotchar:   glo     re                  ; get character

           smi     127                 ; got backspace
           bz      gotbksp

           bdf     gotchar             ; has high bit set, ignore

           adi     127-32              ; printing character received
           bdf     gotprnt

           adi     32-8                ; backspace received
           bz      gotbksp

           adi     8-3                 ; control-c received
           bz      gotctlc

           adi     3-13               ; carriage return received
           bnz     getchar


           ; Return from input due to either return or control-c. When
           ; either entry point is called, D will be zero and DF will be
           ; set as a result of the subtraction used for comparison.

gotctlc:   str     rf                  ; zero-terminate input string

           inc     r2                  ; restore saved rb
           lda     r2
           phi     rb
           ldn     r2
           plo     rb

           ghi     re                  ; if not return or echo not enabled,
           shr                         ;  don't echo anything, but save
           glo     re                  ;  input char to stack along the way
           stxd
           smbi    13
           bnz     notecho

           ldi     notecho             ; output return
           br      send

notecho:   irx                         ; get back character typed, if 3
           ldx                         ;  then set df, if 13 then clear df
           sdi     3

           br      return


           ; If a printing character, see if there is any room left in
           ; the buffer, and append it if there is, ignore otherwise.

gotprnt:   glo     rc                  ; if any room for character
           bnz     addprnt
           ghi     rc                  ; if not any room for character
           bz      getchar

addprnt:   glo     re
           str     rf                  ; append character to buffer
           inc     rf

           dec     rc                  ; increment count, decrement space
           inc     rb

           ghi     re                  ; if echo disabled, get next char
           shr
           bnf     getchar

           ldi     getchar             ; echo char and get next
           br      send


           ; Process a backspace received: if not at beginning of buffer,
           ; decrement buffer and count, increment free space, and output
           ; a backspace-space-backspace sequence to erase character.

gotbksp:   glo     rb
           bnz     dobkspc
           ghi     rb
           bz      getchar

dobkspc:   dec     rf                  ; back up pointer

           dec     rb                  ; decrement count, increment space
           inc     rc

           ghi     re                  ; if echo disabled, get next char
           shr
           bnf     getchar

           sep     scall
           dw      o_inmsg
           db      8,32,8,0

           br      getchar



end:       ; That's all folks!


