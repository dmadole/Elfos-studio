; You have permission to use, modify, copy, and distribute
; this software so long as this copyright notice is retained.
; This software may not be used in commercial applications
; without express written permission from the author.


           ; Include kernal API entry points

           include bios.inc
           include kernel.inc

           ; Define non-published API elements

intret     equ     03f0h
iserve     equ     03f6h
ivec       equ     03fdh
version    equ     0400h
himem      equ     0442h

o_intvec   equ     ivec-1

o_tty      equ     035dh
o_inputl   equ     0357h
o_setbd    equ     0360h

           ; Executable program header

           org     2000h - 6
           dw      start
           dw      end-start
           dw      start

start:     org     2000h
           br      main

           ; Build information

           db      8+80h              ; month
           db      18                 ; day
           dw      2021               ; year
           dw      1                  ; build
           db      'Written by David S. Madole',0

           ; Check minimum kernel version we need before doing anything else,
           ; in particular we need support for himem variable to allocate
           ; memory for persistent module to use.

main:      ldi     high himem
           phi     r7
           ldi     low himem
           plo     r7

           ldn     r7
           lbz     versfail

           ; Allocate memory below himem for the driver code block, leaving
           ; address to copy code into in register R8 and R9 and length
           ; of code to copy in RF. Updates himem to reflect allocation.

           ldi     high himem         ; pointer to top of memory variable
           phi     r7
           ldi     low himem
           plo     r7

           inc     r7                 ; move to lsb of himem
           ldn     r7                 ; subtract size to install from himem
           smi     low end-module-1   ; keep borrow flag of result
           ldi     0                  ; but round down to page boundary
           plo     r8
           plo     r9

           dec     r7                 ; move to msb of himem and finish
           ldn     r7                 ; subtraction to get code block address
           smbi    high end-module-1
           phi     r8
           phi     r9

           dec     r8                 ; set himem to one less than block

           ghi     r8                 ; update himem to below new block
           str     r7
           inc     r7
           glo     r8
           str     r7
           dec     r7

           inc     r8                 ; restore to start of code block


           ; Copy the code of the persistent module to the memory block that
           ; was just allocated. R8 and R9 both point to this block before
           ; the copy. R9 will be used but R8 will still point to it after.

           ldi     high module         ; get source address to copy from
           phi     rd
           ldi     low module
           plo     rd

           ldi     high end-module+255
           phi     rf
           ldi     low end-module+255
           plo     rf

copycode:  lda     rd                 ; copy code to destination address
           str     r9
           inc     r9
           dec     rf
           ghi     rf
           lbnz    copycode


           ; Patch existing interrupt vector into our exit jump

           ghi     r8
           adi     high intnext-module
           phi     rd
           ldi     low intnext-module
           plo     rd

           ldi     high ivec
           phi     rf
           ldi     low ivec
           plo     rf

           inc     rd

           lda     rf
           str     rd
           inc     rd
           ldn     rf
           str     rd

           ; Update kernel and BIOS hooks to point to our module code. At
           ; this point, R9 points to the new BIOS jump table in RAM, and
           ; R8 points to the base address of the module code in RAM.

           ldi     high kernhook      ; Get point to table of patch points
           phi     r7
           ldi     low kernhook
           plo     r7

           ghi     r8
           smi     high module
           str     r2

kernloop:  lda     r7                 ; a zero marks end of the table
           lbz     kerndone

           phi     rd
           lda     r7                 ; get lsb of patch address
           plo     rd
           inc     rd                 ; skip the lbr opcode

           lda     r7
           add
           str     rd

           inc     rd
           lda     r7                 ; add the offset in the table to the
           str     rd                 ; address at the patch point

           lbr     kernloop


           ; At this point we are done, set the baud rate either from command
           ; line if supplied, or auto-baud if not or if supplied rate is not
           ; valid, then output a success message, and end.


kerndone: 

skipspc1:  lda     ra                 ; skip any whitespace
           lbz     notvalid
           smi     '!'
           lbnf    skipspc1

           smi     '-'-'!'            ; if next character is not a dash,
           lbnz    getbaud            ; then no option

           lda     ra                 ; if option is not 'k' then it is
           smi     'q'                ; not valid
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

notvalid:  ldi     high iserve
           phi     r1
           ldi     low iserve
           plo     r1

           sep     r4                 ; if any argument not valid, then
           dw      o_setbd            ; just auto-baud instead

output:    sep     scall
           dw      o_inmsg
           db      '1854 UART Driver Build 1 for Elf/OS',13,10,0

           sep     sret

versfail:  sep     scall
           dw      o_inmsg
           db      'ERROR: Needs kernel version 0.4.0 or higher',13,10,0

           sep     sret


           ; Table giving addresses of jump vectors we need to update
           ; to point to us instead, along with offset from the start
           ; of the module in himem to repoint those to.

kernhook:  dw      o_intvec, intserv
           dw      o_type, type
           dw      o_tty, type
           dw      o_readkey, read
           dw      o_msg, msg
           dw      o_inmsg, inmsg
           dw      o_input, input
           dw      o_inputl, inputl
           dw      o_setbd, setbd
           db      0

           org     (($ + 0ffh) & 0ff00h)

module:    ; Start the actual module code on a new page so that it forms
           ; a block of page-relocatable code that will be copied to himem.

; Within this module, a simplified subroutine convention is used to save
; overhead of SCRT and also saving and restoring registers needlessly. The
; way a call is done is to push the return address within the page onto the
; stack, and then do a short branch to the subroutine:
;
;          ldi     retaddr
;          stxd
;          br      subroutine
;
; When done, the subroutine then returns by popping the return address and
; setting it into the low byte of the program counter, effecting a branch:
;
;          irx
;          ldx
;          plo     r3
;
; This takes just 6 instructions as compared to SCRT which takes 32 as it is
; implemented in Elf/OS. Obviously this only works within a single page of
; memory. It is also not possible to return values in the D register; this
; uses RE.0 to pass values which works well with the Elf/OS SCRT anyway.
;
; Because the return address is arbitrary and does not have to be in-line
; to execution, this can replace a branch instruction as well, so the
; overhead is often only 5 instructions rather than 6.
;
; This is barely modified from a technique described by Wayne Bowdish on
; page 22 of IPSO FACTO issue 12 (June 1979).


           ; This is the replacement for the standard f_type call in BIOS.
           ; It outputs a single character through the UART via polling and
           ; is called with SCRT, but falls through to the "thin call" send
           ; subroutine, to save a branch. Every instruction counts here!

type:      glo     rd                  ; save rd to use as an index to data
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; load data page address into rd.1
           adi     1
           phi     rd

           ldi     return              ; return to standard return block
           stxd 


           ; This "thin call" subroutine sends one character through the
           ; UART. Is assumes that RD.1 has been set with the data page
           ; address and that it can freely modify RD.0 ; The character to
           ; send is passed in RE.0 and is left unchanged on exit.

send:      ldi     low uartflow        ; point to xon/xoff flow control flag
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
           adi     1
           phi     rd

msgloop:   lda     rf                  ; get next character, incrementing
           bz      return              ;  pointer, if end, then return
           plo     re

           ldi     msgloop             ; call the send subroutine, returning
           stxd                        ;  to above to effect the needed loop
           br      send


           ; This is the replacement for the f_inmsg call which is just like
           ; f_msg but uses R6 as an index instead of RF and so sends a null-
           ; terminated string that is inline to the SCALL.

inmsg:     glo     rd                  ; save r3 for data pointer
           stxd
           ghi     rd
           stxd

           ghi     r3                  ; set r3 to point to data page
           adi     1
           phi     rd

inmsglp:   lda     r6                  ; get next character, incrementing
           bz      return              ;  pointer, if end, then return
           plo     re

           ldi     inmsglp             ; call the send subroutine, returning
           stxd                        ;  to above to effect the needed loop
           br      send


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
           adi     1
           phi     rd

           ldi     return              ; return through scrt when done
           stxd


           ; This "thin call" subroutine receives characters by de-queing
           ; them from the buffer the interrupt routine populates. It needs
           ; RD.1 set to the data page address and returns the received
           ; character in RE.0. This also chains to the send "thin call"
           ; subroutine to echo input characters when that is enabled.

recv:      ldi     low uartecho        ; get echo flag in re.1, save into
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

           ghi     re                  ; test echo flag, if set, then chain
           ani     1                   ;  into send to echo character
           bnz     send

           inc     r2                  ; if echo not enabled, then we will
           ldn     r2                  ;  return directly
           plo     r3


           ; The input and inputl replacement routines that follow are
           ; derivative works of the f_input and f_inputl routines in the
           ; BIOS. Accordingly, these bear the following additional terms:
           ;
           ;   This software is copyright 2005 by Michael H Riley 
           ;   You have permission to use, modify, copy, and distribute
           ;   this software so long as this copyright notice is retained.
           ;   This software may not be used in commercial applications
           ;   without express written permission from the author.
           ;
           ; These have been modified to use the "thin call" conventions to
           ; directly call the send and recv subroutines to reduce overhead.
           ; There have also been formatting and other minor changes made.

input:     ldi     high 256            ; allow 256 input bytes
           phi     rc
           ldi     low 256
           plo     rc

inputl:    glo     ra                  ; save RA
           stxd
           ghi     ra
           stxd

           glo     rd                  ; the subroutines need rd setup as
           stxd                        ;  a data pointer
           ghi     rd
           stxd

           ghi     r3                  ; point rd to the receive buffer
           adi     1
           phi     rd                  ;  pointers starting at tail

           ldi     0                   ; byte count
           plo     ra                  ; store into counter

inplp:     ldi     inplp1
           stxd
           br      recv

inplp1:    glo     re                  ; save char
           smi     3                   ; check for <CTRL><C>
           bz      inpterm             ; terminate input

           smi     5                   ; check for <BS>
           bz      isbs                ; jump if so

           smi     5                   ; check for <CR>
           bz      inpdone             ; jump if so

           glo     rc                  ; check count
           bnz     inpcnt              ; jump if can continue

           ghi     rc                  ; check high of count
           bnz     inpcnt

           ldi     8                   ; performa backspace
           plo     re

           ldi     bs2
           stxd
           br      send                 ; remove char from screen

inpcnt:    glo     re
           str     rf                  ; store into output
           inc     rf                  ; point to next position

           smi     08                  ; look for backspace
           bnz     nobs                ; jump if not a backspace

isbs:      glo     ra                  ; get input count
           bz      spc                 ; disregard if string is empty

           dec     ra                  ; decrement the count
           dec     rf                  ; decrement buffer position
           inc     rc                  ; increment allowed characters

bs2:       ldi     32                  ; display a space
           plo     re

           ldi     bs3
           stxd
           br      send

bs3:       ldi     8                   ; then backspace again
           plo     re

           ldi     inplp
           stxd
           br      send

spc:       ldi     32                  ; move cursor right again
           plo     re

           ldi     inplp
           stxd
           br      send

nobs:      inc     ra                  ; increment input count
           dec     rc                  ; decrement character count
           br      inplp               ; and then loop back

inpdone:   ldi     0                   ; need a zero terminator
           shr                         ; reset DF flag, to show valid input

inpdone2:  str     rf                  ; store into buffer

           irx                         ; recover RA

           ldxa
           phi     rd
           ldxa
           plo     rd

           ldxa
           phi     ra
           ldx
           plo     ra

           sep     sret                ; return to caller

inpterm:   smi     0                   ; signal <CTRL><C> exit
           ldi     0                   ; finish
           br      inpdone2
          


           org     (($ + 0ffh) & 0ff00h)

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
intnext:   lbr     intret              ;  then jump to it



uartctrl:  db      39h                 ; control value to configure uart
uartflow:  db      11h                 ; flow control character xon/xoff
uartecho:  db      01h                 ; copy of re.1 from o_read context

           ; The receive buffer extends from wherever it starts up through
           ; the end of the page. It's size can only be changed by changing
           ; the starting point, since the carry flag is used to wrap it.

recvtail:  db      recvbuff            ; pointer to location of last byte
recvhead:  db      recvbuff            ; pointer to just before first byte
recvbuff:  db      0                   ; buffer data up until end of page

end:       ; That's all folks!


