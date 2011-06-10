    .pc = $0800 // start at BASIC

    .import source "rtl\Macros_RTL.asm"

    .byte $00, $0c, $08, $0a, $00, $9e, $20, $32 // encode SYS 2064
    .byte $30, $36, $34, $00, $00, $00, $00, $00 // as BASIC line

    .pc = 2064
    jmp main
main:
//load floating point number into FAC1
    lda #<L1
    ldy #>L1
    jsr $bba2

//convert number in FAC1 to ASCII (pointer in A & Y)
    jsr $bddd

    :PrintStringAY() //print ASCII string pointed to by A/Y pointer
    jmp L2
L1:
    .byte $87, $4c, $00, $00, $00
L2:
    rts
