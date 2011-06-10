    .pc = $0800 // start at BASIC

    .import code "macros\Macros_RTL.macro"

    .byte $00, $0c, $08, $0a, $00, $9e, $20, $32 // encode SYS 2064
    .byte $30, $36, $34, $00, $00, $00, $00, $00 // as BASIC line

Lab2064:
    JMP main
main:
//load floating point number into FAC1
    LDA #<L1
    LDY #>L1
    JSR $BBA2

//convert number in FAC1 to ASCII (pointer in A & Y)
    JSR $BDDD

    PrintStringAY() //print ASCII string pointed to by A/Y pointer
    JMP L2
L1:
    .byte $87 $4C $00 $00 $00
L2:
    RTS
