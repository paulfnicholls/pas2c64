    .pc = $0800 // start at BASIC
    
    .import source "rtl\Macros_RTL.asm"
    
    .byte $00,$0c,$08,$0a,$00,$9e,$20,$32 // encode SYS 2064
    .byte $30,$36,$34,$00,$00,$00,$00,$00 // as BASIC line

Lab2064:
    jmp main
main:
    :PrintStringConst("hello world")
    rts
