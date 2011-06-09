    .org $0800 ; start at BASIC
    .byte $00 $0c $08 $0a $00 $9e $20 $32 ; encode SYS 2064
    .byte $30 $36 $34 $00 $00 $00 $00 $00 ; as BASIC line

Lab2064
    JMP main
main
;load floating point number into FAC1
    LDA <L1
    LDY >L1
    JSR $BBA2
    
;convert number in FAC1 to ASCII (pointer in A & Y)
    JSR $BDDD
    
;How to print string at location in A & Y to screen using $FFD2?
;$FFD2 expects character in A?
    
    JMP L2
L1
    .byte "$87 $48 $00 $00 $00",0
L2
    RTS
