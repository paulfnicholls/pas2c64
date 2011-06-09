    .org $0800 ; start at BASIC
    .byte $00 $0c $08 $0a $00 $9e $20 $32 ; encode SYS 2064
    .byte $30 $36 $34 $00 $00 $00 $00 $00 ; as BASIC line

Lab2064
    JMP main
main
;load floating point number into FAC1
    LDA #<L1
    LDY #>L1
    JSR $BBA2
    
;convert number in FAC1 to ASCII (pointer in A & Y)
    JSR $BDDD
    
;store address in zero-page
    STA $FB
    STY $FB + 1
    LDY #$00
L0
    LDA ($FB),Y
    BEQ L3
    JSR $FFD2
    INY
    JMP L0
L1
    .byte $87 $4C $00 $00 $00
L2
    .byte 0,0
L3
    RTS
