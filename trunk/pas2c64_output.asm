    :BasicUpstart2(main) // 10 sys <start address>
    
    .import source "rtl\Macros_RTL.asm"
    
const_c:
    .byte $05 
var_a:
    .byte $00 $00 
var_b:
    .byte $00 
var_f:
    .byte $00,$00,$00,$00,$00
main:
    rts
