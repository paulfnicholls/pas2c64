.macro PrintStringConst(Str) {
    lda #<StrData
    ldy #>StrData
//store address in zero-page
    sta $fb
    sty $fb + 1
    ldy #$00
loop:
    lda ($fb),y
    beq continue
    jsr $ffd2
    iny
    jmp loop
StrData:
    .text Str
    .byte 0
continue:
}

.macro PrintStringAddr(Addr) {
    lda #<Addr
    ldy #>Addr
//store address in zero-page
    sta $fb
    sty $fb + 1
    ldy #$00
loop:
    lda ($fb),y
    beq continue
    jsr $ffd2
    iny
    jmp loop
continue:
}

//.macro PrintStringAY() {
//store address in zero-page
//    sta $fb
//    sty $fb + 1
//    ldy #$00
//loop:
//    lda ($fb),y
//    beq continue
//    jsr $ffd2
//    iny
//    jmp loop
//continue:
//}