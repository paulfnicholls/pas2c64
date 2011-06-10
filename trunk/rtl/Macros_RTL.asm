.macro PrintStringConst(Str) {
    :PrintStringAddr(StrData)
    jmp continue
StrData:
    .byte Str
continue:
}

.macro PrintStringAddr(Addr) {
    lda #<Addr
    ldy #>Addr
    :PrintStringAY()
}

.macro PrintStringAY() {
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