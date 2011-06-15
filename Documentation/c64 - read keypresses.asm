; currently pressed keycode is stored to $00cb
currkey             = $cb
; vic-II control register
vicctrlreg          = $d016
; background color #0
bgcolor0            = $d021
; background color #1
bgcolor1            = $d022
; background color #2
bgcolor2            = $d023
;------------------------------------
mainloop
          jsr readk
          jmp mainloop
;------------------------------------
readk
          lda currkey
          ; if no key is pressed, it holds the value #$40
          cmp #$40
          beq readkx

          ; f1 key
          cmp #$04
          bne readkf3
          jsr tglchrst
          jmp readkx

          ; f3 key
readkf3   cmp #$05
          bne readkf5
          inc bgcolor0
          jmp readkx

          ; f5
readkf5   cmp #$06
          bne readkf7
          inc bgcolor1
          jmp readkx

          ; f7
readkf7   cmp #$03
          bne readkx
          inc bgcolor2
          jmp readkx

readkx    rts
;------------------------------------
tglchrst
          ; toggles character mode
          ; standard / multicolor

          ; Set multicolor mode
          ; (bit 4 in vic control register)

          lda vicctrlreg
          eor #$10             ; 00010000
          sta vicctrlreg

          rts
;------------------------------------
