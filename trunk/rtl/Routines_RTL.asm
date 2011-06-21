KeyCode:
    .byte $00

//------------------------------------
//  procedure WaitForKey - waits for any key to be pressed and then returns
//------------------------------------
//  input: none
//  output: none
//------------------------------------
asm_waitforkey:
    pha
WaitForKeyLoop:
    lda CURRKEY
    // if no key is pressed, it holds the value #$40
    cmp #$40
    beq WaitForKeyLoop
    // clear the last key
    lda #0
    sta CURRKEY
    pla
    rts

//------------------------------------
// function KeyIsDown - return True if key in A is down
//------------------------------------
//  input: A = key code to check is down
//  output: A = $00 for false, $ff for true
//------------------------------------
KeyIsDown:
    sta KeyCode

    lda CURRKEY
    cmp KeyCode
    beq KeyIsDown_True // key is down so return true
    lda FALSE
    rts
KeyIsDown_True:
    lda TRUE
    rts

//------------------------------------
// function KeyIsUp - return True if key in A is up
//------------------------------------
//  input: A = key code to check is up
//  output: A = $00 for false, $ff for true
//------------------------------------
KeyIsUp:
    sta KeyCode

    lda CURRKEY
    cmp KeyCode
    beq KeyIsUp_False // key is down so return false
    lda TRUE
    rts
KeyIsUp_False:
    lda FALSE
    rts

//------------------------------------
// procedure PrintStringAY - prints string pointed to by A/Y
//------------------------------------
//  input: A = pointer low byte, Y = pointer hi byte
//------------------------------------
PrintStringAY:
//store address in zero-page
    sta $fb
    sty $fb + 1
    ldy #$00
PrintStringAY_loop:
    lda ($fb),y
    beq PrintStringAY_continue
    jsr $ffd2
    iny
    jmp PrintStringAY_loop
PrintStringAY_continue:
    rts

//------------------------------------
// procedure SwitchToUpperCase - switch charset to upper case only
//------------------------------------
asm_switchtouppercase:
    pha
    lda $d018
    and #253 // clear bit 2
    sta $d018
    pla
    rts

//------------------------------------
// procedure SwitchToLowerCase - switch charset to lower & upper case
//------------------------------------
asm_switchtolowercase:
    pha
    lda $d018
    ora #2 // set bit 2
    sta $d018
    pla
    rts

//------------------------------------
// procedure ToggleCase - switch charset to upper case only
//------------------------------------
asm_togglecase:
    pha
    lda $d018
    eor #2 // toggle bit 2
    sta $d018
    pla
    rts    