//======================================================================
//Input a string and store it in GOTINPUT, terminated with a null byte.
//x:a is a pointer to the allowed list of characters, null-terminated.
//max # of chars in y returns num of chars entered in y.
//======================================================================

//.const GETIN = $ffe4 // commented out due to defined elsewhere

// Example usage
FILTERED_TEXT:
  lda #>TEXT
  ldx #<TEXT
  ldy #38
  //Drop through

// Main entry
FILTERED_INPUT:
  sty MAXCHARS
  stx CHECKALLOWED+1
  sta CHECKALLOWED+2

  //Zero characters received.
  lda #$00
  sta INPUT_Y

//Wait for a character.
INPUT_GET:
  jsr GETIN
  beq INPUT_GET

  sta LASTCHAR

  cmp #$14               //Delete
  beq DELETE

  cmp #$0d               //Return
  beq INPUT_DONE

  //Check the allowed list of characters.
  ldx #$00
CHECKALLOWED:
  lda $ffff,x           //Overwritten
  beq INPUT_GET         //Reached end of list (0)

  cmp LASTCHAR
  beq INPUTOK           //Match found

  //Not end or match, keep checking
  inx
  jmp CHECKALLOWED

INPUTOK:
  lda LASTCHAR          //Get the char back
  ldy INPUT_Y
  sta GOTINPUT,y        //Add it to string
  jsr CHROUT             //Print it

  inc INPUT_Y           //Next character

  //End reached?
  lda INPUT_Y
  cmp MAXCHARS
  beq INPUT_DONE

  //Not yet.
  jmp INPUT_GET

INPUT_DONE:
   ldy INPUT_Y
   lda #$00
   sta GOTINPUT,y   //Zero-terminate
   rts

// Delete last character.
DELETE:
  //First, check if we're at the beginning.  If so, just exit.
  lda INPUT_Y
  bne DELETE_OK
  jmp INPUT_GET

  //At least one character entered.
DELETE_OK:
  //Move pointer back.
  dec INPUT_Y

  //Store a zero over top of last character, just in case no other characters are entered.
  ldy INPUT_Y
  lda #$00
  sta GOTINPUT,y

  //Print the delete char
  lda #$14
  jsr CHROUT

  //Wait for next char
  jmp INPUT_GET


//=================================================
//Some example filters
//=================================================

//IPADDRESS:
//  .text "1234567890." .byte 0

TEXT:
  .text " ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890.,-+!#$%&'()*" .byte 0 

//=================================================
MAXCHARS:
  .byte $00

LASTCHAR:
  .byte $00

INPUT_Y:
  .byte $00

GOTINPUT:
  .byte 39