unit unit_pas2c64_constants;

{$MODE Delphi}

interface

const
// standard colours
  cColor_Black      = $00;
  cColor_White      = $01;
  cColor_Red        = $02;
  cColor_Cyan       = $03;
  cColor_Purple     = $04;
  cColor_Green      = $05;
  cColor_Blue       = $06;
  cColor_Yellow     = $07;
  cColor_Orange     = $08 ;
  cColor_Brown      = $09;
  cColor_Pink       = $0A;
  cColor_DarkGrey   = $0B;
  cColor_MediumGrey = $0C;
  cColor_LightGreen = $0D;
  cColor_LightBlue  = $0E;
  cColor_LightGrey  = $0F;

// Standard Kernal ROM routines
  SCINIT = $FF81; // SCINIT. Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
                  // Input: –
                  // Output: –
                  // Used registers: A, X, Y.
                  // Real address: $FF5B.
  IOINIT = $FF84; // IOINIT. Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
                  // Input: –
                  // Output: –
                  // Used registers: A, X.
                  // Real address: $FDA3.
  RAMTAS = $FF87; // RAMTAS. Clear memory addresses $0002-$0101 and $0200-$03FF; run memory test and set start and end address of BASIC work area accordingly; set screen memory to $0400 and datasette buffer to $033C.
                  // Input: –
                  // Output: –
                  // Used registers: A, X, Y.
                  // Real address: $FD50.
  RESTOR = $FF8A; // RESTOR. Fill vector table at memory addresses $0314-$0333 with default values.
                  // Input: –
                  // Output: –
                  // Used registers: –
                  // Real address: $FD15.
  VECTOR = $FF8D; // VECTOR. Copy vector table at memory addresses $0314-$0333 from or into user table.
                  // Input: Carry: 0 = Copy user table into vector table, 1 = Copy vector table into user table; X/Y = Pointer to user table.
                  // Output: –
                  // Used registers: A, Y.
                  // Real address: $FD1A.
  SETMSG = $FF90; // SETMSG. Set system error display switch at memory address $009D.
                  // Input: A = Switch value.
                  // Output: –
                  // Used registers: –
                  // Real address: $FE18.
  LSTNSA = $FF93; // LSTNSA. Send LISTEN secondary address to serial bus. (Must call LISTEN beforehands.)
                  // Input: A = Secondary address.
                  // Output: –
                  // Used registers: A.
                  // Real address: $EDB9.
  TALKSA = $FF96; // TALKSA. Send TALK secondary address to serial bus. (Must call TALK beforehands.)
                  // Input: A = Secondary address.
                  // Output: –
                  // Used registers: A.
                  // Real address: $EDC7.
  MEMBOT = $FF99; // MEMBOT. Save or restore start address of BASIC work area.
                  // Input: Carry: 0 = Restore from input, 1 = Save to output; X/Y = Address (if Carry = 0).
                  // Output: X/Y = Address (if Carry = 1).
                  // Used registers: X, Y.
                  // Real address: $FE25.
  MEMTOP = $FF9C; // MEMTOP. Save or restore end address of BASIC work area.
                  // Input: Carry: 0 = Restore from input, 1 = Save to output; X/Y = Address (if Carry = 0).
                  // Output: X/Y = Address (if Carry = 1).
                  // Used registers: X, Y.
                  // Real address: $FE34.
  SCNKEY = $FF9F; // SCNKEY. Query keyboard; put current matrix code into memory address $00CB, current status of shift keys into memory address $028D and PETSCII code into keyboard buffer.
                  // Input: –
                  // Output: –
                  // Used registers: A, X, Y.
                  // Real address: $EA87.
  SETTMO = $FFA2; // SETTMO. Unknown. (Set serial bus timeout.)
                  // Input: A = Timeout value.
                  // Output: –
                  // Used registers: –
                  // Real address: $FE21.
  IECIN = $FFA5;  // IECIN. Read byte from serial bus. (Must call TALK and TALKSA beforehands.)
                  // Input: –
                  // Output: A = Byte read.
                  // Used registers: A.
                  // Real address: $EE13.
  IECOUT = $FFA8; // IECOUT. Write byte to serial bus. (Must call LISTEN and LSTNSA beforehands.)
                  // Input: A = Byte to write.
                  // Output: –
                  // Used registers: –
                  // Real address: $EDDD.
  UNTALK = $FFAB; // UNTALK. Send UNTALK command to serial bus.
                  // Input: –
                  // Output: –
                  // Used registers: A.
                  // Real address: $EDEF.
  UNLSTN = $FFAE; // UNLSTN. Send UNLISTEN command to serial bus.
                  // Input: –
                  // Output: –
                  // Used registers: A.
                  // Real address: $EDFE.
  LISTEN = $FFB1; // LISTEN. Send LISTEN command to serial bus.
                  // Input: A = Device number.
                  // Output: –
                  // Used registers: A.
                  // Real address: $ED0C.
  TALK = $FFB4;   // TALK. Send TALK command to serial bus.
                  // Input: A = Device number.
                  // Output: –
                  // Used registers: A.
                  // Real address: $ED09.
  READST = $FFB7; // READST. Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
                  // Input: –
                  // Output: A = Device status.
                  // Used registers: A.
                  // Real address: $FE07.
  SETLFS = $FFBA; // SETLFS. Set file parameters.
                  // Input: A = Logical number; X = Device number; Y = Secondary address.
                  // Output: –
                  // Used registers: –
                  // Real address: $FE00.
  SETNAM = $FFBD; // SETNAM. Set file name parameters.
                  // Input: A = File name length; X/Y = Pointer to file name.
                  // Output: –
                  // Used registers: –
                  // Real address: $FDF9.
  OPEN = $FFC0;   // OPEN. Open file. (Must call SETLFS and SETNAM beforehands.)
                  // Input: –
                  // Output: –
                  // Used registers: A, X, Y.
                  // Real address: ($031A), $F34A.
  CLOSE = $FFC3;  // CLOSE. Close file.
                  // Input: A = Logical number.
                  // Output: –
                  // Used registers: A, X, Y.
                  // Real address: ($031C), $F291.
  CHKIN = $FFC6;  // CHKIN. Define file as default input. (Must call OPEN beforehands.)
                  // Input: X = Logical number.
                  // Output: –
                  // Used registers: A, X.
                  // Real address: ($031E), $F20E.
  CHKOUT = $FFC9; // CHKOUT. Define file as default output. (Must call OPEN beforehands.)
                  // Input: X = Logical number.
                  // Output: –
                  // Used registers: A, X.
                  // Real address: ($0320), $F250.
  CLRCHN = $FFCC; // CLRCHN. Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
                  // Input: –
                  // Output: –
                  // Used registers: A, X.
                  // Real address: ($0322), $F333.
  CHRIN = $FFCF;  // CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehands.)
                  // Input: –
                  // Output: A = Byte read.
                  // Used registers: A, Y.
                  // Real address: ($0324), $F157.
  CHROUT = $FFD2; // CHROUT. Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehands.)
                  // Input: A = Byte to write.
                  // Output: –
                  // Used registers: –
                  // Real address: ($0326), $F1CA.
  LOAD = $FFD5;   // LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehands.)
                  // Input: A: 0 = Load, 1-255 = Verify; X/Y = Load address (if secondary address = 0).
                  // Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1); X/Y = Address of last byte loaded/verified (if Carry = 0).
                  // Used registers: A, X, Y.
                  // Real address: $F49E.
  SAVE = $FFD8;   // SAVE. Save file. (Must call SETLFS and SETNAM beforehands.)
                  // Input: A = Address of zero page register holding start address of memory area to save; X/Y = End address of memory area plus 1.
                  // Output: Carry: 0 = No errors, 1 = Error; A = KERNAL error code (if Carry = 1).
                  // Used registers: A, X, Y.
                  // Real address: $F5DD.
  SETTIM = $FFDB; // SETTIM. Set Time of Day, at memory address $00A0-$00A2.
                  // Input: A/X/Y = New TOD value.
                  // Output: –
                  // Used registers: –
                  // Real address: $F6E4.
  RDTIM = $FFDE;  // RDTIM. read Time of Day, at memory address $00A0-$00A2.
                  // Input: –
                  // Output: A/X/Y = Current TOD value.
                  // Used registers: A, X, Y.
                  // Real address: $F6DD.
  STOP = $FFE1;   // STOP. Query Stop key indicator, at memory address $0091; if pressed, call CLRCHN and clear keyboard buffer.
                  // Input: –
                  // Output: Zero: 0 = Not pressed, 1 = Pressed; Carry: 1 = Pressed.
                  // Used registers: A, X.
                  // Real address: ($0328), $F6ED.
  GETIN = $FFE4;  // GETIN. Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehands.)
                  // Input: –
                  // Output: A = Byte read.
                  // Used registers: A, X, Y.
                  // Real address: ($032A), $F13E.
  CLALL = $FFE7;  // CLALL. Clear file table; call CLRCHN.
                  // Input: –
                  // Output: –
                  // Used registers: A, X.
                  // Real address: ($032C), $F32F.
  UDTIM = $FFEA;  // UDTIM. Update Time of Day, at memory address $00A0-$00A2, and Stop key indicator, at memory address $0091.
                  // Input: –
                  // Output: –
                  // Used registers: A, X.
                  // Real address: $F69B.
  SCREEN = $FFED; // SCREEN. Fetch number of screen rows and columns.
                  // Input: –
                  // Output: X = Number of columns (40); Y = Number of rows (25).
                  // Used registers: X, Y.
                  // Real address: $E505.
  PLOT = $FFF0;   // PLOT. Save or restore cursor position.
                  // Input: Carry: 0 = Restore from input, 1 = Save to output; X = Cursor column (if Carry = 0); Y = Cursor row (if Carry = 0).
                  // Output: X = Cursor column (if Carry = 1); Y = Cursor row (if Carry = 1).
                  // Used registers: X, Y.
                  // Real address: $E50A.
  IOBASE = $FFF3; // IOBASE. Fetch CIA #1 base address.
                  // Input: –
                  // Output: X/Y = CIA #1 base address ($DC00).
                  // Used registers: X, Y.
                  // Real address: $E500.

  // floating point routines

  MOVFM = $BBA2;  {
                  Move a Floating Point Number from Memory to FAC1

                  This routine loads FAC1 with the five-byte floating point number
                  pointed to by the address stored in the Accumulator (low byte) and the
                  .Y register (high byte).}

  MOV2F = $BBC7;  {
                  Move a Floating Point Number from FAC1 to Memory

                  This routine is used to move a number from the Floating Point
                  Accumulator (FAC1) to memory at either 92-96 ($5C-$60) or 87-91
                  ($57-$5B), depending on the entry point to the routine.}

  FOUT = $BDDD;   {
                  Convert Contents of FAC1 to ASCII String

                  This routine converts a floating point number to a string of ASCII
                  digits, and sets a pointer to the string in .A and .Y.}

implementation

end.
