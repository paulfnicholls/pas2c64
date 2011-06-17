// Standard Kernal ROM routines
.const CHKIN      = $ffc6 // CHKIN. Define file as default input. (Must call OPEN beforehand.)
.const CHKOUT     = $ffc9 // CHKOUT. Define file as default output. (Must call OPEN beforehand.)
.const CHRIN      = $ffcf // CHRIN. Read byte from default input (for keyboard, read a line from the screen). (If not keyboard, must call OPEN and CHKIN beforehand.)
.const CHROUT     = $ffd2 // CHROUT. Write byte to default output. (If not screen, must call OPEN and CHKOUT beforehand.)
.const CLALL      = $ffe7 // CLALL. Clear file table; call CLRCHN.
.const CLOSE      = $ffc3 // CLOSE. Close file.
.const CLRCHN     = $ffcc // CLRCHN. Close default input/output files (for serial bus, send UNTALK and/or UNLISTEN); restore default input/output to keyboard/screen.
.const GETIN      = $ffe4 // GETIN. Read byte from default input. (If not keyboard, must call OPEN and CHKIN beforehand.)
.const IECIN      = $ffa5 // IECIN. Read byte from serial bus. (Must call TALK and TALKSA beforehand.)
.const IECOUT     = $ffa8 // IECOUT. Write byte to serial bus. (Must call LISTEN and LSTNSA beforehand.)
.const IOBASE     = $fff3 // IOBASE. Fetch CIA #1 base address.
.const IOINIT     = $ff84 // IOINIT. Initialize CIA's, SID volume; setup memory configuration; set and start interrupt timer.
.const LISTEN     = $ffb1 // LISTEN. Send LISTEN command to serial bus.
.const LOAD       = $ffd5 // LOAD. Load or verify file. (Must call SETLFS and SETNAM beforehand.)
.const LSTNSA     = $ff93 // LSTNSA. Send LISTEN secondary address to serial bus. (Must call LISTEN beforehand.)
.const MEMBOT     = $ff99 // MEMBOT. Save or restore start address of BASIC work area.
.const MEMTOP     = $ff9c // MEMTOP. Save or restore end address of BASIC work area.
.const OPEN       = $ffc0 // OPEN. Open file. (Must call SETLFS and SETNAM beforehand.)
.const PLOT       = $fff0 // PLOT. Save or restore cursor position.
.const RAMTAS     = $ff87 // RAMTAS. Clear memory addresses $0002-$0101 and $0200-$03FF; run memory test and set start and end address of BASIC work area accordingly; set screen memory to $0400 and datasette buffer to $033C.
.const RDTIM      = $ffde // RDTIM. read Time of Day, at memory address $00A0-$00A2.
.const READST     = $ffb7 // READST. Fetch status of current input/output device, value of ST variable. (For RS232, status is cleared.)
.const RESTOR     = $ff8a // RESTOR. Fill vector table at memory addresses $0314-$0333 with default values.
.const SAVE       = $ffd8 // SAVE. Save file. (Must call SETLFS and SETNAM beforehand.)
.const SCINIT     = $ff81 // SCINIT. Initialize VIC; restore default input/output to keyboard/screen; clear screen; set PAL/NTSC switch and interrupt timer.
.const SCNKEY     = $ff9f // SCNKEY. Query keyboard; put current matrix code into memory address $00CB, current status of shift keys into memory address $028D and PETSCII code into keyboard buffer.
.const SCREEN     = $ffed // SCREEN. Fetch number of screen rows and columns.
.const SETLFS     = $ffba // SETLFS. Set file parameters.
.const SETMSG     = $ff90 // SETMSG. Set system error display switch at memory address $009D.
.const SETNAM     = $ffbd // SETNAM. Set file name parameters.
.const SETTIM     = $ffdb // SETTIM. Set Time of Day, at memory address $00A0-$00A2.
.const SETTMO     = $ffa2 // SETTMO. Unknown. (Set serial bus timeout.)
.const STOP       = $ffe1 // STOP. Query Stop key indicator, at memory address $0091; if pressed, call CLRCHN and clear keyboard buffer.
.const TALK       = $ffb4 // TALK. Send TALK command to serial bus.
.const TALKSA     = $ff96 // TALKSA. Send TALK secondary address to serial bus. (Must call TALK beforehand.)
.const UDTIM      = $ffea // UDTIM. Update Time of Day, at memory address $00A0-$00A2, and Stop key indicator, at memory address $0091.
.const UNLSTN     = $ffae // UNLSTN. Send UNLISTEN command to serial bus.
.const UNTALK     = $ffab // UNTALK. Send UNTALK command to serial bus.
.const VECTOR     = $ff8d // VECTOR. Copy vector table at memory addresses $0314-$0333 from or into user table.

// floating point routines
.const MOVFM      = $bba2 // MOVFM. Move a Floating Point Number from Memory to FAC1
.const FOUT       = $bddd // FOUT. Convert Contents of FAC1 to ASCII String
.const MOV2F      = $bbc7 // MOV2F. Move a Floating Point Number from FAC1 to Memory

// interrupt routines and vectors
.const IRQVECLO   = $0314 // hardware interrupt (IRQ) vector, low byte
.const IRQVECHI   = $0315 // hardware interrupt (IRQ) vector, high byte
.const STDIRQ     = $ea31 //start address of standard interrupt routines

// keyboard
.const CURRKEY    = $cb   // currently pressed keycode is stored to $00cb; $40  = no key pressed

// VIC-II registersn
.const VICCTRLREG = $d016 // vic-II control register
.const BDRCOLOR   = $d020 // border color
.const BGCOLOR0   = $d021 // background color #0
.const BGCOLOR1   = $d022 // background color #1
.const BGCOLOR2   = $d023 // background color #2

// definitions
.const TRUE       = $ff   // boolean value True
.const FALSE      = $00   // boolean value False