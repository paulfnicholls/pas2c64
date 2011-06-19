// math registers

// integer registers
ireg0: .word 0
ireg1: .word 0
ireg2: .word 0
ireg3: .word 0

// float registers
freg0: .word 0
freg1: .word 0
freg2: .word 0
freg3: .word 0

// string registers
// length, 255 characters + 1 null byte maximum
.byte 0 sreg0: .fill 256, 0
.byte 0 sreg1: .fill 256, 0
.byte 0 sreg2: .fill 256, 0
.byte 0 sreg3: .fill 256, 0

//
.macro Addii ira,irb,irr {

}