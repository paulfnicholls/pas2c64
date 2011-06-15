; http://xabreman.wordpress.com/2011/01/02/6510-asm-simple-interrupt-example/

; hardware interrupt (IRQ) vector, low byte
irqveclo  = $0314
; hardware interrupt (IRQ) vector, high byte
irqvechi  = $0315
; start address of standard interrupt routines
stdirq    = $ea31

          *= $1000  ; sys 4096

          sei       ; disable interrupts

          ; set a custom interrupt routine address to interrupt vector

          lda #<irqtest       ; low byte of irqtest start addr
          ldx #>irqtest       ; high byte of irqtest start addr
          sta irqveclo
          stx irqvechi 

          cli       ; clear interrupt disable bit

          rts

; interrupt routine
irqtest
          ; increase the border color
          inc $d020
          ; continue with standard interrupt routines
          jmp stdirq
