*
* Note that seed may be initialized from BASIC
*

* CBM stuff:

ARG       = $6A           ;Floating point acc2 & rnd seed
SEED      = $8B           ;used by included rnd # genr

* Macros

ADD       MAC             ;Used in RND routine
          LDA ARG+]1
          ADC SEED+]1
          STA ARG+]1
          <<<

ROT       MAC             ;Used in RND routine
          LDA SEED+]1
          ROL
          STA ARG+]1
          <<<

*-----------------------------------------
* A good and fast random # generator.
* The BASIC RND does not deserve the name.
* Rnd # held in SEED (32 bits), ARG used
* for scratch pad.  Preserves X, Y
* Returns msb in A.
*-----------------------------------------

RND       LDA SEED+4
          ASL
          STA ARG+4
          ROT 3
          ROT 2
          ROT 1
          SEC
          ROL ARG+4
          ROL ARG+3
          ROL ARG+2
          ROL ARG+1
          CLC
          ADD 4
          PHA
          ADD 3
          PHA
          ADD 2
          ADD 1
          CLC
          LDA ARG+2
          ADC SEED+4
          STA SEED+2
          LDA ARG+1
          ADC SEED+3
          STA SEED+1
          PLA
          STA SEED+3
          PLA
          STA SEED+4
          LDA SEED+1      ;Most signif byte
          RTS