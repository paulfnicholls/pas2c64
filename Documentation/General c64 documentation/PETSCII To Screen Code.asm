;================================================================================
; 
;CONVERT CBM PETSCII to SCREEN CODE 
; 
;   ---------------------- 
;   .A = PETSCII character 
;   ---------------------- 
;   .A = screen code 
;   .X = entry value 
;   .Y = entry value 
;   ---------------------- 
; 
;   Conversion rules: 
; 
;   PETSCII Range      Input          Output      Mask 
;   ---------------------------------------------------- 
;   Low Control      $00 - $1F ---> $00 - $1F 
;   Punc & Numerals  $20 - $3F ---> $20 - $3F   00111111 
;   Alpha LC         $40 - $5F ---> $00 - $1F   00111111 
;   Alt Alpha LC     $60 - $7F ---> $40 - $5F   01011111 
;   High Control     $80 - $9F ---> $80 - $9F 
;   Alpha UC         $C0 - $FE ---> $40 - $FE   01111111 
;   Pi               $FF ---------> $5E 
;   ---------------------------------------------------- 
; 
petcodcl =$1f                  ;low PETSCII control range end 
petcodch =$9f                  ;high PETSCII control range end 
petcodl  =$60                  ;regular LC PETSCII range end 
petcodm  =$80                  ;alternate LC PETSCII range end 
petcodp  =$7f                  ;pi PETSCII test value 
petcodps =$5e                  ;pi screen code 
pschmask =%01111111            ;PETSCII to screen code high-range 
psclmask =%10111111            ;PETSCII to screen code mid-range 
pscmmask =%11011111            ;PETSCII to screen code low-range 
scblank  =$20                  ;blank char screen code 
; 
petscode cmp #petcodcl+1       ;low control code end 
         bcc petcod03          ;skip conversion 
;            
         cmp #petcodl          ;regular LC range end 
         bcs petcod01          ;try next 
; 
         and #psclmask         ;change to LC screen code 
         rts                   ;done 
; 
petcod01 cmp #petcodm          ;alternate LC range 
         bcs petcod02          ;UC range or pi 
; 
         and #pscmmask 
         rts 
; 
petcod02 cmp #petcodch+1       ;high control code end 
         bcc petcod03          ;no conversion 
; 
         and #pschmask 
         cmp #petcodp          ;is it pi? 
         bne petcod03          ;no, so return it unchanged 
; 
         lda #petcodps         ;pi screen code 
; 
petcod03 rts 
;