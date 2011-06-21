procedure UpperCase; assembler;
asm
    pha
    lda $d018
    and #253  // clear bit 2
    sta $d018
    pla
    rts
end;

begin
end.
