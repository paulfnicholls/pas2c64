procedure UpperCase; assembler;
asm
  lda $d018
  and #253
  sta $d018
end;

procedure LowerCase; assembler;
asm
  lda $d018
  ora #2
  sta $d018
end;

begin
  LowerCase;
  UpperCase;
end.
