program Interrupt_Test;

procedure FlashBorderIRQ; interrupt;
begin
  IncMemB(BDRCOLOR);
  StdIRQ;
end;

procedure Init;
begin
  SetInterrupt(FlashBorderIRQ);
end;

begin
  Init;
end.
