unit unit_pas2c64_CodeGenerator;

{$MODE Delphi}

interface

uses
  SysUtils,
  Classes;

const
  cTAB           = AnsiChar(#9);
  cCRLF          = AnsiString(#13#10);
  cLabelIndent   = 0;
  cCommentIndent = 0;
  cCodeIndent    = 4;
  cIndentChar    = AnsiChar(' ');

type
  C64OpException = Class(Exception);

  TRegister = (
    regA,
    regX,
    regY
  );

  TOpCode = (
    opADC,
    opAND,
    opASL,
    opBCC,
    opBCS,
    opBEQ,
    opBIT,
    opBMI,
    opBNE,
    opBPL,
    opBRK,
    opBVC,
    opBVS,
    opCLC,
    opCLD,
    opCLI,
    opCLV,
    opCMP,
    opCPX,
    opCPY,
    opDEC,
    opDEX,
    opDEY,
    opEOR,
    opINC,
    opINX,
    opINY,
    opJMP,
    opJSR,
    opLDA,
    opLDX,
    opLDY,
    opLSR,
    opNOP,
    opORA,
    opPHA,
    opPHP,
    opPLA,
    opPLP,
    opROL,
    opROR,
    opRTI,
    opRTS,
    opSBC,
    opSEC,
    opSED,
    opSEI,
    opSTA,
    opSTX,
    opSTY,
    opTAX,
    opTAY,
    opTSX,
    opTXA,
    opTXS,
    opTYA
  );

  TMode = (
    modeA,     // OPC A
    modeAbs,   // OPC $HHLL
    modeAbsX,  // OPC $HHLL,X
    modeAbsY,  // OPC $HHLL,Y
    modeImmed, // OPC #$BB
    modeImpl,  // OPC
    modeInd,   // OPC ($HHLL)
    modeXInd,  // OPC ($BB,X)
    modeIndY,  // OPC ($LL),Y
    modeRel,   // OPC $BB
    modeZpg,   // OPC $LL
    modeZpgX,  // OPC $LL,X
    modeZpgY   // OPC $LL,Y
  );

const
  cRegisterName : array[TRegister] of AnsiString = (
    'a',
    'x',
    'y'
  );

  cOpCodeName : array[TOpCode] of AnsiString = (
    'adc',
    'and',
    'asl',
    'bcc',
    'bcs',
    'beq',
    'bit',
    'bmi',
    'bne',
    'bpl',
    'brk',
    'bvc',
    'bvs',
    'clc',
    'cld',
    'cli',
    'clv',
    'cmp',
    'cpx',
    'cpy',
    'dec',
    'dex',
    'dey',
    'eor',
    'inc',
    'inx',
    'iny',
    'jmp',
    'jsr',
    'lda',
    'ldx',
    'ldy',
    'lsr',
    'nop',
    'ora',
    'pha',
    'php',
    'pla',
    'plp',
    'rol',
    'ror',
    'rti',
    'rts',
    'sbc',
    'sec',
    'sed',
    'sei',
    'sta',
    'stx',
    'sty',
    'tax',
    'tay',
    'tsx',
    'txa',
    'txs',
    'tya'
  );

  cLoadRegIm = [regA,regX,regY];
  cLoadReg   = [regA,regX,regY];
  cStoreReg  = [regA,regX,regY];

{
A		   ..	Accumulator	 	      OPC A	 	    operand is AC
abs		 ..	absolute	 	        OPC $HHLL	 	operand is address $HHLL
abs,X	 ..	absolute, X-indexed	OPC $HHLL,X	operand is address incremented by X with carry
abs,Y	 ..	absolute, Y-indexed	OPC $HHLL,Y	operand is address incremented by Y with carry
#		   ..	immediate	 	        OPC #$BB	 	operand is byte (BB)
impl	 ..	implied	 	          OPC	 	      operand implied
ind		 ..	indirect	 	        OPC ($HHLL)	operand is effective address; effective address is value of address
X,ind	 ..	X-indexed, indirect	OPC ($BB,X)	operand is effective zeropage address; effective address is byte (BB) incremented by X without carry
ind,Y	 ..	indirect, Y-indexed	OPC ($LL),Y	operand is effective address incremented by Y with carry; effective address is word at zeropage address
rel		 ..	relative	 	        OPC $BB	 	  branch target is PC + offset (BB), bit 7 signifies negative offset
zpg		 ..	zeropage	 	        OPC $LL	 	  operand is of address; address hibyte = zero ($00xx)
zpg,X	 ..	zeropage, X-indexed	OPC $LL,X	 	operand is address incremented by X; address hibyte = zero ($00xx); no page transition
zpg,Y	 ..	zeropage, Y-indexed	OPC $LL,Y	 	operand is address incremented by Y; address hibyte = zero ($00xx); no page transition
}

type
  TCodeGenerator_C64 = class
  private
    FOutputStream: TStream;
    FLabelIndex: Integer;
  public
    constructor Create;
    destructor  Destroy; override;

    // output control
    procedure SetOutputStream(const aStream: TStream);

    // code generation
    procedure ResetLabels;
    function  NewLabel: AnsiString;

    procedure WriteOutput(const aStr: AnsiString);
    procedure WriteOutputCR(aStr: AnsiString = '');
    procedure WriteLabel(const aStr: AnsiString);
    procedure WriteComment(const aStr: AnsiString);
    procedure WriteCode(const aStr: AnsiString);
    procedure WriteOrigin(const aAddr: Word);

    // used with no basic loader
    procedure WriteProgramStart(const aCodeAddr: Word); overload;
    // used with a basic loader
    procedure WriteProgramStart;                        overload;

    procedure WriteMainStart;

    procedure LoadReg_IM(const aReg: TRegister; const aConstValue: Byte);
    procedure LoadRegW_IM(const aLoReg,aHiReg: TRegister; const aConstValue: Word);
    procedure LoadReg_Mem(const aReg: TRegister; const aAddr: Word);
    procedure StoreReg(const aReg: TRegister; const aAddr: Word);
    procedure OpCode(const aOpCode: TOpCode);
    procedure SwapRegisters(const aSrcReg,aDstReg: TRegister);

    procedure OpA(const aOpCode: TOpCode);                         // OPC A
    procedure OpAbs(const aOpCode: TOpCode; const aAddr: Word);    // OPC $HHLL
    procedure OpAbsX(const aOpCode: TOpCode; const aAddr: Word);   // OPC $HHLL,X
    procedure OpAbsY(const aOpCode: TOpCode; const aAddr: Word);   // OPC $HHLL,Y
    procedure OpImmed(const aOpCode: TOpCode; const aValue: Byte); // OPC #$BB
    procedure OpImpl(const aOpCode: TOpCode);                      // OPC
    procedure OpInd(const aOpCode: TOpCode; const aAddr: Word);    // OPC ($HHLL)
    procedure OpXInd(const aOpCode: TOpCode; const aAddr: Byte);   // OPC ($BB,X)
    procedure OpIndY(const aOpCode: TOpCode; const aAddr: Byte);   // OPC ($LL),Y
    procedure OpRel(const aOpCode: TOpCode; const aAddr: Byte);    // OPC $BB
    procedure OpZpg(const aOpCode: TOpCode; const aAddr: Byte);    // OPC $LL
    procedure OpZpgX(const aOpCode: TOpCode; const aAddr: Byte);   // OPC $LL,X
    procedure OpZpgY(const aOpCode: TOpCode; const aAddr: Byte);   // OPC $LL,Y
  end;

  TC64Mantissa = array[0..3] of Byte;
  PC64MemFloat = ^TC64MemFloat;
  TC64MemFloat = packed record
    Exponent: Byte;
    Mantissa: TC64Mantissa;
  end;

  PC64RegFloat = ^TC64RegFloat;
  TC64RegFloat = packed record
    Exponent: Byte;
    Mantissa: TC64Mantissa;
    Sign: Byte;
  end;

procedure FloatToC64Float(num: Double; out aC64Float: TC64MemFloat); overload;
procedure FloatToC64Float(num: Double; out aC64Float: TC64RegFloat); overload;
function  C64FloatToStr(var aC64Float: TC64MemFloat): String; overload;
function  C64FloatToStr(var aC64Float: TC64RegFloat): String; overload;
function  C64MemFloat(const aExponent,aMan0,aMan1,aMan2,aMan3: Byte): TC64MemFloat;
function  C64RegFloat(const aExponent,aMan0,aMan1,aMan2,aMan3,aSign: Byte): TC64RegFloat;
function  C64FloatToFloat(var aC64Float: TC64MemFloat): Double; overload;
function  C64FloatToFloat(var aC64Float: TC64RegFloat): Double; overload;

implementation

procedure FloatToC64Float(num: Double; out aC64Float: TC64MemFloat);
// converts a floating point number to 5-byte memory FP representation: exponent (1), mantissa (4)
//ftp://n2dvm.com/Commodore/Commie-CDs/Kazez%20FREE-CD/c64-knowledge-base/197.htm
var
  ExpCount: Integer;
  SignBit: Integer;
  Index: Integer;
begin
  Write(Format('%.10f = ',[num]));

  // save sign bit
  SignBit := 0;
  if num < 0 then
  begin
    SignBit := 128;
    num := -num;
  end;

  if Abs(num) < 0.000001 then
  // if input is zero, set output
  begin
    aC64Float.Exponent    := 0;
    aC64Float.Mantissa[0] := 0;
    aC64Float.Mantissa[1] := 0;
    aC64Float.Mantissa[2] := 0;
    aC64Float.Mantissa[3] := 0;
    Exit;
  end;

  // calculate exponent byte
  ExpCount := 0;
  if num < 1 then
    while num < 1 do
    begin
      Dec(ExpCount);
      num := num * 2;
    end
  else
  if num >= 2 then
    while num >= 2 do
    begin
      Inc(ExpCount);
      num := num / 2;
    end;
  aC64Float.Exponent := 129 + ExpCount;

  num := num / 2; // 'un-normalize' it for forther processing (immediate mantissa)

  // calculate mantissa digits
  for Index := 0 to 3 do
  begin
    num := num * 256;
    aC64Float.Mantissa[Index] := Trunc(num);
    num := Frac(num);
  end;

  // round last mantissa digit when required
  if num > 0.5 then Inc(aC64Float.Mantissa[3]);

  // include sign bit in first mantissa digit
  aC64Float.Mantissa[0] := (aC64Float.Mantissa[0] and $7F) or SignBit;
end;

procedure FloatToC64Float(num: Double; out aC64Float: TC64RegFloat);
// converts a floating point number to 6-byte register FP representation: exponent (1), mantissa (4), separate sign (1)
//ftp://n2dvm.com/Commodore/Commie-CDs/Kazez%20FREE-CD/c64-knowledge-base/197.htm
var
  ExpCount: Integer;
  SignBit: Integer;
  Index: Integer;
begin
  Write(Format('%.10f = ',[num]));

  // save sign bit
  SignBit := 0;
  if num < 0 then
  begin
    SignBit := 128;
    num := -num;
  end;

  if Abs(num) < 0.000001 then
  begin
    // if input is zero, set output
    aC64Float.Exponent    := 0;
    aC64Float.Mantissa[0] := 0;
    aC64Float.Mantissa[1] := 0;
    aC64Float.Mantissa[2] := 0;
    aC64Float.Mantissa[3] := 0;
    aC64Float.Sign        := 0;
    Exit;
  end;

  // calculate exponent byte
  ExpCount := 0;
  if num < 1 then
    while num < 1 do
    begin
      Dec(ExpCount);
      num := num * 2;
    end
  else
  if num >= 2 then
    while num >= 2 do
    begin
      Inc(ExpCount);
      num := num / 2;
    end;
  aC64Float.Exponent := 129 + ExpCount;

  num := num / 2; // 'un-normalize' it for forther processing (immediate mantissa)

  // calculate mantissa digits
  for Index := 0 to 3 do
  begin
    num := num * 256;
    aC64Float.Mantissa[Index] := Trunc(num);
    num := Frac(num);
  end;

  // round last mantissa digit when required
  if num > 0.5 then Inc(aC64Float.Mantissa[3]);

  // include sign bit in sign part
  aC64Float.Mantissa[4] := SignBit;
end;

function  C64MemFloat(const aExponent,aMan0,aMan1,aMan2,aMan3: Byte): TC64MemFloat;
begin
  Result.Exponent    := aExponent;
  Result.Mantissa[0] := aMan0;
  Result.Mantissa[1] := aMan1;
  Result.Mantissa[2] := aMan2;
  Result.Mantissa[3] := aMan3;
end;

function  C64RegFloat(const aExponent,aMan0,aMan1,aMan2,aMan3,aSign: Byte): TC64RegFloat;
begin
  Result.Exponent    := aExponent;
  Result.Mantissa[0] := aMan0;
  Result.Mantissa[1] := aMan1;
  Result.Mantissa[2] := aMan2;
  Result.Mantissa[3] := aMan3;
  Result.Sign        := aSign;
end;

function  C64FloatToFloat(var aC64Float: TC64MemFloat): Double; overload;
const
  cSignBit = 128;
var
  Sign: Integer;
  Mult: Integer;
  Mantissa: TC64Mantissa;
  Index: Integer;
begin
  Result := 0;

  if aC64Float.Exponent = 0 then Exit;

  Mult := 1 shl Abs(aC64Float.Exponent - 129);

  Mantissa := aC64Float.Mantissa;

  // get sign bit and convert #1 mantissa digit if required
  Sign := -1;
  if (Mantissa[0] and cSignBit) = 0 then
  begin
    Sign := 1;
    Mantissa[0] := Mantissa[0] or cSignBit;
  end;

  for Index := 0 to 3 do
    Result := (Result + Mantissa[Index]) / 256;

  if aC64Float.Exponent >= 129 then
    Result := Result * 2 * Sign * Mult
  else
    Result := Result * 2 * Sign * 1/Mult;
end;

function  C64FloatToFloat(var aC64Float: TC64RegFloat): Double; overload;
const
  cSignBit = 128;
var
  Sign: Integer;
  Mult: Integer;
  Mantissa: TC64Mantissa;
  Index: Integer;
begin
  Result := 0;

  if aC64Float.Exponent = 0 then Exit;

  Mult := 1 shl Abs(aC64Float.Exponent - 129);

  Mantissa := aC64Float.Mantissa;

  // get sign bit
  Sign := -1;
  if (aC64Float.Sign and cSignBit) = 0 then
    Sign := 1;

  for Index := 0 to 3 do
    Result := (Result + Mantissa[Index]) / 256;

  if aC64Float.Exponent >= 129 then
    Result := Result * 2 * Sign * Mult
  else
    Result := Result * 2 * Sign * {1/}Mult;
end;

function  C64FloatToStr(var aC64Float: TC64MemFloat): String;
begin
  //output C64 mem floating point as hex (Exponent, Mantissa)
  Result := LowerCase(Format('$%.2x,$%.2x,$%.2x,$%.2x,$%.2x',// (mem FP)',
                 [aC64Float.Exponent,
                  aC64Float.Mantissa[0],
                  aC64Float.Mantissa[1],
                  aC64Float.Mantissa[2],
                  aC64Float.Mantissa[3]]));
end;

function  C64FloatToStr(var aC64Float: TC64RegFloat): String;
begin
  //output C64 reg floating point as hex (Exponent, Mantissa, Sign)
  Result := LowerCase(Format('$%.2x,$%.2x,$%.2x,$%.2x,$%.2x,$%.2x',// (reg FP)',
                 [aC64Float.Exponent,
                  aC64Float.Mantissa[0],
                  aC64Float.Mantissa[1],
                  aC64Float.Mantissa[2],
                  aC64Float.Mantissa[3],
                  aC64Float.Sign]));
end;

constructor TCodeGenerator_C64.Create;
begin
  inherited Create;

  FOutputStream := nil;
  ResetLabels;
end;

destructor  TCodeGenerator_C64.Destroy;
begin
  inherited Destroy;
end;

procedure TCodeGenerator_C64.ResetLabels;
begin
  FLabelIndex := 0;
end;

function  TCodeGenerator_C64.NewLabel: AnsiString;
begin
  Result := Format('L%d',[FLabelIndex]);
  Inc(FLabelIndex);
end;

procedure TCodeGenerator_C64.SetOutputStream(const aStream: TStream);
begin
  FOutputStream := aStream;
end;

procedure TCodeGenerator_C64.WriteOutput(const aStr: AnsiString);
begin
  if not Assigned(FOutputStream) then Exit;
  if aStr = '' then Exit;

  FOutputStream.Write(PWideChar(aStr)^,Length(aStr));
end;

procedure TCodeGenerator_C64.WriteOutputCR(aStr: AnsiString = '');
begin
  if not Assigned(FOutputStream) then Exit;
  aStr := aStr + #13#10;

  FOutputStream.Write(PWideChar(aStr)^,Length(aStr));
end;

procedure TCodeGenerator_C64.WriteLabel(const aStr: AnsiString);
begin
  WriteOutputCR(StringOfChar(cIndentChar,cLabelIndent) + aStr + ':');
end;

procedure TCodeGenerator_C64.WriteComment(const aStr: AnsiString);
begin
  WriteOutputCR(StringOfChar(cIndentChar,cCommentIndent) + '//' + aStr);
end;

procedure TCodeGenerator_C64.WriteCode(const aStr: AnsiString);
begin
  WriteOutputCR(StringOfChar(cIndentChar,cCodeIndent) + aStr);
end;

procedure TCodeGenerator_C64.WriteOrigin(const aAddr: Word);
begin
  WriteOutputCR(StringOfChar(cIndentChar,cCodeIndent) + '.pc = $' + LowerCase(IntToHex(aAddr,4)));
end;

procedure TCodeGenerator_C64.WriteProgramStart;
begin
  WriteCode(':BasicUpstart2(main) // 10 sys <start address>');
  WriteCode('');
  WriteCode('');
{  WriteCode('.pc = $0800 // start at BASIC');
  WriteCode('');
  WriteCode('.import source "rtl\Macros_RTL.asm"');
  WriteCode('');
  WriteCode('.byte $00,$0c,$08,$0a,$00,$9e,$20,$32 // encode SYS 2064');
  WriteCode('.byte $30,$36,$34,$00,$00,$00,$00,$00 // as BASIC line');
  WriteOutputCR;
  WriteLabel('Lab2064');
  WriteCode('jmp start');}
end;

procedure TCodeGenerator_C64.WriteProgramStart(const aCodeAddr: Word);
begin
  WriteOrigin(aCodeAddr);
  WriteCode('');
  WriteCode('jmp main');
end;

procedure TCodeGenerator_C64.WriteMainStart;
begin
  WriteLabel('main');
end;

procedure TCodeGenerator_C64.LoadReg_IM(const aReg: TRegister; const aConstValue: Byte);
begin
  if not(aReg in cLoadRegIM) then
    raise C64OpException.Create('LoadReg_IM: Invalid register "'+cRegisterName[aReg]+'"');

  WriteCode(LowerCase(Format('ld%s #$%.2x',[cRegisterName[aReg],aConstValue])));
end;

procedure TCodeGenerator_C64.LoadRegW_IM(const aLoReg,aHiReg: TRegister; const aConstValue: Word);
begin
  if not (aLoReg in cLoadRegIM) or not (aHiReg in cLoadRegIM) then
    raise C64OpException.Create('LoadRegW_IM: Invalid register(s) '+Format('"%s, %s"',[cRegisterName[aLoReg]+cRegisterName[aHiReg]]));

  WriteCode(LowerCase(Format('ld%s <#$%.4x',[cRegisterName[aLoReg],aConstValue])));
  WriteCode(LowerCase(Format('ld%s >#$%.4x',[cRegisterName[aHiReg],aConstValue])));
end;

procedure TCodeGenerator_C64.LoadReg_Mem(const aReg: TRegister; const aAddr: Word);
begin
  if not(aReg in cLoadReg) then
    raise C64OpException.Create('LoadReg_Mem: Invalid register "'+cRegisterName[aReg]+'"');

  WriteCode(LowerCase(Format('ld%s $%.4x',[cRegisterName[aReg],aAddr])));
end;

procedure TCodeGenerator_C64.StoreReg(const aReg: TRegister; const aAddr: Word);
begin
  if not(aReg in cStoreReg) then
    raise C64OpException.Create('StoreReg: Invalid register "'+cRegisterName[aReg]+'"');

  if aAddr <= 255 then
    WriteCode(LowerCase(Format('st%s $%.2x',[cRegisterName[aReg],aAddr])))
  else
    WriteCode(LowerCase(Format('st%s $%.4x',[cRegisterName[aReg],aAddr])));
end;

procedure TCodeGenerator_C64.OpCode(const aOpCode: TOpCode);
begin
  WriteCode(cOpCodeName[aOpCode]);
end;

procedure TCodeGenerator_C64.SwapRegisters(const aSrcReg,aDstReg: TRegister);
var
  IllegalRegs: Boolean;
begin
  IllegalRegs := (aSrcReg = aDstReg) or ((aSrcReg <> regA) and (aDstReg <> regA));

  if IllegalRegs then raise C64OpException.Create('SwapRegisters: Invalid registers "'+cRegisterName[aSrcReg] + '&' + cRegisterName[aDstReg] +'"');

  WriteCode('t' + cRegisterName[aSrcReg] + cRegisterName[aDstReg]);
end;

procedure TCodeGenerator_C64.OpA(const aOpCode: TOpCode);                        // OPC A
begin
  WriteCode(cOpCodeName[aOpCode]);
end;

procedure TCodeGenerator_C64.OpAbs(const aOpCode: TOpCode; const aAddr: Word);   // OPC $HHLL
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,4)));
end;

procedure TCodeGenerator_C64.OpAbsX(const aOpCode: TOpCode; const aAddr: Word);  // OPC $HHLL,X
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,4) + ',X'));
end;

procedure TCodeGenerator_C64.OpAbsY(const aOpCode: TOpCode; const aAddr: Word);  // OPC $HHLL,Y
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,4) + ',Y'));
end;

procedure TCodeGenerator_C64.OpImmed(const aOpCode: TOpCode; const aValue: Byte); // OPC #$BB
begin
  WriteCode(cOpCodeName[aOpCode] + '#$' + LowerCase(IntToHex(aValue,2)));
end;

procedure TCodeGenerator_C64.OpImpl(const aOpCode: TOpCode);                      // OPC
begin
  WriteCode(cOpCodeName[aOpCode]);
end;

procedure TCodeGenerator_C64.OpInd(const aOpCode: TOpCode; const aAddr: Word);   // OPC ($HHLL)
begin
  WriteCode(cOpCodeName[aOpCode] + ' ($' + LowerCase(IntToHex(aAddr,4) + ')'));
end;

procedure TCodeGenerator_C64.OpXInd(const aOpCode: TOpCode; const aAddr: Byte);  // OPC ($BB,X)
begin
  WriteCode(cOpCodeName[aOpCode] + ' ($' + LowerCase(IntToHex(aAddr,2) + ',X)'));
end;

procedure TCodeGenerator_C64.OpIndY(const aOpCode: TOpCode; const aAddr: Byte);  // OPC ($LL),Y
begin
  WriteCode(cOpCodeName[aOpCode] + ' ($' + LowerCase(IntToHex(aAddr,2) + '),Y'));
end;

procedure TCodeGenerator_C64.OpRel(const aOpCode: TOpCode; const aAddr: Byte);   // OPC $BB
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,2)));
end;

procedure TCodeGenerator_C64.OpZpg(const aOpCode: TOpCode; const aAddr: Byte);   // OPC $LL
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,2)));
end;

procedure TCodeGenerator_C64.OpZpgX(const aOpCode: TOpCode; const aAddr: Byte);  // OPC $LL,X
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,2) + ',X'));
end;

procedure TCodeGenerator_C64.OpZpgY(const aOpCode: TOpCode; const aAddr: Byte);  // OPC $LL,Y
begin
  WriteCode(cOpCodeName[aOpCode] + ' $' + LowerCase(IntToHex(aAddr,2) + ',Y'));
end;

end.

