unit unit_pas2c64_parser;
{$IFDEF fpc}
{$MODE DELPHI}
{$ENDIF}
{$H+}
interface

uses
  Classes,
  uParser,
  unit_pas2c64_CodeGenerator,
  unit_pas2c64_symboltable,
  unit_Expressions;

// tokens
var
  Token_poke       : Integer;
  Token_pokew      : Integer;
  Token_peek       : Integer;
  Token_peekw      : Integer;
  Token_copymemb   : Integer;
  Token_write      : Integer;
  Token_writeln    : Integer;
  Token_readln     : Integer;
  Token_interrupt  : Integer;
  Token_assembler  : Integer;
  Token_incmemb    : Integer;
  Token_setint     : Integer;
  Token_stdirq     : Integer;

// asm tokens
  Token_adc        : Integer;
//  Token_and        : Integer;
  Token_asl        : Integer;
  Token_bcc        : Integer;
  Token_bcs        : Integer;
  Token_beq        : Integer;
  Token_bit        : Integer;
  Token_bmi        : Integer;
  Token_bne        : Integer;
  Token_bpl        : Integer;
  Token_brk        : Integer;
  Token_bvc        : Integer;
  Token_bvs        : Integer;
  Token_clc        : Integer;
  Token_cld        : Integer;
  Token_cli        : Integer;
  Token_clv        : Integer;
  Token_cmp        : Integer;
  Token_cpx        : Integer;
  Token_cpy        : Integer;
  Token_dec        : Integer;
  Token_dex        : Integer;
  Token_dey        : Integer;
  Token_eor        : Integer;
  Token_inc        : Integer;
  Token_inx        : Integer;
  Token_iny        : Integer;
  Token_jmp        : Integer;
  Token_jsr        : Integer;
  Token_lda        : Integer;
  Token_ldx        : Integer;
  Token_ldy        : Integer;
  Token_lsr        : Integer;
  Token_nop        : Integer;
  Token_ora        : Integer;
  Token_pha        : Integer;
  Token_php        : Integer;
  Token_pla        : Integer;
  Token_plp        : Integer;
  Token_rol        : Integer;
  Token_ror        : Integer;
  Token_rti        : Integer;
  Token_rts        : Integer;
  Token_sbc        : Integer;
  Token_sec        : Integer;
  Token_sed        : Integer;
  Token_sei        : Integer;
  Token_sta        : Integer;
  Token_stx        : Integer;
  Token_sty        : Integer;
  Token_tax        : Integer;
  Token_tay        : Integer;
  Token_tsx        : Integer;
  Token_txa        : Integer;
  Token_txs        : Integer;
  Token_tya        : Integer;


type
  TExpressionMode = (
    emAllIdents,
    emConstIdentsOnly
  );

  TPas2C64_Parser = class(TBaseParser)
  private
    FCodeAddr       : Word;
    FExpressionMode : TExpressionMode;
    FSymbolTable    : TPas2C64_SymbolTable;
    FCodeGen        : TCodeGenerator_C64;
    FExpression     : TExpressionNodeList;
    FAsmTokens      : set of Byte;

    function  IsIdent(const aStr: String): Boolean;
    procedure GetConstInfo(const aToken: TToken;
                           out   aSymClass,aSymSubClass: Integer;
                           out   aConstNum: Int64;
                           out   aConstType: TIntNumType);

    procedure SetExpressionMode(const aMode: TExpressionMode);
    function  GetTokenTypeFromSymbol(const aSym: TSymbol): Integer;
    function  ResolveIdentifier(const aIdent: TToken): TToken;

    function  Simplify_Neg(const a: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Not(const a: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Mul(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Div(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Sub(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Add(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_IntDiv(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_IntMod(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_And(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Or(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Xor(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Shl(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
    function  Simplify_Shr(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;

    function  OnAddExpressionOperation(const a,b: TExpressionOperandNode;
                                       const Op: TExpressionOperator;
                                       out   aResultValue: String;
                                       out   aResultType: Integer): Boolean;

    function  ParseFactor: TExpressionNodeList;
    function  ParseSignedFactor: TExpressionNodeList;

    function  ParseNot: TExpressionNodeList;
    function  ParseMultiply: TExpressionNodeList;
    function  ParseDivide: TExpressionNodeList;
    function  ParseIntDiv: TExpressionNodeList;
    function  ParseIntMod: TExpressionNodeList;
    function  ParseAnd: TExpressionNodeList;
    function  ParseShl: TExpressionNodeList;
    function  ParseShr: TExpressionNodeList;
    function  ParseTerm: TExpressionNodeList;

    function  ParseAdd: TExpressionNodeList;
    function  ParseSubtract: TExpressionNodeList;
    function  ParseOr: TExpressionNodeList;
    function  ParseXor: TExpressionNodeList;
    function  ParseExpression(const aNewExpression: Boolean;
                              const aMode: TExpressionMode = emAllIdents): TExpressionNodeList;

    function  ParseExpressionAsToken(const aAllowedTokenTypes: array of  Integer): TToken;

    // statements types
    procedure ParsePoke;
    procedure ParsePokeW;
    procedure ParsePeek;
    procedure ParsePeekW;
    procedure ParseCopyMemB;
    procedure ParseIncMemB;
    procedure ParseStdIRQ;
    procedure ParseSetInterrupt;
    procedure ParseWriteLn;
    procedure ParseReadLn;
    procedure ParseWaitForKey;

    procedure ParseProcedureCall(const aSym: TSymbol);
    procedure ParseAssignment(const aSym: TSymbol);
    procedure ParseProcedureCallOrAssignment(const aIdentifier: String);
    //

    procedure ParseStatement;
    procedure ParseBlock;

    procedure ParseAsmStatement;
    procedure ParseAsmBlock;

    procedure ParseConstDecls;
    procedure ParseVarDecls;

    procedure ParseProcDecl;

    procedure RegisterConstants;
  protected
    procedure RegisterGenericTokens; override;
    procedure RegisterKeywordTokens; override;
    procedure ParseInput;            override;
  public
    constructor Create;
    destructor  Destroy;             override;

    property CodeAddr: Word       read FCodeAddr write FCodeAddr;
  end;

function  IntToC64Hex(const aNumber: Int64; const aIntNumType: TIntNumType): AnsiString;

implementation

uses
  SysUtils;

const
  cASCIIToPETSCIITable: array[0..256 - 1] of Byte = (
    $00,$01,$02,$03,$04,$05,$06,$07,$14,$20,$0d,$11,$93,$0a,$0e,$0f,
    $10,$0b,$12,$13,$08,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
    $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
    $40,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
    $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$5b,$5c,$5d,$5e,$5f,
    $c0,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$db,$dc,$dd,$de,$df,
    $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
    $90,$91,$92,$0c,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
    $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
    $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
    $60,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
    $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$7b,$7c,$7d,$7e,$7f,
    $e0,$e1,$e2,$e3,$e4,$e5,$e6,$e7,$e8,$e9,$ea,$eb,$ec,$ed,$ee,$ef,
    $f0,$f1,$f2,$f3,$f4,$f5,$f6,$f7,$f8,$f9,$fa,$fb,$fc,$fd,$fe,$ff
  );

  cPETSCIIToASCIITable: array[0..256 - 1] of Byte = (
    $00,$01,$02,$03,$04,$05,$06,$07,$14,$09,$0d,$11,$93,$0a,$0e,$0f,
    $10,$0b,$12,$13,$08,$15,$16,$17,$18,$19,$1a,$1b,$1c,$1d,$1e,$1f,
    $20,$21,$22,$23,$24,$25,$26,$27,$28,$29,$2a,$2b,$2c,$2d,$2e,$2f,
    $30,$31,$32,$33,$34,$35,$36,$37,$38,$39,$3a,$3b,$3c,$3d,$3e,$3f,
    $40,$61,$62,$63,$64,$65,$66,$67,$68,$69,$6a,$6b,$6c,$6d,$6e,$6f,
    $70,$71,$72,$73,$74,$75,$76,$77,$78,$79,$7a,$5b,$5c,$5d,$5e,$5f,
    $c0,$c1,$c2,$c3,$c4,$c5,$c6,$c7,$c8,$c9,$ca,$cb,$cc,$cd,$ce,$cf,
    $d0,$d1,$d2,$d3,$d4,$d5,$d6,$d7,$d8,$d9,$da,$db,$dc,$dd,$de,$df,
    $80,$81,$82,$83,$84,$85,$86,$87,$88,$89,$8a,$8b,$8c,$8d,$8e,$8f,
    $90,$91,$92,$0c,$94,$95,$96,$97,$98,$99,$9a,$9b,$9c,$9d,$9e,$9f,
    $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
    $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf,
    $60,$41,$42,$43,$44,$45,$46,$47,$48,$49,$4a,$4b,$4c,$4d,$4e,$4f,
    $50,$51,$52,$53,$54,$55,$56,$57,$58,$59,$5a,$7b,$7c,$7d,$7e,$7f,
    $a0,$a1,$a2,$a3,$a4,$a5,$a6,$a7,$a8,$a9,$aa,$ab,$ac,$ad,$ae,$af,
    $b0,$b1,$b2,$b3,$b4,$b5,$b6,$b7,$b8,$b9,$ba,$bb,$bc,$bd,$be,$bf
  );

function  StrToPETSCII(const aStr: String): String;
var
  i: Integer;
begin
  Result := aStr;

  for i := 1 to Length(aStr) do
    Result[i] := Char(cASCIIToPETSCIITable[Byte(aStr[i])]);
end;

function  IntToC64Hex(const aNumber: Int64; const aIntNumType: TIntNumType): AnsiString;
var
  i,s: Integer;
begin
  Result := '';

  s := 0;
  case aIntNumType of
    ntSInt8, ntUInt8   : i := 1;
    ntSInt16, ntUInt16 : i := 2;
    ntSInt32, ntUInt32 : i := 4;
  end;

  while i > 0 do
  begin
    if Result = '' then
      Result := '$' + IntToHex((aNumber shr s) and $FF,2)
    else
      Result := Result + ',$' + LowerCase(IntToHex((aNumber shr s) and $FF,2));
    s := s + 8;
    Dec(i);
  end;

  Result := LowerCase(Result);
end;

constructor TPas2C64_Parser.Create;
begin
  inherited Create;

  FCodeGen        := TCodeGenerator_C64.Create;
  FCodeAddr       := $0000;
  FSymbolTable    := TPas2C64_SymbolTable.Create;
  FExpression     := TExpressionNodeList.Create;
  FExpressionMode := emAllIdents;
end;

destructor  TPas2C64_Parser.Destroy;
begin
  FSymbolTable.Free;
  FExpression.Free;
  FCodeGen.Free;
  inherited Destroy;
end;

procedure TPas2C64_Parser.RegisterGenericTokens;
begin
  inherited RegisterGenericTokens;
end;

procedure TPas2C64_Parser.RegisterKeywordTokens;
begin
  inherited RegisterKeywordTokens;

  Token_poke       := RegisterKeywordToken('Poke');
  Token_pokew      := RegisterKeywordToken('PokeW');
  Token_peek       := RegisterKeywordToken('Peek');
  Token_peekw      := RegisterKeywordToken('PeekW');
  Token_copymemb   := RegisterKeywordToken('CopyMemB');
  Token_write      := RegisterKeywordToken('Write');
  Token_writeln    := RegisterKeywordToken('WriteLn');
  Token_readln     := RegisterKeywordToken('ReadLn');
  Token_interrupt  := RegisterKeywordToken('Interrupt');
  Token_assembler  := RegisterKeywordToken('Assembler');
  Token_incmemb    := RegisterKeywordToken('IncMemB');
  Token_setint     := RegisterKeywordToken('SetInterrupt');
  Token_stdirq     := RegisterKeywordToken('StdIRQ');

  Token_adc        := RegisterKeywordToken('adc');
//  Token_and        := RegisterKeywordToken('and'); // already used :)
  Token_asl        := RegisterKeywordToken('asl');
  Token_bcc        := RegisterKeywordToken('bcc');
  Token_bcs        := RegisterKeywordToken('bcs');
  Token_beq        := RegisterKeywordToken('beq');
  Token_bit        := RegisterKeywordToken('bit');
  Token_bmi        := RegisterKeywordToken('bmi');
  Token_bne        := RegisterKeywordToken('bne');
  Token_bpl        := RegisterKeywordToken('bpl');
  Token_brk        := RegisterKeywordToken('brk');
  Token_bvc        := RegisterKeywordToken('bvc');
  Token_bvs        := RegisterKeywordToken('bvs');
  Token_clc        := RegisterKeywordToken('clc');
  Token_cld        := RegisterKeywordToken('cld');
  Token_cli        := RegisterKeywordToken('cli');
  Token_clv        := RegisterKeywordToken('clv');
  Token_cmp        := RegisterKeywordToken('cmp');
  Token_cpx        := RegisterKeywordToken('cpx');
  Token_cpy        := RegisterKeywordToken('cpy');
  Token_dec        := RegisterKeywordToken('dec');
  Token_dex        := RegisterKeywordToken('dex');
  Token_dey        := RegisterKeywordToken('dey');
  Token_eor        := RegisterKeywordToken('eor');
  Token_inc        := RegisterKeywordToken('inc');
  Token_inx        := RegisterKeywordToken('inx');
  Token_iny        := RegisterKeywordToken('iny');
  Token_jmp        := RegisterKeywordToken('jmp');
  Token_jsr        := RegisterKeywordToken('jsr');
  Token_lda        := RegisterKeywordToken('lda');
  Token_ldx        := RegisterKeywordToken('ldx');
  Token_ldy        := RegisterKeywordToken('ldy');
  Token_lsr        := RegisterKeywordToken('lsr');
  Token_nop        := RegisterKeywordToken('nop');
  Token_ora        := RegisterKeywordToken('ora');
  Token_pha        := RegisterKeywordToken('pha');
  Token_php        := RegisterKeywordToken('php');
  Token_pla        := RegisterKeywordToken('pla');
  Token_plp        := RegisterKeywordToken('plp');
  Token_rol        := RegisterKeywordToken('rol');
  Token_ror        := RegisterKeywordToken('ror');
  Token_rti        := RegisterKeywordToken('rti');
  Token_rts        := RegisterKeywordToken('rts');
  Token_sbc        := RegisterKeywordToken('sbc');
  Token_sec        := RegisterKeywordToken('sec');
  Token_sed        := RegisterKeywordToken('sed');
  Token_sei        := RegisterKeywordToken('sei');
  Token_sta        := RegisterKeywordToken('sta');
  Token_stx        := RegisterKeywordToken('stx');
  Token_sty        := RegisterKeywordToken('sty');
  Token_tax        := RegisterKeywordToken('tax');
  Token_tay        := RegisterKeywordToken('tay');
  Token_tsx        := RegisterKeywordToken('tsx');
  Token_txa        := RegisterKeywordToken('txa');
  Token_txs        := RegisterKeywordToken('txs');
  Token_tya        := RegisterKeywordToken('tya');

  FAsmTokens := [
    Token_adc,Token_and,Token_asl,Token_bcc,Token_bcs,Token_beq,Token_bit,Token_bmi,
    Token_bne,Token_bpl,Token_brk,Token_bvc,Token_bvs,Token_clc,Token_cld,Token_cli,
    Token_clv,Token_cmp,Token_cpx,Token_cpy,Token_dec,Token_dex,Token_dey,Token_eor,
    Token_inc,Token_inx,Token_iny,Token_jmp,Token_jsr,Token_lda,Token_ldx,Token_ldy,
    Token_lsr,Token_nop,Token_ora,Token_pha,Token_php,Token_pla,Token_plp,Token_rol,
    Token_ror,Token_rti,Token_rts,Token_sbc,Token_sec,Token_sed,Token_sei,Token_sta,
    Token_stx,Token_sty,Token_tax,Token_tay,Token_tsx,Token_txa,Token_txs,Token_tya
  ];
end;

function  TPas2C64_Parser.Simplify_Neg(const a: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := True;

  aResultValue := '';
  aResultType  := a.OperandType;

  if not (a.OperandType in [Token_ident,Token_intnumber,Token_fracnumber]) then
    Error('Expression error: Invalid type "'+TokenToStr(a.OperandType)+'" in negate operation');

  if a.OperandType = Token_ident then
  begin
    if a.OperandValue[1] = '-' then
    // make positive; remove -ve sign
      aResultValue := Copy(a.OperandValue,2,Length(a.OperandValue))
    else
    // make negative; add -ve sign
      aResultValue := '-' + a.OperandValue;
  end
  else
  if a.OperandType = Token_fracnumber then
  begin
    aResultValue := FloatToStr(-1 * StrToFloat(a.OperandValue));
  end
  else
  if a.OperandType = Token_intnumber then
  begin
    GetConstInfo(NewToken(a.OperandValue,a.OperandType),SymClass,SymSubClass,ConstNum,ConstType);

    aResultValue := IntToStr(-1 * ConstNum);
  end;

  FExpression.DeleteNode(a);
end;

function  TPas2C64_Parser.Simplify_Not(const a: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
begin
  Result := False;
  aResultValue := '';
  aResultType  := Token_unknown;
end;

function  TPas2C64_Parser.Simplify_Mul(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) * StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
  if (a.OperandType = Token_fracnumber) and (b.OperandType = Token_fracnumber) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) * StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
  if (a.OperandType in [Token_intnumber,Token_fracnumber]) and (b.OperandType in [Token_intnumber,Token_fracnumber]) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) * StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "multiply" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Div(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := FloatToStr(StrToInt(a.OperandValue) / StrToInt(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
  if (a.OperandType = Token_fracnumber) and (b.OperandType = Token_fracnumber) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) / StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
  if (a.OperandType in [Token_intnumber,Token_fracnumber]) and (b.OperandType in [Token_intnumber,Token_fracnumber]) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) / StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "division" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Sub(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) - StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
  if (a.OperandType = Token_fracnumber) and (b.OperandType = Token_fracnumber) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) - StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
  if (a.OperandType in [Token_intnumber,Token_fracnumber]) and (b.OperandType in [Token_intnumber,Token_fracnumber]) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) - StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "subtraction" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Add(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_string) and (b.OperandType = Token_string) then
  begin
    aResultValue := a.OperandValue + b.OperandValue;
    aResultType  := Token_string;
    Result := True;
  end
  else
  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) + StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
  if (a.OperandType = Token_fracnumber) and (b.OperandType = Token_fracnumber) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) + StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
  if (a.OperandType in [Token_intnumber,Token_fracnumber]) and (b.OperandType in [Token_intnumber,Token_fracnumber]) then
  begin
    aResultValue := FloatToStr(StrToFloat(a.OperandValue) + StrToFloat(b.OperandValue));
    aResultType  := Token_fracnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "addition" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_IntDiv(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) div StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "div" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_IntMod(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) mod StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "mod" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_And(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) and StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "and" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Or(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) or StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "or" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Xor(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) xor StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "xor" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Shl(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) shl StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "shl" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.Simplify_Shr(const a,b: TExpressionOperandNode; out aResultValue: String; out aResultType: Integer): Boolean;
var
  SymClass,SymSubClass: Integer;
  ConstNum: Int64;
  ConstType: TIntNumType;
begin
  Result := False;

  aResultValue := '';
  aResultType  := a.OperandType;

  if (a.OperandType = Token_ident) or (b.OperandType = Token_ident) then Exit; // can't simplify

  if (a.OperandType = Token_intnumber) and (b.OperandType = Token_intnumber) then
  begin
    aResultValue := IntToStr(StrToInt(a.OperandValue) shr StrToInt(b.OperandValue));
    aResultType  := Token_intnumber;
    Result := True;
  end
  else
    Error('Expression error: Incompatible type "'+TokenToStr(a.OperandType)+'" and "'+TokenToStr(b.OperandType)+'" in "shr" operation');

  if Result then
  begin
    FExpression.DeleteNode(a);
    FExpression.DeleteNode(b);
  end;
end;

function  TPas2C64_Parser.OnAddExpressionOperation(const a,b: TExpressionOperandNode;
                                                   const Op: TExpressionOperator;
                                                   out   aResultValue: String;
                                                   out   aResultType: Integer): Boolean;
// returns false if operation wasn't simplified.
// Otherwise, aResultValue, aResultType contains the simplified result.
// throws error if incompatible types in operation...
begin
  case Op of
    eoNeg    : Result := Simplify_Neg   (b,aResultValue,aResultType); // only uses 1 operand
    eoNot    : Result := Simplify_Not   (b,aResultValue,aResultType); // only uses 1 operand
    eoMul    : Result := Simplify_Mul   (a,b,aResultValue,aResultType);
    eoDiv    : Result := Simplify_Div   (a,b,aResultValue,aResultType);
    eoSub    : Result := Simplify_Sub   (a,b,aResultValue,aResultType);
    eoAdd    : Result := Simplify_Add   (a,b,aResultValue,aResultType);
    eoIntDiv : Result := Simplify_IntDiv(a,b,aResultValue,aResultType);
    eoIntMod : Result := Simplify_IntMod(a,b,aResultValue,aResultType);
    eoAnd    : Result := Simplify_And   (a,b,aResultValue,aResultType);
    eoOr     : Result := Simplify_Or    (a,b,aResultValue,aResultType);
    eoXor    : Result := Simplify_Xor   (a,b,aResultValue,aResultType);
    eoShl    : Result := Simplify_Shl   (a,b,aResultValue,aResultType);
    eoShr    : Result := Simplify_Shr   (a,b,aResultValue,aResultType);
  else
    Result := False;
  end;
end;

function  TPas2C64_Parser.IsIdent(const aStr: String): Boolean;
begin
  Result := False;

  if aStr ='' then Exit;
  if (aStr[1] = '_') or IsAlpha(aStr[1]) then
    Result := True;
end;

procedure TPas2C64_Parser.GetConstInfo(const aToken: TToken;
                                       out   aSymClass,aSymSubClass: Integer;
                                       out   aConstNum: Int64;
                                       out   aConstType: TIntNumType);
begin
  aSymClass    := cSymClass_Constant;
  aSymSubClass := cSymSubClass_None;

  if aToken.TokenType = Token_string then
    aSymSubClass := cSymSubClass_String
  else
  if aToken.TokenType = Token_fracnumber then
    aSymSubClass := cSymSubClass_Float
  else
  if aToken.TokenType = Token_intnumber then
  begin
    GetIntegerTypeAndValue(aToken.TokenValue,aConstType,aConstNum);

    case aConstType of
      ntSInt8  : aSymSubClass := cSymSubClass_SInt8;
      ntUInt8  : aSymSubClass := cSymSubClass_UInt8;
      ntSInt16 : aSymSubClass := cSymSubClass_SInt16;
      ntUInt16 : aSymSubClass := cSymSubClass_UInt16;
      ntSInt32 : aSymSubClass := cSymSubClass_SInt32;
      ntUInt32 : aSymSubClass := cSymSubClass_UInt32;
    end;
  end;
end;

procedure TPas2C64_Parser.SetExpressionMode(const aMode: TExpressionMode);
begin
  FExpressionMode := aMode;
end;

function  TPas2C64_Parser.GetTokenTypeFromSymbol(const aSym: TSymbol): Integer;
begin
  case aSym.SymbolSubClass of
    cSymSubClass_SInt8  : Result := Token_intnumber;
    cSymSubClass_SInt16 : Result := Token_intnumber;
    cSymSubClass_SInt32 : Result := Token_intnumber;
    cSymSubClass_UInt8  : Result := Token_intnumber;
    cSymSubClass_UInt16 : Result := Token_intnumber;
    cSymSubClass_UInt32 : Result := Token_intnumber;
    cSymSubClass_Float  : Result := Token_fracnumber;
    cSymSubClass_String : Result := Token_string;
  else
    Result := Token_unknown;
  end;
end;

function  TPas2C64_Parser.ResolveIdentifier(const aIdent: TToken): TToken;
var
  Sym: TSymbol;
begin
  Result := aIdent;

  Sym := FSymbolTable.GetSymbol(aIdent.TokenValue);

  if not Assigned(Sym) then
    Error('Illegal expression: Undeclared identifier "'+aIdent.TokenValue+'"');

  if Sym.SymbolClass in [cSymClass_Procedure,cSymClass_Interrupt] then
    Error('Illegal expression: Identifier "'+Sym.SymbolName+'" is a procedure/interrupt routine');

  if Sym.SymbolClass <> cSymClass_Constant then
  begin
    if FExpressionMode = emConstIdentsOnly then
      Error('Illegal expression: Identifier "'+Sym.SymbolName+'" is not a constant')
    else
      Exit;
  end;

  Result.TokenValue := Sym.SymbolValue;
  Result.TokenType  := GetTokenTypeFromSymbol(Sym);

  if IsIdent(Result.TokenValue) then
    Result := ResolveIdentifier(Result);
end;

function  TPas2C64_Parser.ParseFactor: TExpressionNodeList;
var
  v: TToken;
  Sym: TSymbol;
begin
  if Accept(Token_lparen) then
  // factor is an expression, so parse expression
  begin
    Result := ParseExpression(False,FExpressionMode);
    Expect(Token_rparen);
  end
  else
  begin
    v := GetToken;

    if v.TokenType = Token_ident then
    begin
      v := ResolveIdentifier(v);
    end;

    Result := FExpression.AddOperand(v.TokenValue,v.TokenType);
  end;
end;

function  TPas2C64_Parser.ParseSignedFactor: TExpressionNodeList;
var
  NegativeFactor: Boolean;
begin
  NegativeFactor := False;

  if Token.TokenType in [Token_plus,Token_minus] then
  begin
    NegativeFactor := (Token.TokenType = Token_minus);

    Expect(Token.TokenType);
  end;

  Result := ParseFactor;

  if NegativeFactor then
    Result := FExpression.AddOperator(eoNeg,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseNot: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoNot,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseMultiply: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoMul,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseDivide: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoDiv,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseIntDiv: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoIntDiv,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseIntMod: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoIntMod,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseAnd: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoAnd,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseShl: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoShl,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseShr: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoShr,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseTerm: TExpressionNodeList;
begin
  Result := ParseSignedFactor;

  while Token.TokenType in [Token_times,Token_slash,Token_div,Token_mod,Token_and,Token_shl,Token_shr] do
  begin
    if      Accept(Token_times) then Result := ParseMultiply
    else if Accept(Token_slash) then Result := ParseDivide
    else if Accept(Token_div)   then Result := ParseIntDiv
    else if Accept(Token_mod)   then Result := ParseIntMod
    else if Accept(Token_and)   then Result := ParseAnd
    else if Accept(Token_shl)   then Result := ParseShl
    else if Accept(Token_shr)   then Result := ParseShr;
  end;
end;

function  TPas2C64_Parser.ParseAdd: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoAdd,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseSubtract: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoSub,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseOr: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoOr,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseXor: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoXor,OnAddExpressionOperation);
end;

function  TPas2C64_Parser.ParseExpression(const aNewExpression: Boolean;
                                          const aMode: TExpressionMode = emAllIdents): TExpressionNodeList;
begin
  if aNewExpression then FExpression.Clear;

  SetExpressionMode(aMode);

  Result := ParseTerm;

  while Token.TokenType in [Token_plus,Token_minus,Token_or,Token_xor] do
  begin
    if      Accept(Token_plus)  then Result := ParseAdd
    else if Accept(Token_minus) then Result := ParseSubtract
    else if Accept(Token_or)    then Result := ParseOr
    else if Accept(Token_xor)   then Result := ParseXor
  end;
end;

function  TPas2C64_Parser.ParseExpressionAsToken(const aAllowedTokenTypes: array of  Integer): TToken;
var
  e: TExpressionNodeList;
  Node: TExpressionOperandNode;
  i: Integer;
  ValidType: Boolean;
  ValidTypes: String;
begin
  e := ParseExpression(True);
  Node := TExpressionOperandNode(e.GetNode(0));

  Result := NewToken(Node.OperandValue,Node.OperandType);

  ValidType := True;
  if Length(aAllowedTokenTypes) > 0 then
  begin
    ValidType := False;
    for i := 0 to High(aAllowedTokenTypes) do
      if Node.OperandType = aAllowedTokenTypes[i] then
      begin
        ValidType := True;
        Break;
      end;
  end;

  if not ValidType then
  begin
    ValidTypes := '';
    for i := 0 to High(aAllowedTokenTypes) do
      if ValidTypes = '' then
        ValidTypes := '"' + TokenToStr(aAllowedTokenTypes[i]) + '"'
      else
        ValidTypes := ValidTypes + ',"' + TokenToStr(aAllowedTokenTypes[i]) + '"';

    Error('Invalid expression: Expected types '+ValidTypes+', but got "'+TokenToStr(Node.OperandType)+'"');
  end;
end;

// statements

procedure TPas2C64_Parser.ParsePoke;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;

  ValueStr: AnsiString;
  ValueNum: Int64;
  ValueType: TIntNumType;
begin
  Expect(Token_lparen);
  AddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('Poke: Address out of range [$0000,$FFFF]');

  Expect(Token_comma);

  ValueStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(ValueStr,ValueType,ValueNum);
  if not IntegerInRange(ValueNum,$00,$FF) then Error('Poke: Value out of range [$00,$FF]');

  Expect(Token_rparen);

  FCodeGen.LoadReg_IM(regA,ValueNum);
  FCodeGen.StoreReg(regA,AddrNum);
end;

procedure TPas2C64_Parser.ParsePokeW;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;

  ValueStr: AnsiString;
  ValueNum: Int64;
  ValueType: TIntNumType;
begin
  Expect(Token_lparen);
  AddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('PokeW: Address out of range [$0000,$FFFF]');

  Expect(Token_comma);

  ValueStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(ValueStr,ValueType,ValueNum);
  if not IntegerInRange(ValueNum,$0000,$FFFF) then Error('PokeW: Value out of range [$0000,$FFFF]');

  Expect(Token_rparen);

  FCodeGen.WriteCode('lda #<' + ValueStr);
  FCodeGen.StoreReg(regA,AddrNum);
  FCodeGen.WriteCode('lda #>' + ValueStr);
  FCodeGen.StoreReg(regA,AddrNum+1);
end;

procedure TPas2C64_Parser.ParsePeek;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;
begin
  Expect(Token_lparen);
  AddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('Peek: Address out of range [$0000,$FFFF]');
  Expect(Token_rparen);

//  FCodeGen.LoadReg_IM(regA,ValueNum);
//  FCodeGen.StoreReg(regA,AddrNum);
end;

procedure TPas2C64_Parser.ParsePeekW;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;
begin
  Expect(Token_lparen);
  AddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;
  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('PeekW: Address out of range [$0000,$FFFF]');

  Expect(Token_rparen);

//  FCodeGen.LoadReg_IM(regA,ValueNum);
//  FCodeGen.StoreReg(regA,AddrNum);
end;

procedure TPas2C64_Parser.ParseCopyMemB;
var
  SrcAddrStr: AnsiString;
  SrcAddrNum: Int64;
  SrcAddrType: TIntNumType;

  DstAddrStr: AnsiString;
  DstAddrNum: Int64;
  DstAddrType: TIntNumType;
begin
  Expect(Token_lparen);

  SrcAddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;

  GetIntegerTypeAndValue(SrcAddrStr,SrcAddrType,SrcAddrNum);
  if not IntegerInRange(SrcAddrNum,$0000,$FFFF) then Error('CopyMemB: Source address out of range [$0000,$FFFF]');

  Expect(Token_comma);

  DstAddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;

  GetIntegerTypeAndValue(DstAddrStr,DstAddrType,DstAddrNum);
  if not IntegerInRange(DstAddrNum,$0000,$FFFF) then Error('CopyMemB: Destination address out of range [$0000,$FFFF]');

  Expect(Token_rparen);

  FCodeGen.LoadReg_Mem(regA,SrcAddrNum);
  FCodeGen.StoreReg(regA,DstAddrNum);
end;

procedure TPas2C64_Parser.ParseIncMemB;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;
begin
  Expect(Token_lparen);

  AddrStr := ParseExpressionAsToken([Token_intnumber]).TokenValue;

  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('IncMemB: Destination address out of range [$0000,$FFFF]');

  Expect(Token_rparen);

  FCodeGen.WriteCode(LowerCase(Format('inc $%.4x',[AddrNum])));
end;

procedure TPas2C64_Parser.ParseStdIRQ;
begin
  FCodeGen.WriteCode('jmp STDIRQ');
end;

procedure TPas2C64_Parser.ParseSetInterrupt;
var
  IntName: String;
  Sym: TSymbol;
begin
  Expect(Token_lparen);
  IntName := LowerCase(Token.TokenValue);
  Expect(Token_ident);

  Sym := FSymbolTable.GetSymbol(IntName);

  if not Assigned(Sym) then Error('SetInterrupt: Undefined identifier "'+IntName+'"');

  if Sym.SymbolClass <> cSymClass_Interrupt then Error('SetInterrupt: Identifier "'+IntName+'" not an interrupt routine');

  Expect(Token_rparen);

  FCodeGen.WriteCode('sei // disable interrupts');
  FCodeGen.WriteCode('');
  FCodeGen.WriteCode('// set a custom interrupt routine address');
  FCodeGen.WriteCode('// to interrupt vector');
  FCodeGen.WriteCode('');
  FCodeGen.WriteCode('lda #<int_'+IntName+' // low byte of int_'+IntName+' start addr');
  FCodeGen.WriteCode('ldx #>int_'+IntName+' // high byte of int_'+IntName+' start addr');
  FCodeGen.WriteCode('sta IRQVECLO');
  FCodeGen.WriteCode('stx IRQVECHI');
  FCodeGen.WriteCode('');
  FCodeGen.WriteCode('cli // clear interrupt disable bit');
end;


procedure TPas2C64_Parser.ParseWriteLn;
var
  v: TToken;
  c,l,d,t: AnsiString;
  C64Float: TC64MemFloat;

begin
  l := FCodeGen.NewLabel;
  d := FCodeGen.NewLabel;
  c := FCodeGen.NewLabel;

  Expect(Token_lparen);

  v := ParseExpressionAsToken([]);

  if v.TokenType = Token_string then
  begin
    FCodeGen.WriteCode(':PrintStringConst("'+UpperCase(v.TokenValue)+'")');
  end
  else
  if v.TokenType in [Token_intnumber,Token_fracnumber] then
  begin
    FloatToC64Float(StrToFloat(v.TokenValue),C64Float);

    FCodeGen.WriteComment('load floating point number into FAC1');
    FCodeGen.WriteCode('lda #<' + d);
    FCodeGen.WriteCode('ldy #>' + d);
    FCodeGen.WriteCode('jsr $bba2');
    FCodeGen.WriteCode('');

    FCodeGen.WriteComment('convert number in FAC1 to ASCII (pointer in A & Y)');
    FCodeGen.WriteCode('jsr $bddd');
    FCodeGen.WriteCode('');

    FCodeGen.WriteComment('print ASCII number pointed to by A/Y');

    FCodeGen.WriteCode('jsr PrintStringAY');
    FCodeGen.WriteCode('jmp '+c);
    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.byte '+C64FloatToStr(C64Float));
    FCodeGen.WriteLabel(c);
  end;

  Expect(Token_rparen);
  Expect(Token_semicolon);
end;

procedure TPas2C64_Parser.ParseReadLn;
begin
  FCodeGen.WriteCode('jsr FILTERED_TEXT');
end;

procedure TPas2C64_Parser.ParseWaitForKey;
begin
  FCodeGen.WriteCode('jsr WaitForKey');
end;

procedure TPas2C64_Parser.ParseProcedureCall(const aSym: TSymbol);
begin
  if not(aSym.SymbolClass in [cSymClass_Procedure,cSymClass_AsmProc]) then Error('Illegal procedure call: Identifier "'+aSym.SymbolName+'" is not a procedure');

  case aSym.SymbolClass of
    cSymClass_Procedure : FCodeGen.WriteCode('jsr proc_' + aSym.SymbolName);
    cSymClass_AsmProc   : FCodeGen.WriteCode('jsr asm_' + aSym.SymbolName);
  end;
end;

procedure TPas2C64_Parser.ParseAssignment(const aSym: TSymbol);
var
  e: TExpressionNodeList;
begin
  if aSym.SymbolClass <> cSymClass_Variable then Error('Illegal assignment statement: Identifier "'+aSym.SymbolName+'" is not a variable');

  Accept(Token_becomes);

  e := ParseExpression(True);
end;

procedure TPas2C64_Parser.ParseProcedureCallOrAssignment(const aIdentifier: String);
var
  Sym: TSymbol;
begin
  Sym := FSymbolTable.GetSymbol(aIdentifier);

  if not Assigned(Sym) then Error('Undeclared identifier: "'+aIdentifier+'"');

  if Token.TokenType = Token_becomes then
    ParseAssignment(Sym)
  else
    ParseProcedureCall(Sym);
end;

procedure TPas2C64_Parser.ParseStatement;
var
  TokenValue: String;
begin
  TokenValue := Token.TokenValue;

  if      Accept(Token_poke)       then ParsePoke
  else if Accept(Token_pokew)      then ParsePokeW
  else if Accept(Token_peek)       then ParsePeek
  else if Accept(Token_peekw)      then ParsePeekW
  else if Accept(Token_copymemb)   then ParseCopyMemB
  else if Accept(Token_writeln)    then ParseWriteLn
  else if Accept(Token_readln)     then ParseReadLn
  else if Accept(Token_incmemb)    then ParseIncMemB
  else if Accept(Token_stdirq)     then ParseStdIRQ
  else if Accept(Token_setint)     then ParseSetInterrupt
  else if Accept(Token_ident)      then ParseProcedureCallOrAssignment(TokenValue);
end;

procedure TPas2C64_Parser.ParseAsmStatement;
var
  Line: String;
  t: TToken;
begin
  t := Token;
  if Accept(Token_ident) then
  // label
  begin
    FCodeGen.WriteLabel(t.TokenValue);

    // skip rest of tokens till next valid token or end
    while not(Token.TokenType in FAsmTokens) and not(Token.TokenType  in [Token_ident,Token_end]) do
      GetToken;
  end
  else
  if t.TokenType in FAsmTokens then
  begin
    Line := t.TokenValue + ' ';

    GetToken;
    // skip rest of tokens till next valid token or end
    while not(Token.TokenType in FAsmTokens) and not(Token.TokenType  in [Token_ident,Token_end]) do
    begin
      Line := Line + Token.TokenValue;
      GetToken;
    end;

    FCodeGen.WriteCode(Line);
  end;

  Exit;
  if Token.TokenType <> Token_end then
    GetToken;
  Exit;
  t := Token;

  if  (t.TokenType = Token_ident) then
  // label
  begin
    Line := t.TokenValue + ':';

    Expect(t.TokenType);
    Expect(Token_colon);

    FCodeGen.WriteOutputCR(Line);
  end
  else
  if (t.TokenType in FAsmTokens) then
  // assembly instruction
  begin
    Line := '    ' + t.TokenValue + ' ';

    if Accept(Token_adc) then
    begin
      if Accept(Token_hash) then Line := Line + '#';
      if Accept(Token_lss)  then Line := Line + '<';
      if Accept(Token_gtr)  then Line := Line + '>';
      if Accept(Token_lparen) then
      begin

        Expect(Token_rparen);
      end;
    end;
    Expect(t.TokenType);

    while not (Token.TokenType in FAsmTokens) and
          not (Token.TokenType in [Token_ident,Token_end]) do
    begin
      Line := Line + Token.TokenValue;
      GetToken;
    end;

    FCodeGen.WriteOutputCR(Line);
  end
  else
  if t.TokenType <> Token_end then
    Error('Asm Statement: Unexpected symbol "'+TokenToStr(t.TokenType)+'"');
end;

procedure TPas2C64_Parser.ParseAsmBlock;
begin
    while not (Token.TokenType in[Token_end,Token_eof]) do
    begin
      ParseAsmStatement;
    end;
end;

procedure TPas2C64_Parser.ParseBlock;
begin
    while not (Token.TokenType in[Token_end]) do
    begin
      ParseStatement;

      Accept(Token_semicolon);
    end;
end;

procedure TPas2C64_Parser.ParseConstDecls;
var
  ConstName: AnsiString;
  ConstValue: TToken;
  ConstNum: Int64;
  ConstType: TIntNumType;
  IsSingle: Boolean;
  f: TC64MemFloat;
  SymClass,SymSubClass: Integer;
  e: TExpressionNodeList;
  Value: TExpressionOperandNode;
begin
  repeat
    ConstName := Token.TokenValue;

    Expect(Token_ident);

    if FSymbolTable.SymbolExists(ConstName) then
      Error('Constant declaration: Identifer "'+ConstName+'" redeclared!');

    Expect(Token_eql);

    e := ParseExpression(True,emConstIdentsOnly);
    e.WriteExpression;

    Value := TExpressionOperandNode(e.GetNode(0));

    ConstValue.TokenValue := Value.OperandValue;
    ConstValue.TokenType  := Value.OperandType;

    IsSingle := ConstValue.TokenType = Token_fracnumber;

    GetConstInfo(ConstValue,SymClass,SymSubClass,ConstNum,ConstType);

    Expect(Token_semicolon);

    // add const to symbol table
    FSymbolTable.AddSymbol(ConstName,ConstValue.TokenValue,SymClass,SymSubClass);

    if SymSubClass = cSymSubClass_String then
      FCodeGen.WriteCode('.byte '+IntToStr(Length(ConstValue.TokenValue)) + '  // string length');

    FCodeGen.WriteLabel('const_' + ConstName);

    if not (SymSubClass in [cSymSubClass_Float,cSymSubClass_String]) then
      FCodeGen.WriteCode('.byte ' + IntToC64Hex(ConstNum,ConstType))
    else
    if SymSubClass = cSymSubClass_String then
    begin
      FCodeGen.WriteCode('.text "' + ConstValue.TokenValue + '"');
      FCodeGen.WriteCode('.byte 0');
    end
    else
    if SymSubClass = cSymSubClass_Float then
    begin
      FloatToC64Float(StrToFloat(ConstValue.TokenValue),f);
      FCodeGen.WriteCode('.byte ' + C64FloatToStr(f));
    end
    else
      Error('Constant declaration: Illegal token "'+ConstName+'"!');
  until Token.TokenType in[Token_const,Token_var,Token_begin,Token_proc];
end;

procedure TPas2C64_Parser.ParseVarDecls;
var
  VarNames: TStringList;
  VarType: AnsiString;
  VarSize: TIntNumType;
  IsSingle: Boolean;
  IsString: Boolean;
  i: Integer;
begin
  VarNames := TStringList.Create;
  try
    repeat
      VarNames.Clear;

      if FSymbolTable.SymbolExists(Token.TokenValue) then
        Error('Var declaration: Identifer "'+Token.TokenValue+'" redeclared!');

      VarNames.Add(Token.TokenValue);
      Expect(Token_ident);

      while Token.TokenType = Token_comma do
      begin
        Expect(Token_comma);

        if FSymbolTable.SymbolExists(Token.TokenValue) then
          Error('Var declaration: Identifer "'+Token.TokenValue+'" redeclared!');

        VarNames.Add(Token.TokenValue);
        Expect(Token_ident);
      end;
      Expect(Token_colon);

      VarType  := LowerCase(Token.TokenValue);
      IsSingle := False;
      IsString := False;

      if      VarType = 'integer' then VarSize := ntSInt16
      else if VarType = 'single'  then IsSingle := True
      else if VarType = 'string'  then IsString := True
      else Error('Var declaration: Illegal variable type "'+VarType+'"');

      Expect(Token_ident);
      Expect(Token_semicolon);

      // output variables in asm
      for i := 0 to VarNames.Count - 1 do
      begin
        if IsString then
          FCodeGen.WriteCode('.byte 0 // string length');

        FCodeGen.WriteLabel('var_' + VarNames.Strings[i]);
        if IsString then
        begin
          FCodeGen.WriteCode('.fill 256, 0 // 255 string chars + null byte end');
          FSymbolTable.AddSymbol(VarNames.Strings[i],'',cSymClass_Variable,cSymSubClass_String);
        end
        else
        if IsSingle then
        begin
          FCodeGen.WriteCode('.byte $00,$00,$00,$00,$00');
          FSymbolTable.AddSymbol(VarNames.Strings[i],'',cSymClass_Variable,cSymSubClass_Float);
        end
        else
        // is integer
        begin
          FCodeGen.WriteCode('.byte ' + IntToC64Hex(0,VarSize));
          FSymbolTable.AddSymbol(VarNames.Strings[i],'',cSymClass_Variable,cSymSubClass_SInt16);
        end;
      end;
    until Token.TokenType in[Token_const,Token_var,Token_begin,Token_var,Token_proc];
  finally
    VarNames.Free;
  end;
end;

procedure TPas2C64_Parser.ParseProcDecl;
var
  ProcName: String;
  IsInterrupt: Boolean;
  IsAssembler: Boolean;
begin
  ProcName := LowerCase(Token.TokenValue);

  if FSymbolTable.SymbolExists(ProcName) then
    Error('Procedure declaration: Identifer "'+ProcName+'" redeclared!');

  Expect(Token_ident);

  if Accept(Token_lparen) then
  begin
    Expect(Token_rparen);
  end;

  Expect(Token_semicolon);

  IsInterrupt := False;
  IsAssembler := False;

  if Accept(Token_interrupt) then
  // is interrupt routine
  begin
    FSymbolTable.AddSymbol(ProcName,'',cSymClass_Interrupt,cSymSubClass_None);
    IsInterrupt := True;
    Expect(Token_semicolon);
  end
  else
  if Accept(Token_assembler) then
  begin
    FSymbolTable.AddSymbol(ProcName,'',cSymClass_AsmProc,cSymSubClass_None);
    IsAssembler := True;
    Expect(Token_semicolon);
  end
  else
  // is regular procedure
    FSymbolTable.AddSymbol(ProcName,'',cSymClass_Procedure,cSymSubClass_None);

  if IsAssembler then
    Expect(Token_asm)
  else
    Expect(Token_begin);

  if IsInterrupt then
    FCodeGen.WriteLabel('int_' + ProcName)
  else
  if IsAssembler then
    FCodeGen.WriteLabel('asm_' + ProcName)
  else
    FCodeGen.WriteLabel('proc_' + ProcName);

  if IsAssembler then
    ParseAsmBlock
  else
    ParseBlock;

  if IsInterrupt then
    FCodeGen.WriteCode('rti')
  else
    FCodeGen.WriteCode('rts');

  Expect(Token_end);
  Expect(Token_semicolon);
end;

procedure TPas2C64_Parser.ParseInput;
var
  ProgName: AnsiString;
begin
  FCodeGen.ResetLabels;
  FSymbolTable.Clear;

  RegisterConstants;

  DstStream.Seek(0,soFromBeginning);
  FCodeGen.SetOutputStream(DstStream);

  if Accept(Token_program) then
  begin
    ProgName := Token.TokenValue;

    Expect(Token_ident);

    if Accept(Token_lparen) then
    begin
      Expect(Token_ident);
      Expect(Token_comma);
      Expect(Token_ident);
      Expect(Token_rparen);
    end;
    Expect(Token_semicolon);
  end;

  if FCodeAddr = $0000 then
    FCodeGen.WriteProgramStart
  else
    FCodeGen.WriteProgramStart(FCodeAddr);

  FCodeGen.WriteComment('Standard routines, etc.');
  FCodeGen.WriteCode('.import source "rtl\Macros_RTL.asm"');
  FCodeGen.WriteCode('.import source "rtl\Consts_RTL.asm"');
  FCodeGen.WriteCode('.import source "rtl\Routines_RTL.asm"');
  FCodeGen.WriteCode('.import source "rtl\TextInput_RTL.asm"');

  while Token.TokenType in [Token_const,Token_var,Token_proc] do
  begin
    if      Accept(Token_const)     then ParseConstDecls
    else if Accept(Token_var)       then ParseVarDecls
    else if Accept(Token_proc)      then ParseProcDecl;
  end;

  Expect(Token_begin);

  FCodeGen.WriteMainStart;

  ParseBlock;

  FCodeGen.OpCode(opRTS);

  Expect(Token_end);
  Expect(Token_period);
end;

procedure TPas2C64_Parser.RegisterConstants;
  procedure RegisterConstant(const aName,aValue: String; const aConstType: Integer);
  var
    SymClass,SymSubClass: Integer;
    ConstNum: Int64;
    ConstType: TIntNumType;
  begin
    GetConstInfo(NewToken(aValue,aConstType),SymClass,SymSubClass,ConstNum,ConstType);

    FSymbolTable.AddSymbol(aName,aValue,SymClass,SymSubClass);
  end;

  procedure RegisterProcedure(const aName: String);
  begin
    FSymbolTable.AddSymbol(aName,'',cSymClass_Procedure,cSymSubClass_None);
  end;

begin
// Standard Kernal ROM routines
  RegisterConstant('CHKIN'      , '$ffc6', Token_intnumber);
  RegisterConstant('CHKOUT'     , '$ffc9', Token_intnumber);
  RegisterConstant('CHRIN'      , '$ffcf', Token_intnumber);
  RegisterConstant('CHROUT'     , '$ffd2', Token_intnumber);
  RegisterConstant('CLALL'      , '$ffe7', Token_intnumber);
  RegisterConstant('CLOSE'      , '$ffc3', Token_intnumber);
  RegisterConstant('CLRCHN'     , '$ffcc', Token_intnumber);
  RegisterConstant('GETIN'      , '$ffe4', Token_intnumber);
  RegisterConstant('IECIN'      , '$ffa5', Token_intnumber);
  RegisterConstant('IECOUT'     , '$ffa8', Token_intnumber);
  RegisterConstant('IOBASE'     , '$fff3', Token_intnumber);
  RegisterConstant('IOINIT'     , '$ff84', Token_intnumber);
  RegisterConstant('LISTEN'     , '$ffb1', Token_intnumber);
  RegisterConstant('LOAD'       , '$ffd5', Token_intnumber);
  RegisterConstant('LSTNSA'     , '$ff93', Token_intnumber);
  RegisterConstant('MEMBOT'     , '$ff99', Token_intnumber);
  RegisterConstant('MEMTOP'     , '$ff9c', Token_intnumber);
  RegisterConstant('OPEN'       , '$ffc0', Token_intnumber);
  RegisterConstant('PLOT'       , '$fff0', Token_intnumber);
  RegisterConstant('RAMTAS'     , '$ff87', Token_intnumber);
  RegisterConstant('RDTIM'      , '$ffde', Token_intnumber);
  RegisterConstant('READST'     , '$ffb7', Token_intnumber);
  RegisterConstant('RESTOR'     , '$ff8a', Token_intnumber);
  RegisterConstant('SAVE'       , '$ffd8', Token_intnumber);
  RegisterConstant('SCINIT'     , '$ff81', Token_intnumber);
  RegisterConstant('SCNKEY'     , '$ff9f', Token_intnumber);
  RegisterConstant('SCREEN'     , '$ffed', Token_intnumber);
  RegisterConstant('SETLFS'     , '$ffba', Token_intnumber);
  RegisterConstant('SETMSG'     , '$ff90', Token_intnumber);
  RegisterConstant('SETNAM'     , '$ffbd', Token_intnumber);
  RegisterConstant('SETTIM'     , '$ffdb', Token_intnumber);
  RegisterConstant('SETTMO'     , '$ffa2', Token_intnumber);
  RegisterConstant('STOP'       , '$ffe1', Token_intnumber);
  RegisterConstant('TALK'       , '$ffb4', Token_intnumber);
  RegisterConstant('TALKSA'     , '$ff96', Token_intnumber);
  RegisterConstant('UDTIM'      , '$ffea', Token_intnumber);
  RegisterConstant('UNLSTN'     , '$ffae', Token_intnumber);
  RegisterConstant('UNTALK'     , '$ffab', Token_intnumber);
  RegisterConstant('VECTOR'     , '$ff8d', Token_intnumber);

// floating point routines
  RegisterConstant('MOVFM'      , '$bba2', Token_intnumber);
  RegisterConstant('FOUT'       , '$bddd', Token_intnumber);
  RegisterConstant('MOV2F'      , '$bbc7', Token_intnumber);

// interrupt routines and vectors
  RegisterConstant('IRQVECLO'   , '$0314', Token_intnumber);
  RegisterConstant('IRQVECHI'   , '$0315', Token_intnumber);
//  RegisterConstant('STDIRQ'     , '$ea31', Token_intnumber);

// keyboard
  RegisterConstant('CURRKEY'    , '$cb', Token_intnumber);

// VIC-II registers
  RegisterConstant('VICCTRLREG' , '$d016', Token_intnumber);
  RegisterConstant('BDRCOLOR'   , '$d020', Token_intnumber);
  RegisterConstant('BGCOLOR0'   , '$d021', Token_intnumber);
  RegisterConstant('BGCOLOR1'   , '$d022', Token_intnumber);
  RegisterConstant('BGCOLOR2'   , '$d023', Token_intnumber);

// definitions
  RegisterConstant('TRUE'       , '$ff'  , Token_intnumber);
  RegisterConstant('FALSE'      , '$00'  , Token_intnumber);

// procedures in Routines_RTL.asm
  RegisterProcedure('SwitchToUpperCase');
  RegisterProcedure('SwitchToLowerCase');
  RegisterProcedure('ToggleCase');
  RegisterProcedure('WaitForKey');
end;

end.
