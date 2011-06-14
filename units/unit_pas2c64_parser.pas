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
  Token_writememb : Integer;
  Token_writememw : Integer;
  Token_readmemb  : Integer;
  Token_copymemb  : Integer;
  Token_write     : Integer;
  Token_writeln   : Integer;

type
  TPas2C64_Parser = class(TBaseParser)
  private
    FCodeAddr    : Word;
    FSymbolTable : TPas2C64_SymbolTable;
    FCodeGen     : TCodeGenerator_C64;
    FExpression  : TExpressionNodeList;

    function  ParseFactor: TExpressionNodeList;
    function  ParseSignedFactor: TExpressionNodeList;

    function  ParseNot: TExpressionNodeList;
    function  ParseMultiply: TExpressionNodeList;
    function  ParseDivide: TExpressionNodeList;
    function  ParseIntDiv: TExpressionNodeList;
    function  ParseIntMod: TExpressionNodeList;
    function  ParseAnd: TExpressionNodeList;
    function  ParseTerm: TExpressionNodeList;

    function  ParseAdd: TExpressionNodeList;
    function  ParseSubtract: TExpressionNodeList;
    function  ParseOr: TExpressionNodeList;
    function  ParseExpression(const aNewExpression: Boolean): TExpressionNodeList;

    // statements types
    procedure ParseWriteMemB;
    procedure ParseCopyMemB;
    procedure ParseWriteLn;
    //

    procedure ParseStatement;
    procedure ParseBlock;

    procedure ParseConstDecls;
    procedure ParseVarDecls;

  protected
    procedure RegisterGenericTokens; override;
    procedure RegisterKeywordTokens; override;
    procedure ParseInput;            override;
  public
    constructor Create;
    destructor  Destroy;             override;

    property CodeAddr: Word       read FCodeAddr write FCodeAddr;
  end;

function  IntToC64Hex(const aNumber: Int64; const aNumType: TNumType): AnsiString;

implementation

uses
  SysUtils;

function  IntToC64Hex(const aNumber: Int64; const aNumType: TNumType): AnsiString;
var
  i,s: Integer;
begin
  Result := '';

  s := 0;
  case aNumType of
    ntInt8, ntUInt8   : i := 1;
    ntInt16, ntUInt16 : i := 2;
    ntInt32, ntUInt32 : i := 4;
  end;

  while i > 0 do
  begin
    Result := Result + '$' + IntToHex((aNumber shr s) and $FF,2) + ' ';
    s := s + 8;
    Dec(i);
  end;

  Result := LowerCase(Result);
end;

constructor TPas2C64_Parser.Create;
begin
  inherited Create;

  FCodeGen  := TCodeGenerator_C64.Create;
  FCodeAddr := $0000;
  FExpression := TExpressionNodeList.Create;
end;

destructor  TPas2C64_Parser.Destroy;
begin
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

  Token_writememb := RegisterKeywordToken('WriteMemB');
  Token_writememw := RegisterKeywordToken('WriteMemW');
  Token_readmemb  := RegisterKeywordToken('ReadMemB');
  Token_copymemb  := RegisterKeywordToken('CopyMemB');
  Token_write     := RegisterKeywordToken('Write');
  Token_writeln   := RegisterKeywordToken('WriteLn');
end;

function  TPas2C64_Parser.ParseFactor: TExpressionNodeList;
var
  v: TToken;
begin
  if Accept(Token_lparen) then
  // factor is an expression, so parse expression
  begin
    Result := ParseExpression(False);
    Expect(Token_rparen);
  end
  else
  begin
    v := GetToken;
    Result := FExpression.AddOperand(v.TokenValue,v.TokenType);
  end;
end;

function  TPas2C64_Parser.ParseSignedFactor: TExpressionNodeList;
var
  NegativeFactor: Boolean;
  ThisCount: Integer;
  LastCount: Integer;
  DeltaCount: Integer;
begin
  NegativeFactor := False;

  if Token.TokenType in [Token_plus,Token_minus] then
  begin
    FExpression.AddOperand('0',Token_intnumber);
    NegativeFactor := (Token.TokenType = Token_minus);

    Expect(Token.TokenType);
  end;

  ItemCount := FExpression.GetNodeCount;

  Result := ParseFactor;

  if NegativeFactor then
  begin
    DeltaCount := FExpression.GetNodeCount - ItemCount;

    Result := FExpression.AddOperator(eoSub);
  end;
end;

function  TPas2C64_Parser.ParseNot: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoNot);
end;

function  TPas2C64_Parser.ParseMultiply: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoMul);
end;

function  TPas2C64_Parser.ParseDivide: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoDiv);
end;

function  TPas2C64_Parser.ParseIntDiv: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoIntDiv);
end;

function  TPas2C64_Parser.ParseIntMod: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoIntMod);
end;

function  TPas2C64_Parser.ParseAnd: TExpressionNodeList;
begin
  Result := ParseSignedFactor;
  Result := FExpression.AddOperator(eoAnd);
end;

function  TPas2C64_Parser.ParseTerm: TExpressionNodeList;
begin
  Result := ParseSignedFactor;

  while Token.TokenType in [Token_times,Token_slash,Token_div,Token_mod,Token_and] do
  begin
    if      Accept(Token_times) then Result := ParseMultiply
    else if Accept(Token_slash) then Result := ParseDivide
    else if Accept(Token_div)   then Result := ParseIntDiv
    else if Accept(Token_mod)   then Result := ParseIntMod
    else if Accept(Token_and)   then Result := ParseAnd;
  end;
end;

function  TPas2C64_Parser.ParseAdd: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoAdd);
end;

function  TPas2C64_Parser.ParseSubtract: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoSub);
end;

function  TPas2C64_Parser.ParseOr: TExpressionNodeList;
begin
  Result := ParseTerm;
  Result := FExpression.AddOperator(eoOr);
end;

function  TPas2C64_Parser.ParseExpression(const aNewExpression: Boolean): TExpressionNodeList;
begin
  try
    if aNewExpression then FExpression.Clear;

    Result := ParseTerm;

    while Token.TokenType in [Token_plus,Token_minus,Token_or] do
    begin
      if      Accept(Token_plus)  then Result := ParseAdd
      else if Accept(Token_minus) then Result := ParseSubtract
      else if Accept(Token_or)    then Result := ParseOr;
    end;
  except
    on E:Exception do
      Error(E.Message);
  end;
end;

// statements

procedure TPas2C64_Parser.ParseWriteMemB;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TNumType;

  ValueStr: AnsiString;
  ValueNum: Int64;
  ValueType: TNumType;
begin
  Expect(Token_lparen);
  AddrStr := GetToken.TokenValue;
  GetIntegerTypeAndValue(AddrStr,AddrType,AddrNum);
  if not IntegerInRange(AddrNum,$0000,$FFFF) then Error('WriteMemB: Address out of range [$0000,$FFFF]');

  Expect(Token_comma);

  ValueStr := GetToken.TokenValue;
  GetIntegerTypeAndValue(ValueStr,ValueType,ValueNum);
  if not IntegerInRange(ValueNum,$00,$FF) then Error('WriteMemB: Value out of range [$00,$FF]');

  Expect(Token_rparen);
  Expect(Token_semicolon);

  FCodeGen.LoadReg_IM(regA,ValueNum);
  FCodeGen.StoreReg(regA,AddrNum);
end;

procedure TPas2C64_Parser.ParseCopyMemB;
var
  SrcAddrStr: AnsiString;
  SrcAddrNum: Int64;
  SrcAddrType: TNumType;

  DstAddrStr: AnsiString;
  DstAddrNum: Int64;
  DstAddrType: TNumType;
begin
  Expect(Token_lparen);
  SrcAddrStr := GetToken.TokenValue;
  GetIntegerTypeAndValue(SrcAddrStr,SrcAddrType,SrcAddrNum);
  if not IntegerInRange(SrcAddrNum,$0000,$FFFF) then Error('CopyMemB: Source address out of range [$0000,$FFFF]');

  Expect(Token_comma);
  DstAddrStr := GetToken.TokenValue;
  GetIntegerTypeAndValue(DstAddrStr,DstAddrType,DstAddrNum);
  if not IntegerInRange(DstAddrNum,$0000,$FFFF) then Error('CopyMemB: Destination address out of range [$0000,$FFFF]');

  Expect(Token_rparen);
  Expect(Token_semicolon);

  FCodeGen.LoadReg_Mem(regA,SrcAddrNum);
  FCodeGen.StoreReg(regA,DstAddrNum);
end;

procedure TPas2C64_Parser.ParseWriteLn;
var
  v: TToken;
  c,l,d,t: AnsiString;
  C64Float: TC64MemFloat;
  e: TExpressionNodeList;
begin
  l := FCodeGen.NewLabel;
  d := FCodeGen.NewLabel;
  t := FCodeGen.NewLabel;
  c := FCodeGen.NewLabel;

  Expect(Token_lparen);

  e := ParseExpression(True);

  Expect(Token_rparen);

  WriteLn('Expression:');
  e.WriteExpression;

  Exit;
  v := GetToken;

  if v.TokenType = Token_string then
  begin
    FCodeGen.WriteCode(':PrintStringConst("'+v.TokenValue+'")');
{    FCodeGen.LoadReg_IM(regY,0);
    FCodeGen.WriteLabel(l);
    FCodeGen.WriteCode('lda ' + d + ',Y');
    FCodeGen.WriteCode('beq ' + c);
    FCodeGen.WriteCode('jsr $ffd2');
    FCodeGen.WriteCode('iny');
    FCodeGen.WriteCode('jmp ' + l);

//    FCodeGen.WriteCode(':PrintStringAddr('+d+')');
    FCodeGen.WriteCode('jmp ' + c);
    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.text "' + v.TokenValue + '"');
    FCodeGen.WriteCode('.byte 0');
    FCodeGen.WriteLabel(c);}
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

    FCodeGen.WriteComment('print ASCII number');

{    FCodeGen.WriteComment('store address in zero-page');
    FCodeGen.WriteCode('sta $fb');
    FCodeGen.WriteCode('sty $fb + 1');

    FCodeGen.LoadReg_IM(regY,0);
    FCodeGen.WriteLabel(l);
    FCodeGen.WriteCode('lda ($fb),y');
    FCodeGen.WriteCode('beq ' + c);
    FCodeGen.WriteCode('jsr $ffd2');
    FCodeGen.WriteCode('iny');
    FCodeGen.WriteCode('jmp ' + l);}
    FCodeGen.WriteCode(':PrintStringAY()');
    FCodeGen.WriteCode('jmp ' + c);

    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.byte ' + C64FloatToStr(C64Float));
    FCodeGen.WriteLabel(t);
    FCodeGen.WriteCode('.byte 0,0');
    FCodeGen.WriteLabel(c);

{    // convert FAC1 into string and get address in A an Y

    FCodeGen.LoadReg_IM(regY,0);
    FCodeGen.WriteLabel(l);
    FCodeGen.WriteCode('lda ' + d + ',Y');
    FCodeGen.WriteCode('cmp #0');
    FCodeGen.WriteCode('beq ' + c);
    FCodeGen.WriteCode('jsr $ffd2');
    FCodeGen.WriteCode('iny');
    FCodeGen.WriteCode('jmp ' + l);
    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.byte "' + C64FloatToStr(C64Float) + '",0');
    FCodeGen.WriteLabel(c);}
  end;

  Expect(Token_rparen);
  Expect(Token_semicolon);
end;

procedure TPas2C64_Parser.ParseStatement;
begin
  if      Accept(Token_writememb) then ParseWriteMemB
  else if Accept(Token_copymemb)  then ParseCopyMemB
  else if Accept(Token_writeln)   then ParseWriteLn
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
  ConstType: TNumType;
begin
  repeat
    ConstName := Token.TokenValue;

    Expect(Token_ident);
    Expect(Token_eql);

    ConstValue := GetToken;

    GetIntegerTypeAndValue(ConstValue.TokenValue,ConstType,ConstNum);

    Expect(Token_semicolon);

    FCodeGen.WriteLabel('const_' + ConstName);
    FCodeGen.WriteCode('.byte ' + IntToC64Hex(ConstNum,ConstType));
  until Token.TokenType in[Token_const,Token_var,Token_begin];
end;

procedure TPas2C64_Parser.ParseVarDecls;
var
  VarName: AnsiString;
  VarType: AnsiString;
  VarSize: TNumType;
begin
  repeat
    VarName := Token.TokenValue;

    Expect(Token_ident);
    Expect(Token_colon);

    VarType := LowerCase(Token.TokenValue);

    if      VarType = 'byte'    then VarSize := ntUInt8
    else if VarType = 'word'    then VarSize := ntUInt16
    else if VarType = 'integer' then VarSize := ntInt16
    else Error('Var declaration: Illegal type "'+VarType+'"');

    Expect(Token_ident);
    Expect(Token_semicolon);

    FCodeGen.WriteLabel('var_' + VarName);
    FCodeGen.WriteCode('.byte ' + IntToC64Hex(0,VarSize));
  until Token.TokenType in[Token_const,Token_var,Token_begin];
end;

procedure TPas2C64_Parser.ParseInput;
var
  ProgName: AnsiString;
begin
  FCodeGen.ResetLabels;

  DstStream.Seek(0,soFromBeginning);
  FCodeGen.SetOutputStream(DstStream);

  Expect(Token_program);

  ProgName := Token.TokenValue;

  Expect(Token_ident);
  Expect(Token_semicolon);

  if FCodeAddr = $0000 then
    FCodeGen.WriteProgramStart
  else
    FCodeGen.WriteProgramStart(FCodeAddr);

  while Token.TokenType in[Token_var,Token_const] do
  begin
    if Accept(Token_const) then
      ParseConstDecls
    else
    if Accept(Token_var) then
      ParseVarDecls;
  end;

  Expect(Token_begin);

  FCodeGen.WriteMainStart;

  ParseBlock;

  FCodeGen.OpCode(opRTS);

  Expect(Token_end);
  Expect(Token_period);
end;

end.
