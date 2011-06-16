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
  Token_interrupt : Integer;
  Token_incmemb   : Integer;
  Token_setint    : Integer;
  Token_stdirq    : Integer;

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

    function  IsIdent(const aStr: String): Boolean;
    procedure GetConstInfo(const aToken: TToken;
                           out   aSymClass,aSymSubClass: Integer;
                           out   aConstNum: Int64;
                           out   aConstType: TIntNumType);

    procedure SetExpressionMode(const aMode: TExpressionMode);
    function  GetTokenTypeFromSymbol(const aSym: TSymbol): Integer;
    function  ResolveIdentifier(const aIdent: TToken): TToken;

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
    function  ParseExpression(const aNewExpression: Boolean;
                              const aMode: TExpressionMode = emAllIdents): TExpressionNodeList;

    // statements types
    procedure ParseWriteMemB;
    procedure ParseCopyMemB;
    procedure ParseIncMemB;
    procedure ParseStdIRQ;
    procedure ParseSetInterrupt;
    procedure ParseWriteLn;

    procedure ParseProcedureCall(const aSym: TSymbol);
    procedure ParseAssignment(const aSym: TSymbol);
    procedure ParseProcedureCallOrAssignment(const aIdentifier: String);
    //

    procedure ParseStatement;
    procedure ParseBlock;

    procedure ParseConstDecls;
    procedure ParseVarDecls;

    procedure ParseProcDecl;
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
    Result := Result + '$' + IntToHex((aNumber shr s) and $FF,2) + ' ';
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

  Token_writememb := RegisterKeywordToken('WriteMemB');
  Token_writememw := RegisterKeywordToken('WriteMemW');
  Token_readmemb  := RegisterKeywordToken('ReadMemB');
  Token_copymemb  := RegisterKeywordToken('CopyMemB');
  Token_write     := RegisterKeywordToken('Write');
  Token_writeln   := RegisterKeywordToken('WriteLn');
  Token_interrupt := RegisterKeywordToken('Interrupt');
  Token_incmemb   := RegisterKeywordToken('IncMemB');
  Token_setint    := RegisterKeywordToken('SetInterrupt');
  Token_stdirq    := RegisterKeywordToken('StdIRQ');
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

  if Sym.SymbolClass <> cSymClass_Constant then Exit;

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
      v := ResolveIdentifier(v);

    Result := FExpression.AddOperand(v.TokenValue,v.TokenType);
  end;
end;

function  TPas2C64_Parser.ParseSignedFactor: TExpressionNodeList;
var
  NegativeFactor: Boolean;
  LastCount: Integer;
  DeltaCount: Integer;
  Operand: TExpressionOperandNode;
begin
  NegativeFactor := False;

  if Token.TokenType in [Token_plus,Token_minus] then
  begin
    LastCount := FExpression.GetNodeCount;

    NegativeFactor := (Token.TokenType = Token_minus);

    Expect(Token.TokenType);
  end;

  Result := ParseFactor;

  if NegativeFactor then
  begin
    DeltaCount := FExpression.GetNodeCount - LastCount;

    if DeltaCount > 1 then
      Result := FExpression.AddOperator(eoNeg)
    else
    begin
      Operand := TExpressionOperandNode(FExpression.GetNode(FExpression.GetNodeCount - 1));
      if Operand.OperandType = Token_ident then
        Result := FExpression.AddOperator(eoNeg)
      else
        Operand.OperandValue := '-' + Operand.OperandValue;
    end;
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

function  TPas2C64_Parser.ParseExpression(const aNewExpression: Boolean;
                                          const aMode: TExpressionMode = emAllIdents): TExpressionNodeList;
begin
  if aNewExpression then FExpression.Clear;

  SetExpressionMode(aMode);

  Result := ParseTerm;

  while Token.TokenType in [Token_plus,Token_minus,Token_or] do
  begin
    if      Accept(Token_plus)  then Result := ParseAdd
    else if Accept(Token_minus) then Result := ParseSubtract
    else if Accept(Token_or)    then Result := ParseOr;
  end;
end;

// statements

procedure TPas2C64_Parser.ParseWriteMemB;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;

  ValueStr: AnsiString;
  ValueNum: Int64;
  ValueType: TIntNumType;
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
  SrcAddrType: TIntNumType;

  DstAddrStr: AnsiString;
  DstAddrNum: Int64;
  DstAddrType: TIntNumType;
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

procedure TPas2C64_Parser.ParseIncMemB;
var
  AddrStr: AnsiString;
  AddrNum: Int64;
  AddrType: TIntNumType;
begin
  Expect(Token_lparen);

  AddrStr := GetToken.TokenValue;
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
  FCodeGen.WriteCode('lda #<int_'+IntName+'       // low byte of int_'+IntName+' start addr');
  FCodeGen.WriteCode('ldx #>int_'+IntName+'       // high byte of int_'+IntName+' start addr');
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

procedure TPas2C64_Parser.ParseProcedureCall(const aSym: TSymbol);
begin
  if aSym.SymbolClass <> cSymClass_Procedure then Error('Illegal procedure call: Identifier "'+aSym.SymbolName+'" is not a procedure');

  FCodeGen.WriteCode('jsr proc_' + aSym.SymbolName);
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

  if      Accept(Token_writememb) then ParseWriteMemB
  else if Accept(Token_copymemb)  then ParseCopyMemB
  else if Accept(Token_writeln)   then ParseWriteLn
  else if Accept(Token_incmemb)   then ParseIncMemB
  else if Accept(Token_stdirq)    then ParseStdIRQ
  else if Accept(Token_setint)    then ParseSetInterrupt
  else if Accept(Token_ident)     then ParseProcedureCallOrAssignment(TokenValue);
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

    // TODO: don't just get first expression node, evaluate it!
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
  if Accept(Token_interrupt) then
  // is interrupt routine
  begin
    FSymbolTable.AddSymbol(ProcName,'',cSymClass_Interrupt,cSymSubClass_None);
    IsInterrupt := True;
  end
  else
  // is regular procedure
    FSymbolTable.AddSymbol(ProcName,'',cSymClass_Procedure,cSymSubClass_None);

  if IsInterrupt then
    Expect(Token_semicolon);

  Expect(Token_begin);

  if IsInterrupt then
    FCodeGen.WriteLabel('int_' + ProcName)
  else
    FCodeGen.WriteLabel('proc_' + ProcName);

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

end.
