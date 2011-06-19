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
  Token_interrupt  : Integer;
  Token_assembler  : Integer;
  Token_incmemb    : Integer;
  Token_setint     : Integer;
  Token_stdirq     : Integer;
//  Token_waitforkey : Integer;

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
    procedure ParseWaitForKey;

    procedure ParseProcedureCall(const aSym: TSymbol);
    procedure ParseAssignment(const aSym: TSymbol);
    procedure ParseProcedureCallOrAssignment(const aIdentifier: String);
    //

    procedure ParseStatement;
    procedure ParseBlock;

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
      Result := Result + ',$' + IntToHex((aNumber shr s) and $FF,2);
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
  Token_interrupt  := RegisterKeywordToken('Interrupt');
  Token_assembler  := RegisterKeywordToken('Assembler');
  Token_incmemb    := RegisterKeywordToken('IncMemB');
  Token_setint     := RegisterKeywordToken('SetInterrupt');
  Token_stdirq     := RegisterKeywordToken('StdIRQ');
//  Token_waitforkey := RegisterKeywordToken('WaitForKey');
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

procedure TPas2C64_Parser.ParseWaitForKey;
begin
  FCodeGen.WriteCode('jsr WaitForKey');
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

  if      Accept(Token_poke)       then ParsePoke
  else if Accept(Token_pokew)      then ParsePokeW
  else if Accept(Token_peek)       then ParsePeek
  else if Accept(Token_peekw)      then ParsePeekW
  else if Accept(Token_copymemb)   then ParseCopyMemB
  else if Accept(Token_writeln)    then ParseWriteLn
  else if Accept(Token_incmemb)    then ParseIncMemB
  else if Accept(Token_stdirq)     then ParseStdIRQ
  else if Accept(Token_setint)     then ParseSetInterrupt
//  else if Accept(Token_waitforkey) then ParseWaitForKey
  else if Accept(Token_ident)      then ParseProcedureCallOrAssignment(TokenValue);
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
  RegisterConstant('STDIRQ'     , '$ea31', Token_intnumber);

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
