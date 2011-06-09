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
  unit_pas2c64_symboltable;

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
end;

constructor TPas2C64_Parser.Create;
begin
  inherited Create;

  FCodeGen  := TCodeGenerator_C64.Create;
  FCodeAddr := $0000;
end;

destructor  TPas2C64_Parser.Destroy;
begin
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
  c,l,d: AnsiString;
  C64Float: TC64MemFloat;
begin
  l := FCodeGen.NewLabel;
  d := FCodeGen.NewLabel;
  c := FCodeGen.NewLabel;

  Expect(Token_lparen);
  v := GetToken;

  if v.TokenType = Token_string then
  begin
    FCodeGen.LoadReg_IM(regY,0);
    FCodeGen.WriteLabel(l);
    FCodeGen.WriteCode('LDA ' + d + ',Y');
    FCodeGen.WriteCode('CMP #0');
    FCodeGen.WriteCode('BEQ ' + c);
    FCodeGen.WriteCode('JSR $FFD2');
    FCodeGen.WriteCode('INY');
    FCodeGen.WriteCode('JMP ' + l);
    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.byte "' + v.TokenValue + '",0');
    FCodeGen.WriteLabel(c);
  end
  else
  if v.TokenType = Token_number then
  begin
    FloatToC64Float(StrToFloat(v.TokenValue),C64Float);

    FCodeGen.WriteComment('load floating point number into FAC1');
    FCodeGen.WriteCode('LDA <' + d);
    FCodeGen.WriteCode('LDY >' + d);
    FCodeGen.WriteCode('JSR $BBA2');
    FCodeGen.WriteCode('');

    FCodeGen.WriteComment('convert number in FAC1 to ASCII (pointer in A & Y)');
    FCodeGen.WriteCode('JSR $BDDD');
    FCodeGen.WriteCode('');

    FCodeGen.WriteComment('How to print string at location in A & Y to screen using $FFD2?');
    FCodeGen.WriteComment('$FFD2 expects character in A?');
    FCodeGen.WriteCode('');

    FCodeGen.WriteCode('JMP ' + c);
    FCodeGen.WriteLabel(d);
    FCodeGen.WriteCode('.byte "' + C64FloatToStr(C64Float) + '",0');
    FCodeGen.WriteLabel(c);

{    // convert FAC1 into string and get address in A an Y

    FCodeGen.LoadReg_IM(regY,0);
    FCodeGen.WriteLabel(l);
    FCodeGen.WriteCode('LDA ' + d + ',Y');
    FCodeGen.WriteCode('CMP #0');
    FCodeGen.WriteCode('BEQ ' + c);
    FCodeGen.WriteCode('JSR $FFD2');
    FCodeGen.WriteCode('INY');
    FCodeGen.WriteCode('JMP ' + l);
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

{  FCodeGen.OpA(opLDA);          // OPC A
  FCodeGen.OpAbs(opLDA,$5511);  // OPC $HHLL
  FCodeGen.OpAbsX(opLDA,$5511); // OPC $HHLL,X
  FCodeGen.OpAbsY(opLDA,$5511); // OPC $HHLL,Y
  FCodeGen.OpImmed(opLDA,$55);  // OPC #$BB
  FCodeGen.OpImpl(opLDA);       // OPC
  FCodeGen.OpInd(opLDA,$5511);  // OPC ($HHLL)
  FCodeGen.OpXInd(opLDA,$55);   // OPC ($BB,X)
  FCodeGen.OpIndY(opLDA,$55);   // OPC ($LL),Y
  FCodeGen.OpRel(opLDA,$55);    // OPC $BB
  FCodeGen.OpZpg(opLDA,$55);    // OPC $LL
  FCodeGen.OpZpgX(opLDA,$55);   // OPC $LL,X
  FCodeGen.OpZpgY(opLDA,$55);   // OPC $LL,Y

  Exit;}
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
