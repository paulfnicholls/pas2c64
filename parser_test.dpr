program parser_test;

{$APPTYPE CONSOLE}
uses
  unit_base_parser in 'units\unit_base_parser.pas',
  unit_base_parser_types in 'units\unit_base_parser_types.pas',
  Classes;

procedure Emit(const aStream: TStream; const aMsg: AnsiString);
begin
  aStream.Write(PAnsiChar(aMsg)^,Length(aMsg));
end;

procedure EmitLn(const aStream: TStream; aMsg: AnsiString);
begin
  aMsg := aMsg + cCRLF;
  aStream.Write(PAnsiChar(aMsg)^,Length(aMsg));
end;

procedure Reset(const aStream: TStream);
begin
  aStream.Seek(0,soFromBeginning);
end;

type
  TTestParser = class(TBaseParser)
  private
    procedure ParseConstDecls;
    procedure ParseVarDecls;
  protected
    procedure ParseInput; override;
  public
  end;

procedure TTestParser.ParseConstDecls;
var
  ConstName: AnsiString;
  ConstValue: TToken;
begin
  WriteLn;
  WriteLn('Constants:');
  repeat
    ConstName := Token.TokenValue;

    Expect(Token_ident);
    Expect(Token_eql);

    ConstValue := Token;

    GetToken;
    Expect(Token_semicolon);

    WriteLn(ConstName + '= ' + ConstValue.TokenValue); // just a test, use EmitLn in final parser for output
  until Token.TokenType in[Token_const,Token_var,Token_begin];
end;

procedure TTestParser.ParseVarDecls;
var
  VarName: AnsiString;
  VarType: TToken;
begin
  WriteLn;
  WriteLn('Variables:');
  repeat
    VarName := Token.TokenValue;

    Expect(Token_ident);
    Expect(Token_colon);

    VarType := Token;

    Expect(Token_ident);
    Expect(Token_semicolon);

    WriteLn(VarName + ': ' + VarType.TokenValue); // just a test, use EmitLn in final parser for output
  until Token.TokenType in[Token_const,Token_var,Token_begin];
end;

procedure TTestParser.ParseInput;
var
  ProgName: AnsiString;
begin
  Expect(Token_program);

  ProgName := Token.TokenValue;
  WriteLn('Program name = '+ProgName); // just a test, use EmitLn in final parser for output

  Expect(Token_ident);
  Expect(Token_semicolon);

  while Token.TokenType in[Token_var,Token_const] do
  begin
    if Accept(Token_const) then
      ParseConstDecls
    else
    if Accept(Token_var) then
      ParseVarDecls;
  end;

  Expect(Token_begin);
  Expect(Token_end);
  Expect(Token_period);
end;

const
  cProgram =
  'program ATest_program;' + #13#10 +
  'const' + #13#10 +
  '  x = 1;' + #13#10 +
  '  y = 2;' + #13#10 +
  'var' + #13#10 +
  '  a: Integer;' + #13#10 +
  '  b: Byte;' + #13#10 +
  '  c: Word;' + #13#10 +
  'const' + #13#10 +
  '  _x1 = 1;' + #13#10 +
  'begin' + #13#10 +
  'end.';
var
  Parser: TTestParser;
  SrcStream: TMemoryStream;
  DstStream: TMemoryStream;
begin
  Parser := TTestParser.Create;
  SrcStream := TMemoryStream.Create;
  DstStream := TMemoryStream.Create;
  try
    Emit(SrcStream,cProgram);

    Reset(SrcStream);

    if not Parser.Execute(SrcStream,DstStream) then
      WriteLn(Parser.ErrorMsg);

  finally
    Parser.Free;
    SrcStream.Free;
    DstStream.Free;
  end;

  WriteLn;
  WriteLn('Finished...');
  ReadLn;
end.
