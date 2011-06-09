unit uParserPL0;

interface

uses
  uParser;

type
  {TParserPL0}

  TParserPL0 = class(TBaseParser)
  private
    procedure Factor;
    procedure Term;
    procedure Expression;
    procedure Condition;
    procedure Statement;
    procedure Block;
  protected
    procedure RegisterKeywordTokens; override;
    procedure ParseInput; override;
  public
  end;

implementation

var
  Token_odd  : Integer;
  Token_call : Integer;

procedure TParserPL0.Factor;
begin
  if Accept(Token_ident) then
  begin
  end
  else
  if Accept(Token_number) then
  begin
  end
  else
  if Accept(Token_lparen) then
  begin
    Expression;
    Expect(Token_rparen);
  end
  else
  begin
    Error('factor: syntax error');
    GetToken;
  end;
end;

procedure TParserPL0.Term;
begin
  Factor;

  while Token.TokenType in [Token_times,Token_slash] do
  begin
    GetToken;
    Factor;
  end;
end;

procedure TParserPL0.Expression;
begin
    if Token.TokenType in [Token_plus,Token_minus] then
        GetToken;

    Term;
    while Token.TokenType in [Token_plus,Token_minus] do
    begin
        GetToken;
        Term;
    end;
end;

procedure TParserPL0.Condition;
begin
    if Accept(Token_odd) then
    begin
        Expression;
    end
    else
    begin
        Expression;

        if Token.TokenType in [Token_eql,Token_neq,Token_lss,Token_leq,Token_gtr,Token_geq] then
        begin
            GetToken;
            Expression;
        end
        else
        begin
            Error('Condition: invalid operator');
            GetToken;
        end;
    end;
end;

procedure TParserPL0.Statement;
begin
    if Accept(Token_ident) then
    begin
        Expect(Token_becomes);
        Expression;
    end
    else
    if Accept(Token_call) then
    begin
        Expect(Token_ident);
    end
    else
    if Accept(Token_begin) then
    begin
        repeat
            Statement;
        until not Accept(Token_semicolon);

        Expect(Token_end);
    end
    else
    if Accept(Token_if) then
    begin
        Condition;
        Expect(Token_then);
        Statement;
    end
    else
    if Accept(Token_while) then
    begin
        Condition;
        Expect(Token_do);
        statement;
    end;
end;

procedure TParserPL0.Block;
begin
  if Accept(Token_const) then
  begin
      repeat
          Expect(Token_ident);
          Expect(Token_eql);
          Expect(Token_number);
      until not Accept(Token_comma);

      Expect(Token_semicolon);
  end;

  if Accept(Token_var) then
  begin
      repeat
          Expect(Token_ident);
      until not Accept(Token_comma);

      Expect(Token_semicolon);
  end;

  while Accept(Token_proc) do
  begin
      Expect(Token_ident);
      Expect(Token_semicolon);
      Block;
      Expect(Token_semicolon);
  end;

  Statement;
end;

procedure TParserPL0.ParseInput;
begin
  Block;
  Expect(Token_period);
end;

procedure TParserPL0.RegisterKeywordTokens;
begin
  inherited RegisterKeywordTokens;

  Token_odd  := RegisterKeywordToken('odd');
  Token_call := RegisterKeywordToken('call');
end;

end.
