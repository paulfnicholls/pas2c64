unit uParser;

{$MODE Delphi}

interface

uses
  SysUtils,
  Classes;

const
  cCRLF = #13#10;

// tokens
var
  // Generic tokens
  Token_unknown      : Integer;
  Token_eof          : Integer;
  Token_ident        : Integer;
  Token_intnumber    : Integer;
  Token_fracnumber   : Integer;
  Token_string       : Integer;
  Token_colon        : Integer;
  Token_lparen       : Integer;
  Token_rparen       : Integer;
  Token_times        : Integer;
  Token_slash        : Integer;
  Token_plus         : Integer;
  Token_minus        : Integer;
  Token_eql          : Integer;
  Token_neq          : Integer;
  Token_lss          : Integer;
  Token_leq          : Integer;
  Token_gtr          : Integer;
  Token_geq          : Integer;
  Token_semicolon    : Integer;
  Token_becomes      : Integer;
  Token_comma        : Integer;
  Token_period       : Integer;
  Token_dollar       : Integer;
  Token_at           : Integer;
  Token_hash         : Integer;
  Token_comment1     : Integer;
  Token_comment2s    : Integer;
  Token_comment2e    : Integer;
  Token_range        : Integer;

  // Keyword tokens
  Token_var          : Integer;
  Token_proc         : Integer;
  Token_interrupt    : Integer;
  Token_begin        : Integer;
  Token_end          : Integer;
  Token_if           : Integer;
  Token_then         : Integer;
  Token_else         : Integer;
  Token_while        : Integer;
  Token_do           : Integer;
  Token_const        : Integer;
  Token_program      : Integer;
  Token_and          : Integer;
  Token_or           : Integer;
  Token_div          : Integer;
  Token_mod          : Integer;

type
  { TIntNumType }
  TIntNumType = (
    ntSInt8,
    ntUInt8,
    ntSInt16,
    ntUInt16,
    ntSInt32,
    ntUInt32
  );

  { TIntNumTypeRange }
  TIntNumTypeRange = record
    MinVal,MaxVal: Int64;
  end;

const
  { cNumTypeRange }
  cNumTypeRange: array [TIntNumType] of TIntNumTypeRange = (
    (MinVal: - 128;        MaxVal: 127),
    (MinVal: 0;            MaxVal: 255),
    (MinVal: - 32768;      MaxVal: 32767),
    (MinVal: 0;            MaxVal: 65535),
    (MinVal: - 2147483648; MaxVal: 2147483647),
    (MinVal: 0;            MaxVal: 4294967295)
  );

type
  { TToken }
  TToken = record
    TokenValue: String;
    TokenType: Integer;
  end;

  { TCharSet }
  TCharSet = set of Char;

  { TParseException }
  TParseException = class(Exception);

{ --- TBaseParser ----------------------------------------------------------- }
  TBaseParser = class(TObject)
  private
    FSrcStream      : TStream;
    FDstStream      : TStream;
    FErrorMsg       : String;
    FStringChar     : Char;
    FCharacter      : Char;
    FToken          : TToken;
    FAllowBinNumbers: Boolean;
  protected
    // override these if you want to alter the tokens being registered
    procedure RegisterGenericTokens; virtual;
    procedure RegisterKeywordTokens; virtual;
    //

    procedure Error(const aErrorMsg: String);
    procedure Emit(const aMsg: String);
    procedure EmitLn(aMsg: String);

    function  IsDecDigit  (const c: Char): Boolean;
    function  IsHexDigit  (const c: Char): Boolean;
    function  IsBinDigit  (const c: Char): Boolean;
    function  IsAlpha     (const c: Char): Boolean;
    function  IsIdentChar (const c: Char): Boolean;
    function  IsIdentStart(const c: Char): Boolean;
    function  IsWhiteSpace(const c: Char): Boolean;
    function  IsOperator  (const c: Char): Boolean;  virtual;

    function  Accept(const aTokenType: Integer): Boolean;
    procedure Expect(const aTokenType: Integer);

    procedure GetCharacter;
    procedure GetDecNumber;
    procedure GetHexNumber;
    procedure GetBinNumber;
    procedure GetIdent;
    procedure GetString;
    procedure GetOperator; virtual;

    // returns the current token and then retrieves the next one
    function  GetToken: TToken;
    procedure SkipWhiteSpaces;

    procedure ParseInput; virtual; abstract;
  public
    constructor Create;
    destructor  Destroy; override;

    function  Execute(const aSrcStream,aDstStream: TStream): Boolean;

    property Character       : Char    read FCharacter;
    property SrcStream       : TStream read FSrcStream;
    property DstStream       : TStream read FDstStream;
    property StringChar      : Char    read FStringChar      write FStringChar;
    property Token           : TToken  read FToken;
    property ErrorMsg        : String  read FErrorMsg;
    property AllowBinNumbers : Boolean read FAllowBinNumbers write FAllowBinNumbers;
  end;

{ --- Routines -------------------------------------------------------------- }

function  RegisterGenericToken(const aTokenName: String): Integer;
function  RegisterKeywordToken(const aTokenName: String): Integer;
procedure CheckTokenForKeyword(var aToken: TToken);
function  TokenToStr(const aTokenType: Integer): String;
function  BinToInt64(aBin: String): Int64;
function  IntegerInRange(const aNumber,aMin,aMax: Int64): Boolean;
procedure GetIntegerTypeAndValue(const aNumber: String; var aIntNumType: TIntNumType; var aValue: Int64);
procedure ClearAllTokens;

implementation

{ --- Routines -------------------------------------------------------------- }
var
  CurrentTokenType: Integer;
  TokenList: TStringList;
  KeywordList: TStringList;

{$ifdef fpc}
function  CharInSet(const c: Char; const aSet: TCharSet): Boolean;
begin
  Result := c in aSet;
end;

{$endif}

function  NewTokenType: Integer;
begin
  Result := CurrentTokenType;
  Inc(CurrentTokenType);
end;

function  RegisterKeywordToken(const aTokenName: String): Integer;
var
  TokenType: Integer;
begin
  TokenType := NewTokenType;
  KeywordList.AddObject(LowerCase(aTokenName),Pointer(TokenType));

  Result := TokenType;
end;

function  RegisterGenericToken(const aTokenName: String): Integer;
var
  TokenType: Integer;
begin
  TokenType := NewTokenType;
  TokenList.AddObject(LowerCase(aTokenName),Pointer(TokenType));

  Result := TokenType;
end;

procedure CheckTokenForKeyword(var aToken: TToken);
var
  Index: Integer;
begin
  if not KeywordList.Find(LowerCase(aToken.TokenValue),Index) then Exit;

  aToken.TokenType := Integer(KeywordList.Objects[Index]);
end;

function  TokenToStr(const aTokenType: Integer): String;
var
  Index: Integer;
begin
  Result := 'Unknown';

  Index := TokenList.IndexOfObject(Pointer(aTokenType));

  if Index <> -1 then
    Result := TokenList.Strings[Index]
  else
  begin
    Index := KeywordList.IndexOfObject(Pointer(aTokenType));

    if Index <> -1 then
      Result := KeywordList.Strings[Index]
  end;
end;

function BinToInt64(aBin: String): Int64;
var
  i: Integer;
  p: Int64;
begin
  Result := 0;

  if aBin = '' then Exit;

  p := 1;
  i := Length(aBin);
  while (i > 0) and (aBin[i] <> '%') do
  begin
    Result := Result + p * Ord(aBin[i] = '1');
    p := p * 2;
    Dec(i);
  end;
end;

function  IntegerInRange(const aNumber,aMin,aMax: Int64): Boolean;
begin
  Result := (aNumber >= aMin) and
            (aNumber <= aMax);
end;

procedure GetIntegerTypeAndValue(const aNumber: String; var aIntNumType: TIntNumType; var aValue: Int64);
var
  i: TIntNumType;
  v, c: Integer;
begin
  if aNumber = '' then
  begin
    aIntNumType := ntUInt8;
    aValue   := 0;
    Exit;
  end;

  c := 0;

  if aNumber[1] = '%' then
    v := BinToInt64(aNumber)
  else
    Val(aNumber, v, c);

  if c <> 0 then
    raise TParseException.Create('Invalid number');

  for i := Low(TIntNumType) to High(TIntNumType) do
    if IntegerInRange(v, cNumTypeRange[i].MinVal,cNumTypeRange[i].MaxVal) then
    begin
      aIntNumType := i;
      AValue := v;
      Exit;
    end;

  raise TParseException.Create('Invalid number');
end;

{ --- TBaseParser ----------------------------------------------------------- }
constructor TBaseParser.Create;
begin
  inherited Create;

  FAllowBinNumbers := True;
  FStringChar      := '''';

  Token_unknown      := RegisterGenericToken('Unknown');
  Token_eof          := RegisterGenericToken('EOF');
  Token_ident        := RegisterGenericToken('Ident');
  Token_intnumber    := RegisterGenericToken('Integer');
  Token_fracnumber   := RegisterGenericToken('Single');
  Token_string       := RegisterGenericToken('String');

  RegisterGenericTokens;
  RegisterKeywordTokens;

  FToken.TokenValue := 'Unknown';
  FToken.TokenType  := Token_unknown;
end;

destructor  TBaseParser.Destroy;
begin
  ClearAllTokens;

  inherited Destroy;
end;

procedure TBaseParser.Error(const aErrorMsg: String);
begin
  raise TParseException.Create(aErrorMsg);
end;

procedure TBaseParser.Emit(const aMsg: String);
begin
  if not Assigned(FDstStream) then Exit;

  FDstStream.Write(PChar(aMsg)^,Length(aMsg)*SizeOf(Char));
end;

procedure TBaseParser.EmitLn(aMsg: String);
begin
  if not Assigned(FDstStream) then Exit;

  aMsg := aMsg + cCRLF;
  FDstStream.Write(PChar(aMsg)^,Length(aMsg)*SizeOf(Char));
end;

function  TBaseParser.IsDecDigit(const c: Char): Boolean;
begin
  Result := CharInSet(c,['0'..'9']);
end;

function  TBaseParser.IsHexDigit(const c: Char): Boolean;
begin
  Result := CharInSet(c,['0'..'9','a'..'z','A'..'Z']);
end;

function  TBaseParser.IsBinDigit(const c: Char): Boolean;
begin
  Result := CharInSet(c,['0'..'1']);
end;

function  TBaseParser.IsAlpha(const c: Char): Boolean;
begin
  Result := CharInSet(c,['a'..'z','A'..'Z']);
end;

function  TBaseParser.IsIdentChar(const c: Char): Boolean;
begin
  Result := CharInSet(c,['0'..'9','_','a'..'z','A'..'Z']);
end;

function  TBaseParser.IsIdentStart(const c: Char): Boolean;
begin
  Result := CharInSet(c,['_','a'..'z','A'..'Z']);
end;

function  TBaseParser.IsWhiteSpace(const c: Char): Boolean;
begin
  Result := CharInSet(c,[' ',#9,#13,#10]);
end;

function  TBaseParser.IsOperator(const c: Char): Boolean;
begin
  Result := CharInSet(c,['=','<','>','-','+','*','/',':']);
end;

procedure TBaseParser.GetCharacter;
var
  c: AnsiChar;
begin
  FCharacter := #0;
  if FSrcStream.Position < FSrcStream.Size then
  begin
    FSrcStream.Read(c,SizeOf(c));
    FCharacter := WideChar(c);
  end;
end;

procedure TBaseParser.GetDecNumber;
begin
  if not IsDecDigit(FCharacter) then Error('Number expected');

  FToken.TokenType  := Token_intnumber;
  FToken.TokenValue := '';

  while (FCharacter <> #0) and IsDecDigit(FCharacter) do
  begin
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;
  end;

  if FCharacter = '.' then
  begin
    FToken.TokenType  := Token_fracnumber;
  // get fractional part of number
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;

    while (FCharacter <> #0) and IsDecDigit(FCharacter) do
    begin
      FToken.TokenValue := FToken.TokenValue + FCharacter;
      GetCharacter;
    end;
  end;

  // get any scientific part of number
  if LowerCase(FCharacter) = 'e' then
  begin
    // parse 'e' part
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;

    // parse +/- number part
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;

    while (FCharacter <> #0) and IsDecDigit(FCharacter) do
    begin
      FToken.TokenValue := FToken.TokenValue + FCharacter;
      GetCharacter;
    end;
  end;

  SkipWhiteSpaces;
end;

procedure TBaseParser.GetHexNumber;
begin
  if FCharacter <> '$' then Error('Number expected');

  FToken.TokenType  := Token_intnumber;
  FToken.TokenValue := FCharacter;

  GetCharacter;

  while (FCharacter <> #0) and IsHexDigit(FCharacter) do
  begin
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;
  end;
  SkipWhiteSpaces;
end;

procedure TBaseParser.GetBinNumber;
begin
  if FCharacter <> '%' then Error('Number expected');

  FToken.TokenType  := Token_intnumber;
  FToken.TokenValue := FCharacter;

  GetCharacter;

  while (FCharacter <> #0) and IsBinDigit(FCharacter) do
  begin
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;
  end;
  SkipWhiteSpaces;
end;

procedure TBaseParser.GetIdent;
begin
  if not IsIdentStart(FCharacter) then Error('Identifier expected');

  FToken.TokenType  := Token_ident;
  FToken.TokenValue := '';

  while (FCharacter <> #0) and IsIdentChar(FCharacter) do
  begin
    FToken.TokenValue := FToken.TokenValue + FCharacter;
    GetCharacter;
  end;
  SkipWhiteSpaces;
end;

procedure TBaseParser.GetString;
var
  EOFString: Boolean;
begin
  if FCharacter <> FStringChar then Error('String expected');

  FToken.TokenType  := Token_string;
  FToken.TokenValue := '';

  GetCharacter;

  EOFString := False;

  while not EOFString do
  begin
    while (FCharacter <> #0) and (FCharacter <> FStringChar) do
    begin
      FToken.TokenValue := FToken.TokenValue + FCharacter;
      GetCharacter;
    end;
    GetCharacter;

    if FCharacter = FStringChar then
    begin
      FToken.TokenValue := FToken.TokenValue + FCharacter;
      GetCharacter;
    end
    else
      EOFString := True;
  end;
  SkipWhiteSpaces;
end;

procedure TBaseParser.GetOperator;
begin
  FToken.TokenValue := FCharacter;
  if CharInSet(FCharacter,  ['=','-','+','*','/']) then
  begin
    Case FCharacter Of
      '=' : FToken.TokenType := Token_eql;
      '-' : FToken.TokenType := Token_minus;
      '+' : FToken.TokenType := Token_plus;
      '*' : FToken.TokenType := Token_times;
      '/' : FToken.TokenType := Token_slash;
    end;
    GetCharacter;
    if (FToken.TokenType = Token_slash) and (FCharacter = '/') then
    begin
      FToken.TokenValue := '//';
      FToken.TokenType  := Token_comment1;
      GetCharacter;
    end;
  end
  else
  if FCharacter = ':' then
  // could be ':', or ':='
  begin
    FToken.TokenType := Token_colon;
    GetCharacter;
    if FCharacter = '=' then
    begin
      FToken.TokenValue := ':=';
      FToken.TokenType  := Token_becomes;
      GetCharacter;
    end;
  end
  else
  if FCharacter = '<' then
  // could be '<', or '<=', or '<>'
  begin
    FToken.TokenType := Token_lss;
    GetCharacter;
    if FCharacter = '=' then
    begin
      FToken.TokenValue := '<=';
      FToken.TokenType  := Token_leq;
      GetCharacter;
    end
    else
    if FCharacter = '>' then
    // is '<>'
    begin
      FToken.TokenValue := '<>';
      FToken.TokenType  := Token_neq;
      GetCharacter;
    end;;
  end
  else
  if FCharacter = '>' then
  // could be '>', or '>='
  begin
    FToken.TokenType := Token_gtr;
    GetCharacter;
    if FCharacter = '=' then
    begin
      FToken.TokenValue := '>=';
      FToken.TokenType  := Token_geq;
      GetCharacter;
    end;
  end;
  SkipWhiteSpaces;
end;

function  TBaseParser.GetToken: TToken;
begin
  Result := FToken;

  FToken.TokenValue := 'Unknown';
  FToken.TokenType  := Token_unknown;

  if FCharacter = #0 then
  begin
    FToken.TokenValue := 'EOF';
    FToken.TokenType  := Token_eof;
  end
  else
  if IsIdentStart(FCharacter) then
  begin
    GetIdent;
    CheckTokenForKeyword(FToken);
  end
  else
  if FCharacter = '$' then
    GetHexNumber
  else
  If FAllowBinNumbers and (FCharacter = '%') then
    GetBinNumber
  else
  if IsDecDigit(FCharacter) then
    GetDecNumber
  else
  if FCharacter = FStringChar then
    GetString
  else
  If IsOperator(FCharacter) then
    GetOperator
  else
  begin
    FToken.TokenValue := FCharacter;
    case FCharacter of
      ',' : FToken.TokenType := Token_comma;
      ';' : FToken.TokenType := Token_semicolon;
      '(' : FToken.TokenType := Token_lparen;
      ')' : FToken.TokenType := Token_rparen;
      '.' : FToken.TokenType := Token_period;
      '@' : FToken.TokenType := Token_at;
      '#' : FToken.TokenType := Token_hash;
    else
    end;
    GetCharacter;
    if (FToken.TokenType = Token_period) and (FCharacter = '.') then
    begin
      FToken.TokenValue := '..';
      FToken.TokenType  := Token_range;
      GetCharacter;
    end;
    SkipWhiteSpaces;
  end;
end;

procedure TBaseParser.SkipWhiteSpaces;
begin
  while IsWhiteSpace(FCharacter) do
    GetCharacter;
end;

function  TBaseParser.Accept(const aTokenType: Integer): Boolean;
begin
  if FToken.TokenType =  aTokenType then
  begin
    GetToken;
    Result :=  True;
  end
  else
    Result :=  False;
end;

procedure TBaseParser.Expect(const aTokenType: Integer);
begin
  if FToken.TokenType =  aTokenType then
    GetToken
  else
  begin
    if FToken.TokenType <> aTokenType then
      Error('Expected token "'+TokenToStr(aTokenType)+'", found token "'+TokenToStr(FToken.TokenType)+'" instead');
  end;
end;

function  TBaseParser.Execute(const aSrcStream,aDstStream: TStream): Boolean;
begin
  FSrcStream := aSrcStream;
  FDstStream := aDstStream;

  FErrorMsg := '';
  Result    := True;

  // initialize parser
  GetCharacter;

  SkipWhiteSpaces;
  GetToken;

  try
    ParseInput; // override this to make a proper parser
  except
    on E:Exception do
    begin
      FErrorMsg := E.Message;
      Result    := False;
    end;
  end;
end;

procedure TBaseParser.RegisterGenericTokens;
begin
  Token_colon        := RegisterGenericToken(':');
  Token_lparen       := RegisterGenericToken('(');
  Token_rparen       := RegisterGenericToken(')');
  Token_times        := RegisterGenericToken('*');
  Token_slash        := RegisterGenericToken('/');
  Token_plus         := RegisterGenericToken('+');
  Token_minus        := RegisterGenericToken('-');
  Token_eql          := RegisterGenericToken('=');
  Token_neq          := RegisterGenericToken('<>');
  Token_lss          := RegisterGenericToken('<');
  Token_leq          := RegisterGenericToken('<=');
  Token_gtr          := RegisterGenericToken('>');
  Token_geq          := RegisterGenericToken('>=');
  Token_semicolon    := RegisterGenericToken(';');
  Token_becomes      := RegisterGenericToken(':=');
  Token_comma        := RegisterGenericToken(',');
  Token_period       := RegisterGenericToken('.');
  Token_dollar       := RegisterGenericToken('$');
  Token_at           := RegisterGenericToken('@');
  Token_hash         := RegisterGenericToken('#');
  Token_comment1     := RegisterGenericToken('//');
  Token_comment2s    := RegisterGenericToken('{');
  Token_comment2e    := RegisterGenericToken('}');
  Token_range        := RegisterGenericToken('..');
end;

procedure TBaseParser.RegisterKeywordTokens;
begin
  Token_var          := RegisterKeywordToken('var');
  Token_proc         := RegisterKeywordToken('procedure');
  Token_interrupt    := RegisterKeywordToken('interrupt');
  Token_begin        := RegisterKeywordToken('begin');
  Token_end          := RegisterKeywordToken('end');
  Token_if           := RegisterKeywordToken('if');
  Token_then         := RegisterKeywordToken('then');
  Token_else         := RegisterKeywordToken('else');
  Token_while        := RegisterKeywordToken('while');
  Token_do           := RegisterKeywordToken('do');
  Token_const        := RegisterKeywordToken('const');
  Token_program      := RegisterKeywordToken('program');
  Token_and          := RegisterKeywordToken('and');
  Token_or           := RegisterKeywordToken('or');
  Token_div          := RegisterKeywordToken('div');
  Token_mod          := RegisterKeywordToken('mod');
end;

procedure ClearAllTokens;
begin
  TokenList.Clear;
  KeywordList.Clear;
end;

initialization
  CurrentTokenType := 0;

  TokenList := TStringList.Create;
  TokenList.Sorted := False;

  KeywordList := TStringList.Create;
  KeywordList.Sorted := True;
  KeywordList.Duplicates := dupError;

finalization
  TokenList.Free;
  KeywordList.Free;

end.
