unit unit_pas2c64_operators;

{$mode delphi}

interface

uses
  SysUtils,
  unit_pas2c64_types,
  unit_pas2c64_CodeGenerator;

type
  OperatorException = class(Exception);

  TOperatorType = (
    otNeg,
    otNot,
    otMul,
    otDiv,
    otSub,
    otAdd,
    otIntDiv,
    otIntMod,
    otAnd,
    otOr,
    otXor,
    otShl,
    otShr
  );

  TOperator = class
  private
    FOperatorType: TOperatorType;
    FOperandCount: Integer;
  protected
    procedure Error(const aErrorMsg: String);
  public
    constructor Create(const aOperatorType: TOperatorType; const aOperandCount: Integer);

    function  Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean; virtual; abstract;
    procedure EmitCode(const aCodeGen: TCodeGenerator_C64);                                  virtual; abstract;

    property OperatorType: TOperatorType read FOperatorType;
    property OperandCount: Integer       read FOperandCount;
  end;

  TNegOperator = class(TOperator)
  public
    constructor Create; reintroduce;

    function  Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean; override;
    procedure EmitCode(const aCodeGen: TCodeGenerator_C64);                                  override;
  end;

  TNotOperator = class(TOperator)
  public
    constructor Create; reintroduce;

    function  Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean; override;
    procedure EmitCode(const aCodeGen: TCodeGenerator_C64);                                  override;
  end;

  TAddOperator = class(TOperator)
  public
    constructor Create; reintroduce;

    function  Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean; override;
    procedure EmitCode(const aCodeGen: TCodeGenerator_C64);                                  override;
  end;

  TSubOperator = class(TOperator)
  public
    constructor Create; reintroduce;

    function  Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean; override;
    procedure EmitCode(const aCodeGen: TCodeGenerator_C64);                                  override;
  end;

implementation


constructor TOperator.Create(const aOperatorType: TOperatorType; const aOperandCount: Integer);
begin
  inherited Create;

  FOperatorType := aOperatorType;
  FOperandCount := aOperandCount;
end;

procedure TOperator.Error(const aErrorMsg: String);
begin
  raise OperatorException.Create(aErrorMsg);
end;

//
// concrete operator classes
//
constructor TNegOperator.Create;
begin
  inherited Create(otNeg,1);
end;

function  TNegOperator.Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean;
begin
  Result := False;

  if not (b.PrimitiveType in [ptWord,ptInteger,ptSingle]) then
    Error('Unary subtract operation: Incompatible type "'+PrimitiveTypeToStr(b.PrimitiveType)+'"');

  if b.PrimitiveClass <> pcConstant then Exit; // only evaluate constants

  case b.PrimitiveType of
    ptWord    : r := NewWordPrimitiveTypeConstant   ((b.vWord xor $ffff) + 1);
    ptInteger : r := NewIntegerPrimitiveTypeConstant(-b.vInteger);
    ptBoolean : r := NewBooleanPrimitiveTypeConstant(-b.vSingle);
  end;

  Result := True;
end;

procedure TNegOperator.EmitCode(const aCodeGen: TCodeGenerator_C64);
begin
end;

constructor TNotOperator.Create;
begin
  inherited Create(otNot,1);
end;

function  TNotOperator.Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean;
begin
  Result := False;

  if not (b.PrimitiveType in [ptWord,ptInteger,ptBoolean]) then
    Error('Not operation: Incompatible type "'+PrimitiveTypeToStr(b.PrimitiveType)+'"');

  if b.PrimitiveClass <> pcConstant then Exit; // only evaluate constants

  case b.PrimitiveType of
    ptWord    : r := NewWordPrimitiveTypeConstant   (not b.vWord);
    ptInteger : r := NewIntegerPrimitiveTypeConstant(not b.vInteger);
    ptBoolean : r := NewBooleanPrimitiveTypeConstant(not b.vBoolean);
  end;

  Result := True;
end;

procedure TNotOperator.EmitCode(const aCodeGen: TCodeGenerator_C64);
begin
end;

constructor TAddOperator.Create;
begin
  inherited Create(otAdd,2);
end;

function  TAddOperator.Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean;
begin
  Result := False;

  if not(a.PrimitiveType in [ptWord,ptInteger,ptBoolean]) and
     not(b.PrimitiveType in [ptWord,ptInteger,ptBoolean])then
    Error('Not operation: Incompatible type(s) -  "'+PrimitiveTypeToStr(b.PrimitiveType)+'"');

  if (a.PrimitiveClass <> pcConstant) or
     (b.PrimitiveClass <> pcConstant) then Exit; // only evaluate constants

  case b.PrimitiveType of
    ptWord    : r := NewWordPrimitiveTypeConstant   (a.vWord + b.vWord);
    ptInteger : r := NewIntegerPrimitiveTypeConstant(not b.vInteger);
    ptBoolean : r := NewBooleanPrimitiveTypeConstant(not b.vBoolean);
  end;

  Result := True;
end;

procedure TAddOperator.EmitCode(const aCodeGen: TCodeGenerator_C64);
begin

end;

constructor TSubOperator.Create;
begin
  inherited Create(otSub,2);
end;

function  TSubOperator.Simplify(const a,b: TPrimitiveTypeValue; out r: TPrimitiveTypeValue): Boolean;
begin
  Result := False;
end;

procedure TSubOperator.EmitCode(const aCodeGen: TCodeGenerator_C64);
begin

end;

end.
