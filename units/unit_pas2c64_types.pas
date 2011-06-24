unit unit_pas2c64_types;

{$mode delphi}

interface

//
// primitive types supported in pas2c64:
// -------------------------------------------
// Integer (16-Bit signed integers)
// Word    (16-Bit unsigned integer)
// Single  (32-Bit floating point number)
// Boolean (True or False; internally represented by $ff and $00 respectively)
// String  (255 maximum characters length strings)
//
type
  TInt16     = Smallint;    // 16-Bit signed integer
  TString255 = String[255]; // strings are max 255 characters

  TPrimitiveType = (
    ptInteger,
    ptWord,
    ptSingle,
    ptBoolean,
    ptString
  );

//
// primitive types can be of a certain class:
// * a constant (128, $ff, -3.5e-2, 'hello world')
// * a variable in memory
// * a function result type
  TPrimitiveClass = (
    pcConstant,
    pcVariable,
    pcFunction
  );

  PPrimitiveTypeValue = ^TPrimitiveTypeValue;
  TPrimitiveTypeValue = packed record
    ValueStr: String; // string representation of this primitive value (if applicable)
    PrimitiveClass: TPrimitiveClass;

    case PrimitiveType : TPrimitiveType of
      ptInteger : (vInteger : TInt16);
      ptWord    : (vWord    : Word);
      ptSingle  : (vSingle  : Single);
      ptBoolean : (vBoolean : Boolean);
      ptString  : (vString  : TString255);
  end;

function  PrimitiveTypeToStr(const aPrimitiveType: TPrimitiveType): String;
function  StrToPrimitiveType(const aStr: String; out aPrimitiveType: TPrimitiveType): Boolean;

function  PrimitiveClassToStr(const aPrimitiveClass: TPrimitiveClass): String;
function  StrToPrimitiveClass(const aStr: String; out aPrimitiveClass: TPrimitiveClass): Boolean;

function  NewIntegerPrimitiveTypeConstant(const aValue: Integer): TPrimitiveTypeValue;
function  NewWordPrimitiveTypeConstant   (const aValue: Word): TPrimitiveTypeValue;
function  NewSinglePrimitiveTypeConstant (const aValue: Single): TPrimitiveTypeValue;
function  NewBooleanPrimitiveTypeConstant(const aValue: Boolean): TPrimitiveTypeValue;
function  NewStringPrimitiveTypeConstant (const aValue: String): TPrimitiveTypeValue;

implementation

uses
  SysUtils;

const
  cPrimitiveTypeStr: array[TPrimitiveType] of String = (
    'integer',
    'word',
    'single',
    'boolean',
    'string'
  );

  cPrimitiveClassStr: array[TPrimitiveClass] of String = (
    'constant',
    'variable',
    'function'
  );

function  PrimitiveTypeToStr(const aPrimitiveType: TPrimitiveType): String;
// converts a primitive type to a string
begin
  Result := cPrimitiveTypeStr[aPrimitiveType];
end;

function  StrToPrimitiveType(const aStr: String; out aPrimitiveType: TPrimitiveType): Boolean;
// converts a string to a primitive type, and returns false if it failed
var
  pt: TPrimitiveType;
begin
  Result := False;

  for pt := Low(TPrimitiveType) to High(TPrimitiveType) do
    if LowerCase(cPrimitiveTypeStr[pt]) = LowerCase(aStr) then
    begin
      Result         := True;
      aPrimitiveType := pt;
      Exit;
    end;
end;

function  PrimitiveClassToStr(const aPrimitiveClass: TPrimitiveClass): String;
// converts a primitive class to a string
begin
  Result := cPrimitiveClassStr[aPrimitiveClass];
end;

function  StrToPrimitiveClass(const aStr: String; out aPrimitiveClass: TPrimitiveClass): Boolean;
// converts a string to a primitive class, and returns false if it failed
var
  pc: TPrimitiveClass;
begin
  Result := False;

  for pc := Low(TPrimitiveClass) to High(TPrimitiveClass) do
    if LowerCase(cPrimitiveClassStr[pc]) = LowerCase(aStr) then
    begin
      Result          := True;
      aPrimitiveClass := pc;
      Exit;
    end;
end;


function  NewIntegerPrimitiveTypeConstant(const aValue: Integer): TPrimitiveTypeValue;
begin
  Result.PrimitiveClass := pcConstant;
  Result.PrimitiveType  := ptInteger;
  Result.vInteger       := aValue;
  Result.ValueStr       := IntToStr(aValue);
end;

function  NewWordPrimitiveTypeConstant   (const aValue: Word): TPrimitiveTypeValue;
begin
  Result.PrimitiveClass := pcConstant;
  Result.PrimitiveType  := ptWord;
  Result.vWord          := aValue;
  Result.ValueStr       := IntToStr(aValue);
end;

function  NewSinglePrimitiveTypeConstant (const aValue: Single): TPrimitiveTypeValue;
begin
  Result.PrimitiveClass := pcConstant;
  Result.PrimitiveType  := ptSingle;
  Result.vSingle        := aValue;
  Result.ValueStr       := FloatToStr(aValue);
end;

function  NewBooleanPrimitiveTypeConstant(const aValue: Boolean): TPrimitiveTypeValue;
const
  cBooleanToStr: array[Boolean] of String = (
    'False',
    'True'
  );
begin
  Result.PrimitiveClass := pcConstant;
  Result.PrimitiveType  := ptBoolean;
  Result.vBoolean       := aValue;
  Result.ValueStr       := cBooleanToStr[aValue];
end;

function  NewStringPrimitiveTypeConstant (const aValue: String): TPrimitiveTypeValue;
begin
  Result.PrimitiveClass := pcConstant;
  Result.PrimitiveType  := ptString;
  Result.vString        := aValue;
  Result.ValueStr       := aValue;
end;

end.
