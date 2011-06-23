unit unit_pas2c64_types;

{$mode delphi}

interface

//
// simple primitive types supported in pas2c64:
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

  TSimpleType = (
    stInteger,
    stWord,
    stSingle,
    stBoolean,
    stString,
  );

  PSimpleTypeValue = ^TSimpleTypeValue;
  TSimpleTypeValue = packed record
    ValueStr: String; // string representation of this value

    case SimpleType : TSimpleType of
      stInteger : (vInteger : TInt16);
      stWord    : (vWord    : Word);
      stSingle  : (vSingle  : Single);
      stBoolean : (vBoolean : Boolean);
      stString  : (vString  : TString255);
  end;

function  SimpleTypeToStr(const aSimpleType: TSimpleType): String;
function  StrToSimpleType(const aStr: String; out aSimpleType: TSimpleType): Boolean;

implementation

const
  cSimpleTypeStr: array[TSimpleType] of String = (
    'integer',
    'word',
    'single',
    'boolean',
    'string'
  );

function  SimpleTypeToStr(const aSimpleType: TSimpleType): String;
// converts a simple type to a string
begin
  Result := cSimpleTypeStr[aSimpleType];
end;

function  StrToSimpleType(const aStr: String; out aSimpleType: TSimpleType): Boolean;
// converts a string to a simple type, and returns false if it failed
var
  st: TSimpleType;
begin
  Result := False;

  for st := Low(TSimpleType) to High(TSimpleType) do
    if LowerCase(cSimpleTypeStr[st]) = LowerCase(aStr) then
    begin
      Result      := True;
      aSimpleType := st;
      Exit;
    end;
end;

end.

