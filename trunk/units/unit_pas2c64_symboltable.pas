unit unit_pas2c64_symboltable;

{$MODE Delphi}

interface

uses
  Classes;

const
  cClass_Constant  = 0;
  cClass_Variable  = 1;
  cClass_Procedure = 2;

  cSubClass_None   = -1;
  cSubClass_Int8   = 0;
  cSubClass_Int16  = 1;
  cSubClass_Int32  = 2;
  cSubClass_UInt8  = 3;
  cSubClass_UInt16 = 4;
  cSubClass_UInt32 = 5;
  cSubClass_Float  = 6;

type
  TSymbol = class
    SymbolClass: Integer;
    SymbolSubClass: Integer;
    SymbolName: AnsiString;
    SymbolValue: AnsiString;
  end;

  TPas2C64_SymbolTable = class
  private
    FSymbolTable: TStringList;
  public
    constructor Create;
    destructor  Destroy; override;

    procedure Clear;

    function  AddSymbol(const aName,aValue: AnsiString; const aClass,aSubClass: Integer): TSymbol;
    function  SymbolExists(const aName: AnsiString): Boolean;
    function  GetSymbol(const aName: AnsiString): TSymbol;
  end;

implementation

uses
  SysUtils;

constructor TPas2C64_SymbolTable.Create;
begin
  inherited Create;

  FSymbolTable := TStringList.Create;
  FSymbolTable.Sorted := True;
  FSymbolTable.Duplicates := dupIgnore;
end;

destructor  TPas2C64_SymbolTable.Destroy;
begin
  Clear;

  FSymbolTable.Free;

  inherited Destroy;
end;

procedure TPas2C64_SymbolTable.Clear;
var
  i: Integer;
begin
  for i := 0 to FSymbolTable.Count - 1 do
    TSymbol(FSymbolTable.Objects[i]).Free;

  FSymbolTable.Clear;
end;

function  TPas2C64_SymbolTable.AddSymbol(const aName,aValue: AnsiString; const aClass,aSubClass: Integer): TSymbol;
var
  Index: Integer;
begin
  Result := nil;

  if FSymbolTable.Find(LowerCase(aName),Index) then
    raise Exception.Create('Symbol "'+aName+'" already exists!');

  Result := TSymbol.Create;
  Result.SymbolName     := LowerCase(aName);
  Result.SymbolValue    := aValue;
  Result.SymbolClass    := aClass;
  Result.SymbolSubClass := aSubClass;

  FSymbolTable.AddObject(LowerCase(aName),Result);
end;

function  TPas2C64_SymbolTable.SymbolExists(const aName: AnsiString): Boolean;
var
  Index: Integer;
begin
  Result := FSymbolTable.Find(LowerCase(aName),Index);
end;

function  TPas2C64_SymbolTable.GetSymbol(const aName: AnsiString): TSymbol;
var
  Index: Integer;
begin
  Result := nil;

  if not FSymbolTable.Find(LowerCase(aName),Index) then Exit;

  Result := TSymbol(FSymbolTable.Objects[Index]);
end;

end.
