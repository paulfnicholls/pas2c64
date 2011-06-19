unit unit_pas2c64_symboltable;

{$MODE Delphi}

interface

uses
  Classes;

const
  cSymClass_None      = -1;
  cSymClass_Constant  = 0;
  cSymClass_Variable  = 1;
  cSymClass_Procedure = 2;
  cSymClass_Interrupt = 3;
  cSymClass_AsmProc   = 4;

  cSymSubClass_None   = -1;
  cSymSubClass_SInt8  = 0;
  cSymSubClass_SInt16 = 1;
  cSymSubClass_SInt32 = 2;
  cSymSubClass_UInt8  = 3;
  cSymSubClass_UInt16 = 4;
  cSymSubClass_UInt32 = 5;
  cSymSubClass_Float  = 6;
  cSymSubClass_String = 7;

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
