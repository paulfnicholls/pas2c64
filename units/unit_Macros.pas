unit unit_Macros;

{$MODE Delphi}

interface

uses
  Classes;

type
  TMacro = class
  private
    FName: String;
    FArgs: array of String;
    FLines: TStringList;
  public
    constructor Create(const aName: String; const aArgs: array of String);
    destructor  Destroy; override;

    procedure AddLine(const aLine: String);
    procedure Output(const aStream: TStream; const aArgs: array of String);

    property Name: String read FName;
  end;

implementation

constructor TMacro.Create(const aName: String; const aArgs: array of String);
begin
  inherited Create;

  FLines := TStringList.Create;
end;

destructor  TMacro.Destroy;
begin
  FLines.Free;

  inherited Destroy;
end;

procedure TMacro.AddLine(const aLine: String);
begin
  FLines.Add(aLine);
end;

procedure TMacro.Output(const aStream: TStream; const aArgs: array of String);
begin

end;

end.
