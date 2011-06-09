program pas2c64;

{$MODE Delphi}

//{$APPTYPE CONSOLE}
uses
  Forms, Interfaces,
  form_pas2c64 in 'form_pas2c64.pas' {FormMainForm},
  unit_pas2c64_CodeGenerator in 'units\unit_pas2c64_CodeGenerator.pas',
  unit_pas2c64_parser in 'units\unit_pas2c64_parser.pas',
  unit_pas2c64_constants in 'units\unit_pas2c64_constants.pas',
  unit_pas2c64_symboltable in 'units\unit_pas2c64_symboltable.pas',
  uParser in 'units\uParser.pas',
  unit_Macros in 'units\unit_Macros.pas';

{$R *.res}

var
  C64MemFLoat: TC64MemFloat;
  C64RegFLoat: TC64RegFloat;
begin
  Application.Initialize;
  Application.CreateForm(TFormMainForm, FormMainForm);
  Application.Run;

{  FloatToC64Float(0,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(1,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(2,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(3,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(1/2,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(1/4,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);
  FloatToC64Float(-67,C64RegFLoat);
  C64FloatToFloat(C64RegFLoat);

  FloatToC64Float(13,C64RegFLoat);

  FloatToC64Float(0,C64MemFLoat);
  C64FloatToFloat(C64MemFloat);
  FloatToC64Float(1/4,C64MemFLoat);
  C64FloatToFloat(C64MemFloat);
  FloatToC64Float(3.141592654,C64MemFLoat);
  C64FloatToFloat(C64MemFloat);
  FloatToC64Float(+27,C64MemFLoat);
  C64FloatToFloat(C64MemFloat);

  ReadLn;}
end.
