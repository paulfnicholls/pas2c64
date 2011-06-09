program pas2c64;

//{$APPTYPE CONSOLE}
uses
  Forms,
  form_pas2c64 in 'form_pas2c64.pas' {FormMainForm},
  unit_pas2c64_CodeGenerator in 'units\unit_pas2c64_CodeGenerator.pas',
  unit_pas2c64_parser in 'units\unit_pas2c64_parser.pas',
  unit_pas2c64_constants in 'units\unit_pas2c64_constants.pas',
  unit_pas2c64_symboltable in 'units\unit_pas2c64_symboltable.pas',
  uParser in 'units\uParser.pas',
  unit_Macros in 'units\unit_Macros.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMainForm, FormMainForm);
  Application.Run;
end.
