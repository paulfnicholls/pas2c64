program toy_parser;
uses
  uParser in 'units\uParser.pas',
  uParserPL0 in 'units\uParserPL0.pas',
  Unit1 in 'forms\Unit1.pas' {Form1};

var
  Form: TForm1;
begin
  Form := TForm1.Create(nil);
  try
    Form.ShowModal;
  finally
    Form.Free;
  end;
end.