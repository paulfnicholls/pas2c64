unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,
  uParserPL0;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Memo2: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Parser: TParserPL0;
  SrcStream: TMemoryStream;
  DstStream: TMemoryStream;
begin
  Parser := TParserPL0.Create;
  SrcStream := TMemoryStream.Create;
  DstStream := TMemoryStream.Create;

  try
    Memo2.Clear;
    Memo1.Lines.SaveToStream(SrcStream);
    SrcStream.Seek(0,soFromBeginning);

    if not Parser.Execute(SrcStream,DstStream) then
      Memo2.Lines.Text := Parser.ErrorMsg
    else
      Memo2.Lines.Text := 'Parse complete :)';
  finally
    Parser.Free;
    SrcStream.Free;
    DstStream.Free;
  end;
end;

end.
