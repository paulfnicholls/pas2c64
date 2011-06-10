unit form_pas2c64;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, SynEdit, SynEditHighlighter, SynHighlighterPas, StdCtrls, ExtCtrls,
  unit_pas2c64_CodeGenerator,
  unit_pas2c64_parser;

type

  { TFormMainForm }

  TFormMainForm = class(TForm)
    SynPasSyn: TSynPasSyn;
    GroupBox1: TGroupBox;
    SynEdit_SourceCode: TSynEdit;
    Panel1: TPanel;
    Button_Compile: TButton;
    Button_CompileAndRun: TButton;
    Splitter: TSplitter;
    GroupBox2: TGroupBox;
    SynEdit_AssemblyOutput: TSynEdit;
    CheckBox_UseBasicLoader: TCheckBox;
    Edit_CodeAddr: TEdit;
    Label_CodeAddr: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure Button_CompileClick(Sender: TObject);
    procedure Button_CompileAndRunClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox_UseBasicLoaderClick(Sender: TObject);
  private
    { Private declarations }
    FParser: TPas2C64_Parser;
    procedure CompileCode;
  public
    { Public declarations }
  end;

var
  FormMainForm: TFormMainForm;

implementation

{$R *.lfm}

uses
  ShellAPI,registry, shlobj;

procedure RegisterFileType(ExtName:String; AppName:String) ;
var
   reg:TRegistry;
begin
  reg := TRegistry.Create;
  try
   reg.RootKey:=HKEY_CLASSES_ROOT;
   reg.OpenKey('.' + ExtName, True) ;
   reg.WriteString('', ExtName + 'file') ;
   reg.CloseKey;
   reg.CreateKey(ExtName + 'file') ;
   reg.OpenKey(ExtName + 'file\DefaultIcon', True) ;
   reg.WriteString('', AppName + ',0') ;
   reg.CloseKey;
   reg.OpenKey(ExtName + 'file\shell\open\command', True) ;
   reg.WriteString('',AppName+' "%1"') ;
   reg.CloseKey;
  finally
   reg.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil) ;
end;

procedure TFormMainForm.Button_CompileAndRunClick(Sender: TObject);
begin
  CompileCode;

  ShellExecute(handle,'open','pas2c64_runcode.bat',nil,nil,SW_SHOWNORMAL);
end;

procedure TFormMainForm.CheckBox_UseBasicLoaderClick(Sender: TObject);
begin
  Label_CodeAddr.Enabled := not CheckBox_UseBasicLoader.Checked;
  Edit_CodeAddr .Enabled := not CheckBox_UseBasicLoader.Checked;
end;

procedure TFormMainForm.CompileCode;
var
  SrcStream: TMemoryStream;
  DstStream: TMemoryStream;
  sl: TStringList;
begin
  SrcStream := TMemoryStream.Create;
  DstStream := TMemoryStream.Create;
  sl := TStringList.Create;
  try
    SynEdit_SourceCode.Lines.SaveToStream(SrcStream);
    SrcStream.Seek(0,soFromBeginning);

    FParser.CodeAddr := $0000;
    if not CheckBox_UseBasicLoader.Checked then
      FParser.CodeAddr := StrToInt(Edit_CodeAddr.Text);

    try
      if not FParser.Execute(SrcStream,DstStream) then
        ShowMessage(FParser.ErrorMsg)
      else
      begin
        DstStream.Seek(0,soFromBeginning);
        DstStream.SaveToFile('pas2c64_output.asm');

        DstStream.Seek(0,soFromBeginning);
        SynEdit_AssemblyOutput.BeginUpdate;
        SynEdit_AssemblyOutput.Lines.LoadFromStream(DstStream);
        SynEdit_AssemblyOutput.EndUpdate;

        ShellExecute(handle,'open','pas2c64_compilecode.bat',nil,nil,SW_SHOWNORMAL);
      end;
    except
      on E:Exception do
        ShowMessage(E.Message);
    end;
  finally
    SrcStream.Free;
    DstStream.Free;
    sl.Free;
  end;
end;

procedure TFormMainForm.FormCreate(Sender: TObject);
begin
  FParser := TPas2C64_Parser.Create;
end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
begin
  FParser.Free;
end;

procedure TFormMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_F9) then
  begin
    CompileCode;

    if not (ssCTRL in Shift) then
      ShellExecute(handle,'open','test.prg',nil,nil,SW_SHOWNORMAL);
  end;
end;

procedure TFormMainForm.Button_CompileClick(Sender: TObject);
begin
  CompileCode
end;

procedure TFormMainForm.Button1Click(Sender: TObject);
begin
   RegisterFileType('t64','C:\Shared\WinVICE-2.2-x86\x64.exe');
   RegisterFileType('prg','C:\Shared\WinVICE-2.2-x86\x64.exe');
end;

end.
