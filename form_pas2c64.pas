unit form_pas2c64;

{$MODE Delphi}

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, SynEdit, SynHighlighterPas,
  StdCtrls, ExtCtrls, Menus, unit_pas2c64_parser, Controls, ComCtrls;

type

  { TFormMainForm }

  TFormMainForm = class(TForm)
    GroupBox_SourceCode: TGroupBox;
    MainMenu: TMainMenu;
    MenuItem_NewSource: TMenuItem;
    MenuItem_CompileAndRun: TMenuItem;
    MenuItem_Project: TMenuItem;
    MenuItem_Compile: TMenuItem;
    MenuItem_Exit: TMenuItem;
    MenuItem_SaveSource: TMenuItem;
    MenuItem_OpenSource: TMenuItem;
    MenuItem_File: TMenuItem;
    PageControl1: TPageControl;
    PageControl2: TPageControl;
    Panel1: TPanel;
    Button_Compile: TButton;
    Button_CompileAndRun: TButton;
    CheckBox_UseBasicLoader: TCheckBox;
    Edit_CodeAddr: TEdit;
    Label_CodeAddr: TLabel;
    Splitter: TSplitter;
    SynEdit_AssemblyOutput: TSynEdit;
    SynEdit_Errors: TSynEdit;
    SynEdit_SourceCode: TSynEdit;
    SynPasSyn: TSynPasSyn;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet_AssemblyOutput: TTabSheet;
    TabSheet_Errors: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button_CompileClick(Sender: TObject);
    procedure Button_CompileAndRunClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CheckBox_UseBasicLoaderClick(Sender: TObject);
    procedure MenuItem_ExitClick(Sender: TObject);
    procedure MenuItem_NewSourceClick(Sender: TObject);
    procedure MenuItem_OpenSourceClick(Sender: TObject);
    procedure MenuItem_SaveSourceClick(Sender: TObject);
    procedure SynEdit_SourceCodeKeyUp(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FParser: TPas2C64_Parser;
    FFileName: String;
    procedure CompileCode;
    procedure UpdateCaption;
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
   reg.RootKey := HKEY_CLASSES_ROOT;
   reg.OpenKey('.' + ExtName, True);
   reg.WriteString('', ExtName + 'file');
   reg.CloseKey;
   reg.CreateKey(ExtName + 'file');
   reg.OpenKey(ExtName + 'file\DefaultIcon', True);
   reg.WriteString('', AppName + ',0');
   reg.CloseKey;
   reg.OpenKey(ExtName + 'file\shell\open\command', True);
   reg.WriteString('',AppName+' "%1"');
   reg.CloseKey;
  finally
   reg.Free;
  end;

  SHChangeNotify(SHCNE_ASSOCCHANGED, SHCNF_IDLIST, nil, nil);
end;

procedure TFormMainForm.Button_CompileAndRunClick(Sender: TObject);
begin
  CompileCode;

  if FParser.ErrorMsg = '' then
    ShellExecute(handle,'open','pas2c64_compileandruncode.bat',nil,nil,SW_SHOWNORMAL);
end;

procedure TFormMainForm.CheckBox_UseBasicLoaderClick(Sender: TObject);
begin
  Label_CodeAddr.Enabled := not CheckBox_UseBasicLoader.Checked;
  Edit_CodeAddr .Enabled := not CheckBox_UseBasicLoader.Checked;
end;

procedure TFormMainForm.MenuItem_ExitClick(Sender: TObject);
begin
  Close;
end;

procedure TFormMainForm.MenuItem_NewSourceClick(Sender: TObject);
begin
  SynEdit_SourceCode.BeginUpdate;
  SynEdit_SourceCode.Lines.Clear;
  SynEdit_SourceCode.EndUpdate;
  FFileName := 'Untitled.pas';
  UpdateCaption;
end;

procedure TFormMainForm.MenuItem_OpenSourceClick(Sender: TObject);
var
   od: TOpenDialog;
begin
  od := TOpenDialog.Create(Self);
  try
    od.Title      := 'Open pas2c64 Source Code';
    od.DefaultExt := '.pas';
    od.Filter     := 'pas2c64 file (*.pas)|*.pas';
    od.Options    := od.Options + [ofPathMustExist,ofFileMustExist];

    if od.Execute then
    begin
      SynEdit_SourceCode.BeginUpdate;
      SynEdit_SourceCode.Lines.LoadFromFile(od.FileName);
      SynEdit_SourceCode.EndUpdate;

      SynEdit_AssemblyOutput.ClearAll;

      FFileName := od.FileName;
      UpdateCaption;
    end;
  finally
    od.Free;
  end;
end;

procedure TFormMainForm.MenuItem_SaveSourceClick(Sender: TObject);
var
   sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(Self);
  try
    sd.Title      := 'Save pas2c64 Source Code';
    sd.DefaultExt := '.pas';
    sd.Filter     := 'pas2c64 file (*.pas)|*.pas';
    sd.FileName   := ChangeFileExt(ExtractFileName(FFileName),'.pas');
    sd.Options    := sd.Options + [ofPathMustExist,ofOverwritePrompt];

    if sd.Execute then
    begin
      FFileName := ChangeFileExt(sd.FileName,'.pas');

      SynEdit_SourceCode.Lines.SaveToFile(sd.FileName);
      UpdateCaption;
    end;
  finally
    sd.Free;
  end;
end;

procedure TFormMainForm.SynEdit_SourceCodeKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
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

  SynEdit_SourceCode.BeginUpdate;
  SynEdit_SourceCode.Lines.Clear;
  SynEdit_SourceCode.EndUpdate;
  FFileName := 'Untitled.pas';
  UpdateCaption;
end;

procedure TFormMainForm.FormDestroy(Sender: TObject);
begin
  FParser.Free;
end;

procedure TFormMainForm.UpdateCaption;
begin
  Caption := 'Pas2C64 Compiler - ' + ExtractFileName(FFileName);
end;

procedure TFormMainForm.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
end;

procedure TFormMainForm.Button_CompileClick(Sender: TObject);
begin
  CompileCode;

  if FParser.ErrorMsg = '' then
    ShellExecute(handle,'open','pas2c64_compilecode.bat',nil,nil,SW_SHOWNORMAL);
end;

procedure TFormMainForm.Button1Click(Sender: TObject);
begin
   RegisterFileType('t64','C:\Shared\WinVICE-2.2-x86\x64.exe');
   RegisterFileType('prg','C:\Shared\WinVICE-2.2-x86\x64.exe');
end;

end.
