Unit unit_Evaluator;
Interface

Uses
  SysUtils;

{$MODE DELPHI}

Type
{..............................................................................}
  TExpressionEvaluationException = Class(Exception);

  TExpressionEvaluator = Class
  private
    FLook: Char;
    FBuffer: String;
    FErrorMsg: String;

    Function  IsAlpha(Const c: Char): Boolean;
    Function  IsAddOp(Const c: Char): Boolean;
    Function  IsMulOp(Const c: Char): Boolean;
    Function  IsDigit(Const c: Char): Boolean;
    Function  IsWhiteSpace(Const c: Char): Boolean;
    Procedure Error(Const aErrorMsg: String);
    Function  Factor: Single;
    Function  Term: Single;
    Procedure Expected(s: String);
    Procedure Match(s: String);
    Procedure GetChar;
    Procedure SkipWhiteSpaces;
    Function  GetNumber: Single;
    Function  Expression: Single;
    Function  Equation: Single;
  public
    Function Evaluate(aExpression: String; Var aExpressionResult: Single): Boolean;
    Property ErrorMsg: String Read FErrorMsg;
  End;
{..............................................................................}
Function IsNumber(Const aValue: String): Boolean;
Function IsFormulaText(aText: String): Boolean;
{..............................................................................}

{..............................................................................}
Implementation

Const
{..............................................................................}
  cCR    = #13;
  cLF    = #10;
  cSpace = ' ';
  cTab   = ^I;

type
  TCharSet = set of char;

{$IFDEF FPC}
function  CharInset(const c: Char; const aSet: TCharSet): Boolean;
begin
  Result := c in aSet;
end;
{$ENDIF}
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.IsAlpha(Const c: Char): Boolean;
Begin
  Result := CharInSet(c,['a'..'z','A'..'Z']);
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.IsDigit(Const c: Char): Boolean;
Begin
  Result := CharInSet(c,['0'..'9']);
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.IsAddOp(Const c: Char): Boolean;
Begin
  Result := CharInSet(c,['-','+']);
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.IsMulOp(Const c: Char): Boolean;
Begin
  Result := CharInSet(c,['/','*']);
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.IsWhiteSpace(Const c: Char): Boolean;
Begin
  Result := CharInSet(c,[cCR,cLF,cSpace,cTAB]);
End;
{..............................................................................}

{..............................................................................}
Procedure TExpressionEvaluator.Error(Const aErrorMsg: String);
Begin
  FErrorMsg := aErrorMsg;
  Raise TExpressionEvaluationException.Create(aErrorMsg);
End;
{..............................................................................}

{..............................................................................}
Procedure TExpressionEvaluator.Expected(s: String);
Begin
   Error(s + ' Expected');
End;
{..............................................................................}

{..............................................................................}
Procedure TExpressionEvaluator.Match(s: String);
Var
  i: Integer;
Begin
  For i := 1 To Length(s) Do Begin
    If FLook = s[i] Then
      GetChar
    Else
      Expected('''' + s + '''');
  End;
  SkipWhiteSpaces;
End;
{..............................................................................}

{..............................................................................}
Procedure TExpressionEvaluator.GetChar;
Begin
  If FBuffer = '' Then
    FLook := #0
  Else Begin
    FLook := FBuffer[1];
    Delete(FBuffer,1,1);
  End;
End;
{..............................................................................}

{..............................................................................}
Procedure TExpressionEvaluator.SkipWhiteSpaces;
Begin
  While IsWhiteSpace(FLook) Do
    GetChar;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.GetNumber: Single;
Var
  Number: String;
  Code: Integer;
Begin
  // number can start with a digit, or a '.' (implied '0' at beginning)
  If (FLook <> '.') And Not IsDigit(FLook) Then Error('Number Expected');
  Number := '';
  While IsDigit(FLook) Do Begin
    Number := Number + FLook;
    GetChar;
  End;
  If FLook = '.' Then Begin
    Number := Number + FLook;
    GetChar;
    If Not IsDigit(FLook) Then Error('Digits Expected after "."');
    While IsDigit(FLook) Do Begin
      Number := Number + FLook;
      GetChar;
    End;
  End;
  Val('0'+Number,Result,Code);
  If Code <> 0 Then Error('Illegal Number "'+Number+'"');

  SkipWhiteSpaces;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.Factor: Single;
Begin
  If FLook = '(' Then Begin
    Match('(');
    Result := Expression;
    Match(')');
  End
  Else
    Result := GetNumber;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.Term: Single;
Begin
  Result := Factor;
  While IsMulOp(FLook) Do Begin
    Case FLook of
      '*': Begin
        Match('*');
        Result := Result * Term;
      End;
      '/': Begin
        Match('/');
        Result := Result / Term;
      End;
    End;
  End;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.Expression: Single;
begin
  If IsAddOp(FLook) Then
    Result := 0
  Else
    Result := Term;

  While IsAddOp(FLook) Do Begin
    Case FLook of
      '+': Begin
        Match('+');
        Result := Result + Term;
      End;
      '-': Begin
        Match('-');
        Result := Result - Term;
      End;
    End;
  End;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.Equation: Single;
Begin
  If FLook = '=' Then Match('=');
  Result := Expression;
End;
{..............................................................................}

{..............................................................................}
Function  TExpressionEvaluator.Evaluate(aExpression: String; Var aExpressionResult: Single): Boolean;
Begin
  Result := True;

  FBuffer := aExpression;
  FErrorMsg := '';
  GetChar;
  Try
    aExpressionResult := Equation;
  Except
    Result := False;
  End;
End;
{..............................................................................}

{..............................................................................}
Function  IsNumber(Const aValue: String): Boolean;
Var
  ValueF: Single;
  Code: Integer;
Begin
  Val(aValue,ValueF,Code);
  Result := Code = 0;
End;
{..............................................................................}

{..............................................................................}
Function IsFormulaText(aText: String): Boolean;
Begin
  Result := Pos('=',aText) = 1;
End;
{..............................................................................}

{..............................................................................}
End.

