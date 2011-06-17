unit unit_Expressions;

{$mode delphi}

interface

uses
  Classes, SysUtils; 

type
  TExpressionOperator = (
    eoNeg,
    eoNot,
    eoMul,
    eoDiv,
    eoSub,
    eoAdd,
    eoIntDiv,
    eoIntMod,
    eoAnd,
    eoOr,
    eoXor,
    eoShl,
    eoShr
  );

const
  cExpressionOps: Array[TExpressionOperator] of String = (
    'neg',
    'not',
    '*',
    '/',
    '-',
    '+',
    'div',
    'mod',
    'and',
    'or',
    'xor',
    'shl',
    'shr'
  );

type
  TExpressionNodeList    = class;
  TExpressionOperandNode = class;

  // returns false if operation wasn't simplified.
  // Otherwise, aResultValue, aResultType contains the simplified result.
  TOnAddExpressionOperation = function(const a,b: TExpressionOperandNode;
                                       const Op: TExpressionOperator;
                                       out   aResultValue: String;
                                       out   aResultType: Integer): Boolean of object;

  { TExpressionNode }
  TExpressionNode = class
  protected
    FParent: TExpressionNodeList;
  public
    constructor Create(const aParent: TExpressionNodeList);

    function  GetRelativeNode(const aOffset: Integer): TExpressionNode;
    function  GetNodeIndex: Integer;

    property Parent: TExpressionNodeList read FParent;
  end;

  { TExpressionOperandNode }
  TExpressionOperandNode = class(TExpressionNode)
    OperandValue: String;
    OperandType: Integer;

    constructor Create(const aParent: TExpressionNodeList; const aValue: String; const aType: Integer); reintroduce;
  end;

  { TExpressionOperatorNode }
  TExpressionOperatorNode = class(TExpressionNode)
    Op: TExpressionOperator;
    constructor Create(const aParent: TExpressionNodeList; const aOp: TExpressionOperator); reintroduce;
  end;

  { TExpressionNodeList }
  TExpressionNodeList = class
  private
    FNodeList: TList;
  public
    constructor Create;
    destructor  Destroy; override;

    function  AddOperand(const aValue: String; const aType: Integer): TExpressionNodeList;
    function  InsertOperand(const aValue: String; const aType: Integer; const aIndex: Integer): TExpressionNodeList;
    function  AddOperator(const aOp: TExpressionOperator;
                          const aOnAddExpressionOperation: TOnAddExpressionOperation): TExpressionNodeList;
    procedure Clear;
    function  GetNodeCount: Integer;
    function  GetNode(const aIndex: Integer): TExpressionNode;
    function  GetRelativeNode(const aNode: TExpressionNode; const aOffset: Integer): TExpressionNode;
    function  GetNodeIndex(const aNode: TExpressionNode): Integer;
    procedure DeleteNode(const aNode: TExpressionNode);
    function  Simplify(out aErrorMsg: String): TExpressionNodeList;

    procedure WriteExpression;
  end;

implementation

constructor TExpressionNode.Create(const aParent: TExpressionNodeList);
begin
  FParent := aParent;
end;

function  TExpressionNode.GetRelativeNode(const aOffset: Integer): TExpressionNode;
begin
  Result := FParent.GetRelativeNode(Self,aOffset);
end;

function  TExpressionNode.GetNodeIndex: Integer;
begin
  Result := FParent.GetNodeIndex(Self);
end;

constructor TExpressionOperandNode.Create(const aParent: TExpressionNodeList; const aValue: String; const aType: Integer);
begin
  inherited Create(aParent);

  OperandValue := aValue;
  OperandType  := aType;
end;

constructor TExpressionOperatorNode.Create(const aParent: TExpressionNodeList; const aOp: TExpressionOperator);
begin
  inherited Create(aParent);

  Op := aOp;
end;

constructor TExpressionNodeList.Create;
begin
  FNodeList := TList.Create;
end;

destructor  TExpressionNodeList.Destroy;
begin
  Clear;

  FNodeList.Free;
end;

function  TExpressionNodeList.AddOperand(const aValue: String; const aType: Integer): TExpressionNodeList;
var
  Node: TExpressionNode;
begin
  Node := TExpressionOperandNode.Create(Self,aValue,aType);
  FNodeList.Add(Node);

  Result := Self;
end;

function  TExpressionNodeList.InsertOperand(const aValue: String; const aType: Integer; const aIndex: Integer): TExpressionNodeList;
var
  Node: TExpressionNode;
begin
  Node := TExpressionOperandNode.Create(Self,aValue,aType);
  FNodeList.Insert(aIndex,Node);

  Result := Self;
end;

function  TExpressionNodeList.AddOperator(const aOp: TExpressionOperator;
                                          const aOnAddExpressionOperation: TOnAddExpressionOperation): TExpressionNodeList;
var
  Node: TExpressionNode;
  a,b: TExpressionOperandNode;
  // result info for simplified operation
  ResValue: String;
  ResType: Integer;
begin
  // get operands for this operation
  a := TExpressionOperandNode(GetNode(FNodeList.Count - 2));
  b := TExpressionOperandNode(GetNode(FNodeList.Count - 1));

  if aOnAddExpressionOperation(a,b,aOp,ResValue,ResType) then
  // was simplified so add resulting operand instead of operator
    Result := AddOperand(ResValue,ResType)
  else
  begin
  // wasn't simplified so add operator
    Node := TExpressionOperatorNode.Create(Self,aOp);
    FNodeList.Add(Node);

    Result := Self;
  end;
end;

procedure TExpressionNodeList.Clear;
var
  i: Integer;
begin
  for i := FNodeList.Count - 1 downto 0 do
    TExpressionNode(FNodeList.Items[i]).Free;

  FNodeList.Clear;
end;

function  TExpressionNodeList.GetNodeCount: Integer;
begin
  Result := FNodeList.Count;
end;

function  TExpressionNodeList.GetNode(const aIndex: Integer): TExpressionNode;
begin
  Result := nil;

  if (aIndex < 0) or (aIndex >= FNodeList.Count) then Exit;
  Result := TExpressionNode(FNodeList.Items[aIndex]);
end;

function  TExpressionNodeList.GetRelativeNode(const aNode: TExpressionNode; const aOffset: Integer): TExpressionNode;
var
  Index: Integer;
begin
  Result := nil;

  Index := FNodeList.IndexOf(aNode);
  // node not found!
  if Index = -1 then Exit;

  Inc(Index,aOffset);

  // relative node out of range!
  if Index < 0                then Exit;
  if Index >= FNodeList.Count then Exit;

  Result := TExpressionNode(FNodeList.Items[Index]);
end;

procedure TExpressionNodeList.DeleteNode(const aNode: TExpressionNode);
var
  Index: Integer;
begin
  Index := FNodeList.IndexOf(aNode);

  if Index = -1 then Exit;
  FNodeList.Delete(Index);
  aNode.Free;
end;

function  TExpressionNodeList.GetNodeIndex(const aNode: TExpressionNode): Integer;
begin
  Result := FNodeList.IndexOf(aNode)
end;

function  TExpressionNodeList.Simplify(out aErrorMsg: String): TExpressionNodeList;
var
  i: Integer;
  Node: TExpressionNode;
  a,b: TExpressionOperandNode;
  op: TExpressionOperatorNode;
begin
  Result   := Self;
  aErrorMsg := '';

  i := 0;
  while (i >= 0) and (i < FNodeList.Count) do
  begin
    Node := TExpressionNode(FNodeList.Items[i]);

    if Node is TExpressionOperatorNode then
    begin
      // r := a op b
      op := TExpressionOperatorNode(Node);
      a  := TExpressionOperandNode(op.GetRelativeNode(-2));
      b  := TExpressionOperandNode(op.GetRelativeNode(-1));
    end;
    Inc(i);
  end;
end;

procedure TExpressionNodeList.WriteExpression;
var
  i: Integer;
  Node: TExpressionNode;
begin
  WriteLn('Expression');
  WriteLn('----------');
  for i := 0 to FNodeList.Count - 1 do
  begin
    Node := TExpressionNode(FNodeList.Items[i]);

    if Node is TExpressionOperatorNode then
      WriteLn(cExpressionOps[TExpressionOperatorNode(Node).Op])
    else
      WriteLn(TExpressionOperandNode(Node).OperandValue);
  end;
end;

end.

