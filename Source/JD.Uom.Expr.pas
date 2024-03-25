unit JD.Uom.Expr;

interface

//Perform string-based mathematical evaluations using Delphi Web Script (DWS):
{ $DEFINE USE_DWS}
//MOVED TO PROJECT CONFIG LEVEL

//TODO: Offer alternatives...
//- Jedi
//- ???


uses
  System.Classes, System.SysUtils
  {$IFDEF USE_DWS}
  , JD.Uom.Expr.DWS
  {$ENDIF}
  ;

type

  /// <summary>
  /// Encapsulates a string-based mathematical expression evaluator to be used
  ///  in conversions. Made as abstract as possible so the rest of the system
  ///  has no concept of how expressions are evaluated. Alternative solutions
  ///  can be implemented by means of compiler conditionals.
  /// </summary>
  TUOMEvaluator = class(TObject)
  private
    FConstants: TStringList;
    {$IFDEF USE_DWS}
    FModule: TdmDWS;
    {$ENDIF}
    function GetConstants(const Name: String): String;
    //procedure PopulateConstants;
  public
    constructor Create;
    destructor Destroy; override;

    function Evaluate(const Value: Extended; const Expr: String;
      const PrintLn: Boolean = True): Extended;
    function EvaluateStr(const Value: Extended; const Expr: String;
      const PrintLn: Boolean = True): String;
    function Calculate(const Expr: String): String;

    function GetConstantName(const Index: Integer): String;
    function GetConstantValue(const Index: Integer): String;
    function AddConstant(const Name, Value, DataType: String): Integer;
    procedure DeleteConstant(const Index: Integer); overload;
    procedure DeleteConstant(const Name: String); overload;
    procedure ClearConstants;
    function ConstantCount: Integer;
    property Constants[const Name: String]: String read GetConstants; default;
  end;

implementation

uses
  JD.Uom;

{ TUOMEvaluator }

constructor TUOMEvaluator.Create;
begin
  FConstants:= TStringList.Create;
  {$IFDEF USE_DWS}
  FModule:= TdmDWS.Create(nil);
  {$ENDIF}
end;

destructor TUOMEvaluator.Destroy;
begin
  {$IFDEF USE_DWS}
  FreeAndNil(FModule);
  {$ENDIF}
  FreeAndNil(FConstants);
  inherited;
end;

procedure TUOMEvaluator.DeleteConstant(const Name: String);
begin
  DeleteConstant(FConstants.IndexOfName(Name));
end;

procedure TUOMEvaluator.DeleteConstant(const Index: Integer);
begin
  FConstants.Delete(Index);
end;

(*
procedure TUOMEvaluator.PopulateConstants;
var
  X: Integer;
  CN, CV, CT: String;
  CVal: Extended;
  P: Integer;
  {$IFDEF USE_DWS}
  C: TdwsConstant;
  {$ENDIF}
begin
  {$IFDEF USE_DWS}
  FUnit.Constants.Clear;
  for X := 0 to FConstants.Count-1 do begin
    CN:= FConstants.Names[X];
    CV:= FConstants.ValueFromIndex[X];

    //Parse data type...
    P:= Pos('=', CV);
    if P > 0 then begin
      CT:= Copy(CV, 1, P-1);
      Delete(CV, 1, P);
    end else begin
      CT:= 'Float'; //Default
    end;

    //TODO: Check value for anything else to parse...
    CVal:= StrToFloatDef(CV, 0);

    C:= FUnit.Constants.Add;
    C.Name:= CN;
    C.Value:= CVal;
    C.DataType:= CT;
  end;
  {$ENDIF}
end;
*)

function TUOMEvaluator.Evaluate(const Value: Extended; const Expr: String;
  const PrintLn: Boolean = True): Extended;
begin
  {$IFDEF USE_DWS}
  Result:= FModule.Evaluate(Value, Expr, PrintLn);
  {$ELSE}
  Result:= -1;
  {$ENDIF}
end;

function TUOMEvaluator.EvaluateStr(const Value: Extended; const Expr: String;
  const PrintLn: Boolean): String;
begin
  Result:= '';
  {$IFDEF USE_DWS}
  Result:= FModule.EvaluateStr(Value, Expr, PrintLn);
  {$ENDIF}
end;

function TUOMEvaluator.AddConstant(const Name, Value, DataType: String): Integer;
var
  DT: String;
begin
  //Check for duplicate...
  if FConstants.IndexOfName(Name) >= 0 then
    raise Exception.Create('Cannot add duplicate constant "'+Name+'".');
  DT:= Trim(DataType);
  if DT <> '' then
    DT:= DT + '=';
  FConstants.Values[Name]:= DT + Value;
  Result:= FConstants.IndexOfName(Name);
end;

function TUOMEvaluator.Calculate(const Expr: String): String;
begin
  //TODO: Allow user / dev to enter complex formula consisting of UOMS
  //  and return the calculated result.
  //For example:
  //  6.9ft * 17.04m / (4yd * 2)
  //NOTE: May need to wrap inside functions, such as. ..
  //  UOM('6.9ft') * UOM('17.04m') / (UOM('4yd') * 2)
  Result:= EvaluateStr(1, Expr, False);
end;

procedure TUOMEvaluator.ClearConstants;
begin
  FConstants.Clear;
end;

function TUOMEvaluator.ConstantCount: Integer;
begin
  Result:= FConstants.Count;
end;

function TUOMEvaluator.GetConstantValue(const Index: Integer): String;
begin
  Result:= FConstants.ValueFromIndex[Index];
end;

function TUOMEvaluator.GetConstantName(const Index: Integer): String;
begin
  Result:= FConstants.Names[Index];
end;

function TUOMEvaluator.GetConstants(const Name: String): String;
begin
  Result:= FConstants.Values[Name];
end;

end.
