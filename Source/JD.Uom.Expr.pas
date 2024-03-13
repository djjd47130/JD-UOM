unit JD.Uom.Expr;

interface

uses
  System.Classes, System.SysUtils
  , dwsCompiler, dwsExprs, dwsComp, dwsErrors
  ;

type
  TUOMEvalInst = class(TObject)
  private
    FDWS: TDelphiWebScript;
  public
    constructor Create;
    destructor Destroy; override;
    function Evaluate(const Value: Double; const Expr: String): Double;
  end;

implementation

uses
  JD.Uom;

{ TUOMEvalInst }

constructor TUOMEvalInst.Create;
begin
  FDWS:= TDelphiWebScript.Create(nil);

end;

destructor TUOMEvalInst.Destroy;
begin

  FreeAndNil(FDWS);
  inherited;
end;

function TUOMEvalInst.Evaluate(const Value: Double; const Expr: String): Double;
var
  E: String;
  Prog: IdwsProgram;
  Exec: IdwsProgramExecution;
  Res: String;
begin
  //Evaluate expression using DWScript...
  E:= StringReplace(Expr, 'Value', FormatFloat(NumInternalFormat, Value), []);
  Prog:= FDWS.Compile('PrintLn('+E+');');
  if Prog.Msgs.Count > 0 then begin
    raise EUOMEvalException.Create(Prog.Msgs.AsInfo);
  end else begin
    Exec:= prog.Execute;
    if Exec.Msgs.HasErrors then begin
      raise EUOMEvalException.Create(Exec.Msgs.AsInfo);
    end else begin
      Res:= Exec.Result.ToString;
      Res:= StringReplace(Res,#$D,'',[rfReplaceAll]);
      Res:= StringReplace(Res,#$A,'',[rfReplaceAll]);
      Result:= StrToFloatDef(Res, -1);
    end;
  end;
end;

end.
