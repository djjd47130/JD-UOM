unit JD.Uom.Expr;

interface

//Perform string-based mathematical evaluations using Delphi Web Script (DWS):
{$DEFINE USE_DWS}

//TODO: Offer alternatives...
//- Jedi
//- ???


uses
  System.Classes, System.SysUtils
  {$IFDEF USE_DWS}
  , dwsCompiler, dwsExprs, dwsComp, dwsErrors
  {$ENDIF}
  ;

type
  TUOMEvalInst = class(TObject)
  private
    {$IFDEF USE_DWS}
    FDWS: TDelphiWebScript;
    {$ENDIF}
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
  {$IFDEF USE_DWS}
  FDWS:= TDelphiWebScript.Create(nil);
  {$ENDIF}

end;

destructor TUOMEvalInst.Destroy;
begin

  {$IFDEF USE_DWS}
  FreeAndNil(FDWS);
  {$ENDIF}
  inherited;
end;

function TUOMEvalInst.Evaluate(const Value: Double; const Expr: String): Double;
var
  E: String;
  {$IFDEF USE_DWS}
  Prog: IdwsProgram;
  Exec: IdwsProgramExecution;
  Res: String;
  {$ENDIF}
begin
  {$IFDEF USE_DWS}
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
  {$ENDIF}
end;

end.
