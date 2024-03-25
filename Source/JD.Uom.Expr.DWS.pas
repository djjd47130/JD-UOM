unit JD.Uom.Expr.DWS;

//NOTE: This unit is conditionally used in JD.Uom.Expr.pas.
//Be sure to exclude this from package upon compilation!

interface

uses
  System.SysUtils, System.Classes,
  dwsCompiler, dwsExprs, dwsComp, dwsErrors, dwsSymbols;

type
  TdmDWS = class(TDataModule)
    DWS: TDelphiWebScript;
    JDUOM: TdwsUnit;
    procedure JDUOMFunctionsUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsRegisterSimpleUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsRegisterUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsRegisterBaseUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsFindUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsPowerEval(info: TProgramInfo);
    procedure JDUOMFunctionsConvertEval(info: TProgramInfo);
    procedure JDUOMFunctionsUOMStringEval(info: TProgramInfo);
    procedure JDUOMFunctionsSqrEval(info: TProgramInfo);
    procedure JDUOMFunctionsCubeEval(info: TProgramInfo);
    procedure JDUOMFunctionsBaseUOMEval(info: TProgramInfo);
    procedure JDUOMFunctionsUOMExistsEval(info: TProgramInfo);
    procedure JDUOMFunctionsUOMCountEval(info: TProgramInfo);
    procedure JDUOMFunctionsUOMByIndexEval(info: TProgramInfo);
  private
    { Private declarations }
  public
    function EvaluateStr(const Value: Extended; const Expr: String;
      const PrintLn: Boolean = True): String;
    function Evaluate(const Value: Extended; const Expr: String;
      const PrintLn: Boolean = True): Extended;
  end;

var
  dmDWS: TdmDWS;

implementation

{%CLASSGROUP 'Vcl.Controls.TControl'}

{$R *.dfm}

uses
  JD.Uom,
  System.Math;

procedure PopulateUOM(U: TUOM; Info: TProgramInfo);
  procedure SetResultStr(const Name, Value: String);
  begin
    info.ResultVars.Member[Name].ValueAsString:= Value;
  end;
  procedure SetResultInt(const Name: String; const Value: Integer);
  begin
    info.ResultVars.Member[Name].ValueAsInteger:= Value;
  end;
  procedure SetResultFloat(const Name: String; const Value: Extended);
  begin
    info.ResultVars.Member[Name].Value:= Value;
  end;
begin
  SetResultInt('UOMType', Integer(U.UOMType));
  if U.Owner <> nil then
    SetResultStr('ParentUOM', U.Owner.NameSingular);
  SetResultStr('Category', U.Category);
  SetResultStr('NameSingular', U.NameSingular);
  SetResultStr('NamePlural', U.NamePlural);
  SetResultStr('Suffix', U.Suffix);
  SetResultStr('Systems', U.Systems.DelimitedText);
  SetResultFloat('Factor', U.Factor);
  SetResultStr('FromBase', U.ConvertFromBaseFormula);
  SetResultStr('ToBase', U.ConvertToBaseFormula);
  SetResultStr('Aliases', U.AllAliases);
end;

{ TdmDWS }

function TdmDWS.Evaluate(const Value: Extended; const Expr: String;
  const PrintLn: Boolean): Extended;
begin
  Result:= StrToFloatDef(EvaluateStr(Value, Expr, PrintLn), -1);
end;

function TdmDWS.EvaluateStr(const Value: Extended; const Expr: String;
  const PrintLn: Boolean): String;
var
  E: String;
  Prog: IdwsProgram;
  Exec: IdwsProgramExecution;
  Res: String;
begin
  //Evaluate expression using DWScript...

  E:= StringReplace(Expr, 'Value', FormatFloat(NumInternalFormat, Value), []);

  //PopulateConstants;

  if PrintLn then
    Prog:= DWS.Compile('PrintLn('+E+');', '')
  else
    Prog:= DWS.Compile(E, '');
  if Prog.Msgs.Count > 0 then begin
    raise EUOMEvalException.Create(Prog.Msgs.AsInfo);
  end else begin
    Exec:= prog.Execute;
    if Exec.Msgs.HasErrors then begin
      raise EUOMEvalException.Create(Exec.Msgs.AsInfo);
    end else begin
      Res:= Exec.Result.ToString;
      if PrintLn then begin
        Res:= StringReplace(Res,#$D,'',[rfReplaceAll]);
        Res:= StringReplace(Res,#$A,'',[rfReplaceAll]);
      end;
      Result:= Res;
    end;
  end;
end;

procedure TdmDWS.JDUOMFunctionsBaseUOMEval(info: TProgramInfo);
var
  U: TUOM;
begin
  U:= TUOMUtils.GetBaseUOM(Info.ParamAsString[0]);
  if U = nil then
    raise Exception.Create('Failed to find UOM');
  PopulateUOM(U, Info);
end;

procedure TdmDWS.JDUOMFunctionsConvertEval(info: TProgramInfo);
var
  F, T: TUOM;
begin
  F:= TUOMUtils.FindUOM(Info.ParamAsString[1]);
  if F = nil then
    raise Exception.Create('Failed to find From UOM.');
  T:= TUOMUtils.FindUOM(Info.ParamAsString[2]);
  if T = nil then
    raise Exception.Create('Failed to find To UOM.');
  Info.ResultAsFloat:= TUOMUtils.Convert(Info.ParamAsFloat[0],
    F.NameSingular, T.NameSingular);
end;

procedure TdmDWS.JDUOMFunctionsCubeEval(info: TProgramInfo);
begin
  Info.ResultAsFloat:= Power(Info.ParamAsFloat[0], 3);
end;

procedure TdmDWS.JDUOMFunctionsFindUOMEval(info: TProgramInfo);
var
  U: TUOM;
begin
  U:= TUOMUtils.FindUOM(info.ParamAsString[0]);
  if U = nil then
    raise Exception.Create('Failed to find UOM "'+info.ParamAsString[0]+'"');
  PopulateUOM(U, Info);
end;

procedure TdmDWS.JDUOMFunctionsPowerEval(info: TProgramInfo);
begin
  Info.ResultAsFloat:= Power(Info.ParamAsFloat[0], Info.ParamAsFloat[1]);
end;

procedure TdmDWS.JDUOMFunctionsRegisterBaseUOMEval(info: TProgramInfo);
var
  U: TUOM;
begin
  U:= TUOMUtils.FindUOM(info.ParamAsString[1]);
  if U = nil then
    raise Exception.Create('Failed to find UOM "'+info.ParamAsString[1]+'"');
  TUOMUtils.RegisterBaseUOM(info.ParamAsString[0], U);
end;

procedure TdmDWS.JDUOMFunctionsRegisterSimpleUOMEval(info: TProgramInfo);
var
  U: TUOM;
  L: TStringList;
  X: Integer;
begin
  U:= TUOMUtils.RegisterSimpleUOM(
    info.ParamAsString[0], //Category
    info.ParamAsString[1], //Singular
    info.ParamAsString[2], //Plural
    info.ParamAsString[3], //Suffix
    info.ParamAsString[4], //Systems
    info.ParamAsFloat[5], //Factor
    nil //Parent (TODO)
    );
  //Aliases
  L:= TStringList.Create;
  try
    L.Delimiter:= ',';
    L.StrictDelimiter:= True;
    L.DelimitedText:= info.ParamAsString[6];
    for X := 0 to L.Count-1 do begin
      U.AddAlias(L[X]);
    end;
  finally
    L.Free;
  end;
  PopulateUOM(U, Info);
end;

procedure TdmDWS.JDUOMFunctionsRegisterUOMEval(info: TProgramInfo);
var
  U: TUOM;
  L: TStringList;
  X: Integer;
begin
  U:= TUOMUtils.RegisterUOM(
    info.ParamAsString[0], //Category
    info.ParamAsString[1], //Singular
    info.ParamAsString[2], //Plural
    info.ParamAsString[3], //Suffix
    info.ParamAsString[4], //Systems
    info.ParamAsString[5], //From Base
    info.ParamAsString[6], //To Base
    0 //Factor
    );
  //Aliases
  L:= TStringList.Create;
  try
    L.Delimiter:= ',';
    L.StrictDelimiter:= True;
    L.DelimitedText:= info.ParamAsString[7];
    for X := 0 to L.Count-1 do begin
      U.AddAlias(L[X]);
    end;
  finally
    L.Free;
  end;
  PopulateUOM(U, Info);
end;

procedure TdmDWS.JDUOMFunctionsSqrEval(info: TProgramInfo);
begin
  Info.ResultAsFloat:= Power(Info.ParamAsFloat[0], 2);
end;

procedure TdmDWS.JDUOMFunctionsUOMByIndexEval(info: TProgramInfo);
var
  U: TUOM;
begin
  U:= TUOMUtils.GetUOMByIndex(Info.ParamAsInteger[0]);
  if U = nil then
    raise Exception.Create('Failed to find UOM.');
  PopulateUOM(U, Info);
end;

procedure TdmDWS.JDUOMFunctionsUOMCountEval(info: TProgramInfo);
begin
  Info.ResultAsInteger:= TUOMUtils.UOMCount;
end;

procedure TdmDWS.JDUOMFunctionsUOMEval(info: TProgramInfo);
var
  V: TUOMValue;
begin
  V:= TUOMUtils.StrToUOMValue(info.ParamAsString[0]);
  Info.ResultAsFloat:= V.BaseValue;
end;

procedure TdmDWS.JDUOMFunctionsUOMExistsEval(info: TProgramInfo);
var
  U: TUOM;
begin
  U:= TUOMUtils.FindUOM(Info.ParamAsString[0]);
  Info.ResultAsBoolean:= (U <> nil);
end;

procedure TdmDWS.JDUOMFunctionsUOMStringEval(info: TProgramInfo);
var
  V: Extended;
  U: TUOM;
  Res: String;
  Shorten: Boolean;
begin
  V:= Info.ParamAsFloat[0];
  Shorten:= Info.ParamAsBoolean[2];
  U:= TUOMUtils.FindUOM(Info.ParamAsString[1]);
  if U = nil then
    raise Exception.Create('Failed to find UOM.');
  Res:= FormatFloat(NumFormat, V);
  if Shorten then begin
    Res:= Res + U.Suffix;
  end else begin
    Res:= Res + ' ';
    if V = 1 then
      Res:= Res + U.NameSingular
    else
      Res:= Res + U.NamePlural;
  end;
  Info.ResultAsString:= Res;
end;

end.
