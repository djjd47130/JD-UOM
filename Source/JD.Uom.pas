unit JD.Uom;

interface

{$REGION 'README'}

(*
  JD Unit of Measurement Utilities
  by Jerry Dodge

  Encapsulates all possible units of measurement with conversion
  and other useful utilities to implement in Delphi.

  NOTE: The following details are out of date, as things are getting
  documented directly on GitHub, and this documentation was forotten...
  https://github.com/djjd47130/JD-UOM

  Measurement Systems:
  - Metric (Meters, Liters, Kilograms...)
  - US Customary (Feet, Gallons, Pounds...)
  - Imperial (Similar to US Customary but UK based)
  - Natural
  - Random

  UPDATED:
  - International System of Units (SI):
    - The SI comprises a coherent system of units of measurement, including seven base units:
      - Second (s): The unit of time.
      - Meter (m): The unit of length.
      - Kilogram (kg): The unit of mass.
      - Ampere (A): The unit of electric current.
      - Kelvin (K): The unit of thermodynamic temperature.
      - Mole (mol): The unit of amount of substance.
      - Candela (cd): The unit of luminous intensity .
  - British Imperial System:
    - Historically used in the United Kingdom and its former colonies.
    - Includes units like inches, feet, pounds, gallons, and more.
  - United States Customary System:
    - Commonly used in the United States.
    - Includes units like inches, feet, pounds, gallons, and more.
  - Natural Units:
    - Derived from fundamental physical constants.
    - Used in theoretical physics.
  - Non-Standard Units:
    - Various local or specialized units used for specific purposes.

  References:
  - https://www.metric-conversions.org/
    - Original reference
  - https://convertlive.com/
    - Newly discovered reference with many more UOMs

*)

{$ENDREGION}

//Disabled until string mathematical expressions can be implemented.
//  https://wiki.delphi-jedi.org/wiki/JCL_Help:JclExprEval.pas
//  OR
//  https://gobestcode.com/html/math_parser_for_delphi.html
{ $DEFINE USE_MATH_EXPR}

uses
  System.Classes, System.SysUtils, System.Generics.Collections
  {$IFDEF USE_MATH_EXPR}
  , JclExprEval
  {$ENDIF}
  ;

const
  PartOfNumber = ['0'..'9', '.', ','];
  NumFormat = '#,###,###,###,##0.#############';

type
  TUOM = class;
  TUOMUtils = class;

  /// <summary>
  /// Base conversion function for any given UOM.
  /// </summary>
  TConvertProc = Reference to function(const Value: Double): Double;

  /// <summary>
  /// Base object for each possible UOM unit.
  /// </summary>
  TUOM = class(TObject)
  private
    FCategory: String;
    FSystems: TStringList;
    FNameSingular: String;
    FNamePlural: String;
    FPrefix: String;
    FSuffix: String;
    {$IFDEF USE_MATH_EXPR}
    FConvertToBaseFormula: String;
    FConvertFromBaseFormula: String;
    {$ELSE}
    FConvertToBaseProc: TConvertProc;
    FConvertFromBaseProc: TConvertProc;
    {$ENDIF}
    function GetSystems: TStrings;
    procedure SystemsChanged(Sender: TObject);
    procedure SetNamePlural(const Value: String);
    procedure SetNameSingular(const Value: String);
    procedure SetPrefix(const Value: String);
    procedure SetSuffix(const Value: String);
    procedure SetSystems(const Value: TStrings);
    procedure SetCategory(const Value: String);
    {$IFDEF USE_MATH_EXPR}
    procedure SetConvertFromBaseFormula(const Value: TConvertProc);
    procedure SetConvertToBaseFormula(const Value: TConvertProc);
    {$ELSE}
    procedure SetConvertFromBaseProc(const Value: TConvertProc);
    procedure SetConvertToBaseProc(const Value: TConvertProc);
    {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(const ACategory, ANameSingular, ANamePlural, APrefix, ASuffix,
      ASystems: String; const AFromBase: TConvertProc = nil; const AToBase: TConvertProc = nil); overload;
    destructor Destroy; override;
    procedure Invalidate; virtual;
    {$IFDEF USE_MATH_EXPR}
    property ConvertFromBaseFormula: String read FConvertFromBaseFormula write SetConvertFromBaseFormula;
    property ConvertToBaseFormula: String read FConvertToBaseFormula write SetConvertToBaseFormula;
    {$ELSE}
    property ConvertFromBaseProc: TConvertProc read FConvertFromBaseProc write SetConvertFromBaseProc;
    property ConvertToBaseProc: TConvertProc read FConvertToBaseProc write SetConvertToBaseProc;
    {$ENDIF}
    property Category: String read FCategory write SetCategory;
    property Systems: TStrings read GetSystems write SetSystems;
    property NameSingular: String read FNameSingular write SetNameSingular;
    property NamePlural: String read FNamePlural write SetNamePlural;
    property Prefix: String read FPrefix write SetPrefix;
    property Suffix: String read FSuffix write SetSuffix;
    function ConvertFromBase(const AValue: Double): Double;
    function ConvertToBase(const AValue: Double): Double;
  end;

  /// <summary>
  /// Main class encapsulating entire UOM library capabilities.
  /// </summary>
  TUOMUtils = class
  private
    class var FUOMs: TObjectList<TUOM>;
    class var FBaseUOMs: TDictionary<String, TUOM>;
    class var FSystems: TStringList;
    class var FCategories: TStringList;
    {$IFDEF USE_MATH_EXPR}
    class var FEval: TEvaluator;
    {$ENDIF}
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Invalidate; virtual;
    class function GetUOMByIndex(const Index: Integer): TUOM; static;
    class function GetUOMByName(const Name: String): TUOM; static;
    class function GetUOMByPrefix(const Prefix: String): TUOM; static;
    class function GetUOMBySuffix(const Suffix: String): TUOM; static;
    class function GetBaseUOM(const UOM: String): TUOM; static;
    class function CategoryCount: Integer; static;
    class function SystemCount: Integer; static;
    class procedure ListCategories(AList: TStrings); static;
    class procedure ListSystems(AList: TStrings); static;
    class procedure ListUOMs(AList: TStrings; const ACategory: String = '';
      const ASystems: String = ''); static;
    class function UOMCount: Integer; static;
    class procedure RegisterUOM(const AUnit: TUOM); static;
    class procedure RegisterBaseUOM(const ACategory: String; const AUnit: TUOM); static;
    class function Convert(const Value: Double; const FromUOM, ToUOM: String): Double; static;

    class property UOMs[const Index: Integer]: TUOM read GetUOMByIndex; default;
  end;

  //TODO: Abstract record to encapsulate a single possible value attached to a specific UOM category
  TUOMValue = record
  private
    FUOM: String;
    FValue: Double;
    procedure SetUOM(const Value: String);
    procedure SetValue(const Value: Double);
  public
    property UOM: String read FUOM write SetUOM;
    property Value: Double read FValue write SetValue;
  end;


////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////

{
class procedure TUOMUtils.ParseSuffix(AValue: String; var ANumber: Double; var ASuffix: String);
var
  X: Integer;
  NS: String;
begin
  //Separates the suffix from a number in a string, and returns the value separate from the suffix.
  AValue:= Trim(AValue);
  ANumber:= 0;
  ASuffix:= '';
  NS:= '';
  //Iterate through characters...
  for X := 1 to Length(AValue) do begin
    if CharInSet(AValue[X], PartOfNumber) then begin
      //Append to value if it's not a thousands separator
      if AValue[X] <> ',' then
        NS:= NS + AValue[X];
    end else begin
      //Not a number, assume the rest is the suffix.
      ASuffix:= Trim(Copy(AValue, X, Length(AValue)));
      Break;
    end;
  end;
  ANumber:= StrToFloatDef(NS, 0);
end;
}

{ TUOM }

constructor TUOM.Create;
begin
  FSystems:= TStringList.Create;
  FSystems.OnChange:= SystemsChanged;
end;

constructor TUOM.Create(const ACategory, ANameSingular, ANamePlural, APrefix, ASuffix,
  ASystems: String; const AFromBase: TConvertProc = nil; const AToBase: TConvertProc = nil);
begin
  Create;
  FCategory:= ACategory;
  FNameSingular:= ANameSingular;
  FNamePlural:= ANamePlural;
  FPrefix:= APrefix;
  FSuffix:= ASuffix;
  FSystems.Delimiter:= ',';
  FSystems.StrictDelimiter:= True;
  FSystems.DelimitedText:= ASystems;
  FConvertFromBaseProc:= AFromBase;
  FConvertToBaseProc:= AToBase;
  //TODO: Validate...

  //TODO: This doesn't belong here, implement invalidate methods...
  TUOMUtils.ListCategories(TUOMUtils.FCategories);
  TUOMUtils.ListSystems(TUOMUtils.FSystems);
end;

destructor TUOM.Destroy;
begin
  FreeAndNil(FSystems);
end;

procedure TUOM.Invalidate;
begin
  TUOMUtils.Invalidate;
end;

function TUOM.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= Self.FConvertFromBaseProc(AValue);
end;

function TUOM.ConvertToBase(const AValue: Double): Double;
begin
  Result:= Self.FConvertToBaseProc(AValue);
end;

{$IFDEF USE_MATH_EXPR}

procedure TUOMLookupUnit.SetConvertFromBaseFormula(const Value: String);
begin
  FConvertFromBaseFormula := Value;
  //TODO: Validate...
  Invalidate;
end;

procedure TUOMLookupUnit.SetConvertToBaseFormula(const Value: String);
begin
  FConvertToBaseFormula := Value;
  //TODO: Validate...
  Invalidate;
end;

{$ELSE}

procedure TUOM.SetConvertFromBaseProc(const Value: TConvertProc);
begin
  FConvertFromBaseProc := Value;
  //TODO: Validate...
  Invalidate;
end;

procedure TUOM.SetConvertToBaseProc(const Value: TConvertProc);
begin
  FConvertToBaseProc := Value;
  //TODO: Validate...
  Invalidate;
end;

{$ENDIF}

function TUOM.GetSystems: TStrings;
begin
  Result:= TStrings(FSystems);
end;

procedure TUOM.SetNamePlural(const Value: String);
begin
  FNamePlural:= Value;
  //TODO: Validate...
end;

procedure TUOM.SetNameSingular(const Value: String);
begin
  FNameSingular:= Value;
  //TODO: Validate...
end;

procedure TUOM.SetPrefix(const Value: String);
begin
  FPrefix:= Value;
  //TODO: Validate...
end;

procedure TUOM.SetSuffix(const Value: String);
begin
  FSuffix:= Value;
  //TODO: Validate...
end;

procedure TUOM.SetSystems(const Value: TStrings);
begin
  FSystems.Assign(Value);
  TUOMUtils.ListSystems(TUOMUtils.FSystems);
end;

procedure TUOM.SetCategory(const Value: String);
begin
  FCategory:= Value;
  TUOMUtils.ListCategories(TUOMUtils.FCategories);
end;

procedure TUOM.SystemsChanged(Sender: TObject);
begin
  TUOMUtils.ListSystems(TUOMUtils.FSystems);
end;

{ TUOMUtils }

class constructor TUOMUtils.Create;
begin
  FUOMs:= TObjectList<TUOM>.Create(True);
  FBaseUOMs:= TDictionary<String, TUOM>.Create;
  FSystems:= TStringList.Create;
  FCategories:= TStringList.Create;
end;

class destructor TUOMUtils.Destroy;
begin
  FreeAndNil(FCategories);
  FreeAndNil(FSystems);
  FreeAndNil(FBaseUOMs);
  FreeAndNil(FUOMs);
end;

class function TUOMUtils.Convert(const Value: Double; const FromUOM,
  ToUOM: String): Double;
var
  F, T: TUOM;
begin
  //MAIN CONVERSION FUNCTION - Dynamically calls relevant unit conversion procs.

  F:= GetUOMByName(FromUOM);
  if not Assigned(F) then begin
    raise Exception.Create('Conversion from unit "'+F.NameSingular+'" not found.');
  end;

  T:= GetUOMByName(ToUOM);
  if not Assigned(T) then begin
    raise Exception.Create('Conversion to unit "'+F.NameSingular+'" not found.');
  end;

  if not Assigned(F.ConvertFromBaseProc) then begin
    raise Exception.Create('Conversion from proc is not assigned.');
  end;

  if not Assigned(T.ConvertToBaseProc) then begin
    raise Exception.Create('Conversion to proc is not assigned.');
  end;

  try
    {$IFDEF USE_MATH_EXPR}
    //TODO: Perform conversion using mathematical expressions...

    {$ELSE}
    Result:= F.FConvertToBaseProc(Value);
    Result:= T.FConvertFromBaseProc(Result);
    {$ENDIF}
  except
    on E: Exception do begin
      raise Exception.Create('Conversion functions failed: '+E.Message);
    end;
  end;

end;

class procedure TUOMUtils.Invalidate;
begin
  ListCategories(FCategories);
  ListSystems(FSystems);
end;

class function TUOMUtils.GetBaseUOM(const UOM: String): TUOM;
begin
  Result:= FBaseUOMs[UOM];
end;

class function TUOMUtils.GetUOMByIndex(const Index: Integer): TUOM;
begin
  Result:= FUOMs[Index];
end;

class function TUOMUtils.GetUOMByName(const Name: String): TUOM;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUOMs.Count-1 do begin
    if Trim(Name) = Trim(FUOMs[X].FNameSingular) then begin
      Result:= FUOMs[X];
      Break;
    end;
  end;
end;

class function TUOMUtils.GetUOMByPrefix(const Prefix: String): TUOM;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUOMs.Count-1 do begin
    if Trim(Prefix) = Trim(FUOMs[X].FPrefix) then begin
      Result:= FUOMs[X];
      Break;
    end;
  end;
end;

class function TUOMUtils.GetUOMBySuffix(const Suffix: String): TUOM;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUOMs.Count-1 do begin
    if Trim(Suffix) = Trim(FUOMs[X].FSuffix) then begin
      Result:= FUOMs[X];
      Break;
    end;
  end;
end;

class procedure TUOMUtils.ListSystems(AList: TStrings);
var
  X, Y: Integer;
  UN: String;
begin
  AList.Clear;
  for X := 0 to FUOMs.Count-1 do begin
    for Y := 0 to FUOMs[X].FSystems.Count-1 do begin
      UN:= FUOMs[X].FSystems[Y];
      if AList.IndexOf(UN) < 0 then
        AList.Append(UN);
    end;
  end;
end;

class procedure TUOMUtils.ListUOMs(AList: TStrings; const ACategory: String = '';
  const ASystems: String = '');
var
  Inc: Boolean;
  X, Y: Integer;
  U: TUOM;
  L: TStringList;
begin
  AList.Clear;
  Inc:= True;

  for X := 0 to FUOMs.Count-1 do begin
    U:= FUOMs[X];

    //Filter by UOM...
    if (ACategory <> '') then begin
      Inc:= SameText(U.Category, ACategory)
    end;

    //Filter by systems..
    if Inc and (ASystems <> '') then begin
      L:= TStringList.Create;
      try
        L.Delimiter:= ',';
        L.StrictDelimiter:= True;
        L.DelimitedText:= ASystems;
        Inc:= False;
        for Y := 0 to U.Systems.Count-1 do begin
          if not Inc then
            Inc:= L.IndexOf(U.Systems[Y]) >= 0;
        end;
      finally
        L.Free;
      end;
    end;

    if Inc then begin
      AList.AddObject(U.NameSingular, U);
    end;
  end;
end;

class procedure TUOMUtils.ListCategories(AList: TStrings);
var
  X: Integer;
  UN: String;
begin
  AList.Clear;
  for X := 0 to FUOMs.Count-1 do begin
    UN:= FUOMs[X].FCategory;
    if AList.IndexOf(UN) < 0 then
      AList.Append(UN);
  end;
end;

class procedure TUOMUtils.RegisterBaseUOM(const ACategory: String;
  const AUnit: TUOM);
begin
  FBaseUOMs.Add(ACategory, AUnit);
end;

class procedure TUOMUtils.RegisterUOM(const AUnit: TUOM);
begin
  if AUnit = nil then begin
    raise Exception.Create('Cannot register unassigned unit object.');
  end;

  if Trim(AUnit.FNameSingular) = '' then begin
    raise Exception.Create('Cannot register blank unit name.');
  end;

  if Trim(AUnit.FNamePlural) = '' then begin
    raise Exception.Create('Cannot register blank unit name.');
  end;

  if Trim(AUnit.FSuffix) = '' then begin
    raise Exception.Create('Cannot register blank unit suffix.');
  end;

  if Assigned(GetUOMByName(AUnit.FNameSingular)) or
    Assigned(GetUOMByName(AUnit.FNamePlural)) then
  begin
    raise Exception.Create('Cannot register duplicate unit name '+AUnit.FNameSingular);
  end;

  if Assigned(GetUOMBySuffix(AUnit.FSuffix)) then begin
    raise Exception.Create('Cannot register duplicate unit suffix '+AUnit.FSuffix);
  end;

  FUOMs.Add(AUnit);
  Invalidate;
end;

class function TUOMUtils.SystemCount: Integer;
begin
  Invalidate;
  Result:= FSystems.Count;
end;

class function TUOMUtils.UOMCount: Integer;
begin
  Result:= FUOMs.Count;
end;

class function TUOMUtils.CategoryCount: Integer;
begin
  Invalidate;
  Result:= FCategories.Count;
end;

{ TUOMValue }

procedure TUOMValue.SetUOM(const Value: String);
begin
  FUOM := Value;
end;

procedure TUOMValue.SetValue(const Value: Double);
begin
  FValue := Value;
end;

initialization

end.
