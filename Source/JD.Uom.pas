﻿unit JD.Uom;

interface

{$REGION 'README'}

(*
  JD Unit of Measurement Utilities
  by Jerry Dodge

  Encapsulates all possible units of measurement with conversion
  and other useful utilities to implement in Delphi.

  NOTE: This project is hosted and documented on GitHub:
  https://github.com/djjd47130/JD-UOM

  Measurement Systems:
  - Metric (Meters, Liters, Kilograms...)
  - US Customary (Feet, Gallons, Pounds...)
  - Imperial (Similar to US Customary but UK based)
  - Natural (Kelvin, Light Years...)
  - Random (Bananas, iPhones...)
  NOTE: A single UOM might be related to more than one system,
    such as both Imperial and US Customary. In this case, "Systems" strings
    must be comma-separated values, such as `Imperial,US Customary`.
    The library will automatically parse this into individual list items.
    Be sure to reuse the existing systems when necessary, as to not create
    duplicate systems.

  According to references:
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

//Defines whether to use string-based mathematical expressions
//  Disabled until implemented.
//  https://wiki.delphi-jedi.org/wiki/JCL_Help:JclExprEval.pas
//  TODO: Actually, let's make use of DWS (Delphi Web Script)!
{ $DEFINE USE_MATH_EXPR}

{$IFDEF USE_MATH_EXPR}
  { $DEFINE USE_JEDI}
  {$DEFINE USE_DWS}
{$ENDIF}

uses
  System.Classes, System.SysUtils, System.Generics.Collections
  {$IFDEF USE_JEDI}
  , JclExprEval
  {$ENDIF}
  {$IFDEF USE_DWS}
  //TODO
  {$ENDIF}
  ;

type
  TUOMException = Exception;
  TUOMInvalidUnitException = TUOMException;
  TUOMOutOfRangeException = TUOMException;



type
  /// <summary>
  /// Enum representing a specific Metric system size.
  /// </summary>
  TUOMMetricUnit = (msFemto, msPico, msNano, msMicro, msMilli, msCenti, msDeci,
    msBase, msDeca, msHecto, msKilo, msMega, msGiga, msTera, msPeta);
  TUOMMetricUnits = set of TUOMMetricUnit;

  TUOMMetricUtils = class
  private
  public

  end;

const
  PartOfNumber = ['0'..'9', '.', ','];
  NumFormat = '#,###,###,###,###,##0.##################';

  METRIC_FEMTO = 0.000000000000001;
  METRIC_PICO  = 0.000000000001;
  METRIC_NANO  = 0.000000001;
  METRIC_MICRO = 0.000001;
  METRIC_MILLI = 0.001;
  METRIC_CENTI = 0.01;
  METRIC_DECI  = 0.1;
  METRIC_DECA  = 10;
  METRIC_HECTO = 100;
  METRIC_KILO  = 1000;
  METRIC_MEGA  = 1000000;
  METRIC_GIGA  = 1000000000;
  METRIC_TERA  = 1000000000000;
  METRIC_PETA  = 1000000000000000;
  //TODO

type
  TUOM = class;
  TUOMUtils = class;

  {$IFNDEF USE_MATH_EXPR}
  /// <summary>
  /// Base conversion function for any given UOM.
  /// </summary>
  TConvertProc = Reference to function(const Value: Double): Double;
  {$ENDIF}

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
    /// <summary>
    /// A change has been made which requires parent TUOMUtils to refresh its cache.
    /// </summary>
    procedure Invalidate; virtual;
    /// <summary>
    /// Assigns this UOM as the base UOM of its specified Category.
    /// </summary>
    procedure SetAsBase;
    /// <summary>
    /// Returns a value converted FROM the base unit TO the specified unit.
    /// </summary>
    function ConvertFromBase(const AValue: Double): Double;
    /// <summary>
    /// Returns a value converted FROM the specified unit TO the base unit.
    /// </summary>
    function ConvertToBase(const AValue: Double): Double;
  public
    {$IFDEF USE_MATH_EXPR}
    /// <summary>
    /// A string containing a mathematical expression to convert the `Value` FROM the base UOM.
    /// </summary>
    property ConvertFromBaseFormula: String read FConvertFromBaseFormula write SetConvertFromBaseFormula;
    /// <summary>
    /// A string containing a mathematical expression to convert the `Value` TO the base UOM.
    /// </summary>
    property ConvertToBaseFormula: String read FConvertToBaseFormula write SetConvertToBaseFormula;
    {$ELSE}
    /// <summary>
    /// A TConvertProc reference to convert the `Value` FROM the base UOM.
    /// </summary>
    property ConvertFromBaseProc: TConvertProc read FConvertFromBaseProc write SetConvertFromBaseProc;
    /// <summary>
    /// A TConvertProc reference to convert the `Value` TO the base UOM.
    /// </summary>
    property ConvertToBaseProc: TConvertProc read FConvertToBaseProc write SetConvertToBaseProc;
    {$ENDIF}
    /// <summary>
    /// The major group of UOMs (Distance, Area, Volume, Mass, Temperature, etc.)
    /// </summary>
    property Category: String read FCategory write SetCategory;
    /// <summary>
    /// The systematic group(s) of UOMs (Metric, Imperial, US Customary...)
    /// </summary>
    property Systems: TStrings read GetSystems write SetSystems;
    /// <summary>
    /// The singular (value = 1) name of the UOM (Meter, Foot, Gram...).
    /// Also its unique identifier - cannot create duplicates.
    /// </summary>
    property NameSingular: String read FNameSingular write SetNameSingular;
    /// <summary>
    /// The plural (value <> 1) name of the TUOM (Meters, Feet, Grams...).
    /// </summary>
    property NamePlural: String read FNamePlural write SetNamePlural;
    /// <summary>
    /// (NOT IMPLEMENTED) Prefix showing before a given UOM value.
    /// </summary>
    property Prefix: String read FPrefix write SetPrefix;
    /// <summary>
    /// Suffix showing after a given UOM value.
    /// Also a unique CASE-SENSITIVE identifer - cannot create duplicates.
    /// </summary>
    property Suffix: String read FSuffix write SetSuffix;
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
    /// <summary>
    /// Returns a TUOM object based on a given list inded of the master UOM list.
    /// </summary>
    class function GetUOMByIndex(const Index: Integer): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's unique SINGULAR name.
    /// </summary>
    class function GetUOMByName(const Name: String): TUOM; static;
    /// <summary>
    /// TODO: Remove...?
    /// </summary>
    class function GetUOMByPrefix(const Prefix: String): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's u ique suffix (case sensitive).
    /// </summary>
    class function GetUOMBySuffix(const Suffix: String): TUOM; static;
    /// <summary>
    /// Returns a TUOM object of the given Category's base unit.
    /// </summary>
    class function GetBaseUOM(const Category: String): TUOM; static;
    /// <summary>
    /// Returns the number of UOM categories (Distance, Area, Temperature, Mass...)
    /// </summary>
    class function CategoryCount: Integer; static;
    /// <summary>
    /// Returns the number of UOM systems (Metric, Imperial, US Customary...)
    /// </summary>
    class function SystemCount: Integer; static;
    /// <summary>
    /// Populates a given TStrings object with all possible Categories.
    /// </summary>
    class procedure ListCategories(AList: TStrings); static;
    /// <summary>
    /// Populates a given TStrings object with all possible Systems.
    /// </summary>
    class procedure ListSystems(AList: TStrings); static;
    /// <summary>
    /// Populates a given TStrings object with all possible UOMs matching
    /// the given filter parameters.
    /// </summary>
    class procedure ListUOMs(AList: TStrings; const ACategory: String = '';
      const ASystems: String = ''); static;
    /// <summary>
    /// Returns the total number of specific UOMs registered.
    /// </summary>
    class function UOMCount: Integer; static;
    /// <summary>
    /// Registers a new TUOM object into the UOM system.
    /// NOTE: Validation will be done to ensure unique (case sensitive) UOM names,
    /// as well as unique suffixes.
    /// </summary>
    class function RegisterUOM(const AUnit: TUOM): TUOM;  overload; static;
    /// <summary>
    /// Registers a new TUOM object into the UOM system based on a variety of parameters.
    /// NOTE: Validation will be done to ensure unique (case sensitive) UOM names,
    /// as well as unique suffixes.
    /// </summary>
    class function RegisterUOM(const ACategory, ANameSingular, ANamePlural,
      APrefix, ASuffix, ASystems: String;
      const AFromBase: TConvertProc = nil;
      const AToBase: TConvertProc = nil): TUOM; overload; static;
    /// <summary>
    /// Registers the BASE UOM for a given UOM Category. For example,
    /// Meters for Distance, Grams for Mass, Celsius for Temperature...
    /// </summary>
    class procedure RegisterBaseUOM(const ACategory: String; const AUnit: TUOM); static;
    /// <summary>
    /// Converts a given `Value` from a given `FromUOM` to a given `ToUOM` as a `Double`.
    /// Dynamically calls appropriate mechanism based on defined conditionals.
    /// </summary>
    class function Convert(const Value: Double; const FromUOM, ToUOM: String): Double; static;
    /// <summary>
    /// Accesses any registered TUOM object by its given master list index.
    /// </summary>
    class property UOMs[const Index: Integer]: TUOM read GetUOMByIndex; default;
  end;

  /// <summary>
  /// (NOT READY)
  /// Abstract record to encapsulate a single possible value attached to a specific UOM category
  /// </summary>
  TUOMValue = record
  private
    FUOM: String;
    FBaseValue: Double;
    procedure SetUOM(const Value: String);
    procedure SetBaseValue(const Value: Double);
    function GetConvertedValue: Double;
    procedure SetConvertedValue(const Value: Double);
    function GetUomObj: TUOM;
  public
    class operator Implicit(const Value: TUOMValue): Double;
    class operator Implicit(const Value: Double): TUOMValue;
    class operator Implicit(const Value: TUOMValue): String;
    class operator Implicit(const Value: String): TUOMValue;
    class operator Trunc(const Value: TUOMValue): TUOMValue;
    class operator Round(const Value: TUOMValue): TUOMValue;
    class operator Positive(const A: TUOMValue): TUOMValue;
    class operator Negative(const A: TUOMValue): TUOMValue;
    class operator Inc(const A: TUOMValue): TUOMValue;
    class operator Dec(const A: TUOMValue): TUOMValue;
    class operator Equal(const A, B: TUOMValue): Boolean;
    class operator NotEqual(const A, B: TUOMValue): Boolean;
    class operator GreaterThan(const A, B: TUOMValue): Boolean;
    class operator GreaterThanOrEqual(const A, B: TUOMValue): Boolean;
    class operator LessThan(const A, B: TUOMValue): Boolean;
    class operator LessThanOrEqual(const A, B: TUOMValue): Boolean;
    class operator Add(const A, B: TUOMValue): TUOMValue;
    class operator Subtract(const A, B: TUOMValue): TUOMValue;
    class operator Multiply(const A, B: TUOMValue): TUOMValue;
    class operator Divide(const A, B: TUOMValue): TUOMValue;
  public
    /// <summary>
    /// Returns the specified Unit-of-Measurement associated with the value.
    /// </summary>
    property UOM: String read FUOM write SetUOM;
    /// <summary>
    /// Returns the raw base value, unconverted.
    /// </summary>
    property BaseValue: Double read FBaseValue write SetBaseValue;
    /// <summary>
    /// Returns the value converted FROM the base TO the specified unit.
    /// </summary>
    property ConvertedValue: Double read GetConvertedValue write SetConvertedValue;
  end;

  /// <summary>
  /// A record that can be reused for things like Speed or Frequency,
  ///   where two different UOM categories are compared with each other.
  ///   For example, Miles per Hour, Meters per Second, Bananas per Cubic Yard...
  /// Task #45
  /// </summary>
  TUOMCombinedValue = record
  private

  public

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
  FSystems.Delimiter:= ',';
  FSystems.StrictDelimiter:= True;
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
  FSystems.DelimitedText:= ASystems;
  FConvertFromBaseProc:= AFromBase;
  FConvertToBaseProc:= AToBase;
  //TODO: Validate...

  Invalidate;
end;

destructor TUOM.Destroy;
begin
  FreeAndNil(FSystems);
end;

procedure TUOM.Invalidate;
begin
  TUOMUtils.Invalidate;
end;

procedure TUOM.SetAsBase;
begin
  TUOMUtils.RegisterBaseUOM(FCategory, Self);
end;

function TUOM.ConvertFromBase(const AValue: Double): Double;
begin
  if not Assigned(FConvertFromBaseProc) then
    raise TUOMException.Create('Conversion from base function not assigned!');
  Result:= FConvertFromBaseProc(AValue);
end;

function TUOM.ConvertToBase(const AValue: Double): Double;
begin
  if not Assigned(FConvertToBaseProc) then
    raise TUOMException.Create('Conversion to base function not assigned!');
  Result:= FConvertToBaseProc(AValue);
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
  //MAIN CONVERSION FUNCTION - Dynamically calls relevant unit conversion methods.

  F:= GetUOMByName(FromUOM);
  if not Assigned(F) then begin
    raise TUOMInvalidUnitException.Create('Conversion from unit "'+F.NameSingular+'" not found.');
  end;

  T:= GetUOMByName(ToUOM);
  if not Assigned(T) then begin
    raise TUOMInvalidUnitException.Create('Conversion to unit "'+F.NameSingular+'" not found.');
  end;

  if not Assigned(F.ConvertFromBaseProc) then begin
    raise TUOMException.Create('Conversion from proc is not assigned.');
  end;

  if not Assigned(T.ConvertToBaseProc) then begin
    raise TUOMException.Create('Conversion to proc is not assigned.');
  end;

  try
    {$IFDEF USE_MATH_EXPR}
    //TODO: Perform conversion using string-based mathematical expressions...

    {$ELSE}
    Result:= F.FConvertToBaseProc(Value);
    Result:= T.FConvertFromBaseProc(Result);
    {$ENDIF}
  except
    on E: Exception do begin
      raise TUOMException.Create('Conversion functions failed: '+E.Message);
    end;
  end;

end;

class procedure TUOMUtils.Invalidate;
begin
  ListCategories(FCategories);
  ListSystems(FSystems);
end;

class function TUOMUtils.GetBaseUOM(const Category: String): TUOM;
begin
  Result:= FBaseUOMs[Category];
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
  Invalidate;
end;

class function TUOMUtils.RegisterUOM(const ACategory, ANameSingular,
  ANamePlural, APrefix, ASuffix, ASystems: String; const AFromBase,
  AToBase: TConvertProc): TUOM;
begin
  Result:= TUOM.Create;
  try
    Result.FCategory:= ACategory;
    Result.FNameSingular:= ANameSingular;
    Result.FNamePlural:= ANamePlural;
    Result.FPrefix:= APrefix;
    Result.FSuffix:= ASuffix;
    Result.FSystems.Delimiter:= ',';
    Result.FSystems.StrictDelimiter:= True;
    Result.FSystems.DelimitedText:= ASystems;
    Result.FConvertFromBaseProc:= AFromBase;
    Result.FConvertToBaseProc:= AToBase;
  finally
    try
      RegisterUOM(Result);
    except
      Result.Free;
    end;
  end;
  Invalidate;
end;

class function TUOMUtils.RegisterUOM(const AUnit: TUOM): TUOM;
begin
  Result:= AUnit;

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

class operator TUOMValue.Implicit(const Value: TUOMValue): Double;
var
  U: TUOM;
begin
  //Implicitly return the CONVERTED value...
  U:= TUOMUtils.GetUOMByName(Value.FUOM);
  if U = nil then
    raise Exception.Create('UOM "'+Value.FUOM+'" not found!');
  Result:= U.ConvertToBase(Value.FBaseValue);
end;

class operator TUOMValue.Implicit(const Value: Double): TUOMValue;
//var
  //U: TUOM;
begin
  //Implicitly return the BASE value...
  //TODO: UOM?
  Result.FUOM:= '';
  Result.FBaseValue:= Value;
end;

class operator TUOMValue.Implicit(const Value: TUOMValue): String;
var
  U: TUOM;
  V: Double;
begin
  //Implicitly return the CONVERTED value formatted as string...
  U:= TUOMUtils.GetUOMByName(Value.FUOM);
  if U = nil then
    raise Exception.Create('UOM "'+Value.FUOM+'" not found!');
  V:= U.ConvertToBase(Value.FBaseValue);
  Result:= FormatFloat(NumFormat, V)+' '+U.FSuffix;
end;

class operator TUOMValue.Implicit(const Value: String): TUOMValue;
begin
  //TODO: Parse string and its potential prefix/suffix???
end;

class operator TUOMValue.GreaterThan(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue > B.FBaseValue;
end;

class operator TUOMValue.GreaterThanOrEqual(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue >= B.FBaseValue;
end;

class operator TUOMValue.Add(const A, B: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= (A.FBaseValue + B.FBaseValue);
end;

class operator TUOMValue.Dec(const A: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= A.FBaseValue - 1;
end;

class operator TUOMValue.Equal(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue = B.FBaseValue;
end;

class operator TUOMValue.Inc(const A: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= A.FBaseValue + 1;
end;

class operator TUOMValue.LessThan(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue < B.FBaseValue;
end;

class operator TUOMValue.LessThanOrEqual(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue <= B.FBaseValue;
end;

class operator TUOMValue.Divide(const A, B: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= A.FBaseValue / B.FBaseValue;
end;

class operator TUOMValue.Multiply(const A, B: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= A.FBaseValue * B.FBaseValue;
end;

class operator TUOMValue.Negative(const A: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= -A.FBaseValue;
end;

class operator TUOMValue.NotEqual(const A, B: TUOMValue): Boolean;
begin
  Result:= A.FBaseValue <> B.FBaseValue;
end;

class operator TUOMValue.Positive(const A: TUOMValue): TUOMValue;
begin
  Result.FUOM:= A.FUOM;
  Result.FBaseValue:= +A.FBaseValue;
end;

class operator TUOMValue.Round(const Value: TUOMValue): TUOMValue;
begin
  Result.FUOM:= Value.FUOM;
  Result.FBaseValue:= Round(Value.FBaseValue);
end;

function TUOMValue.GetConvertedValue: Double;
var
  U: TUOM;
begin
  Result:= 0;
  U:= GetUomObj;
  if not Assigned(U) then
    raise Exception.Create('Failed to get converted value: Unit-of-Measure "'+FUOM+'" not found.');
  Result:= U.ConvertFromBase(FBaseValue);
end;

function TUOMValue.GetUomObj: TUOM;
begin
  Result:= TUOMUtils.GetUOMByName(FUOM);
end;

procedure TUOMValue.SetConvertedValue(const Value: Double);
var
  U: TUOM;
begin
  U:= GetUomObj;
  if not Assigned(U) then
    raise Exception.Create('Failed to set converted value: Unit-of-Measure "'+FUOM+'" not found.');
  FBaseValue:= U.ConvertToBase(Value);
end;

procedure TUOMValue.SetUOM(const Value: String);
begin
  FUOM := Value;
end;

class operator TUOMValue.Subtract(const A, B: TUOMValue): TUOMValue;
begin
  Result.FBaseValue:= A.FBaseValue - B.FBaseValue;
end;

class operator TUOMValue.Trunc(const Value: TUOMValue): TUOMValue;
begin
  Result.FBaseValue:= Trunc(Value.FBaseValue);
end;

procedure TUOMValue.SetBaseValue(const Value: Double);
begin
  FBaseValue := Value;
end;

initialization

end.
