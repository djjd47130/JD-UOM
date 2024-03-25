unit JD.Uom;

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

  UOM Systems - According to references:
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

  UOM Systems - As implemented in library:
  - Metric (Tiny)
  - Metric
  - Metric (Huge)
  - Imperial (Tiny)
  - Imperial
    - NOTE: "Imperial" is often referred to as "UK" in this library.
  - Imperial (Huge)
  - US Customary (Tiny)
  - US Customary
  - US Customary (Huge)
  - Natural
  - Random

  References:
  - https://www.metric-conversions.org/
    - Original reference
  - https://convertlive.com/
    - Newly discovered reference with many more UOMs
  - https://docwiki.embarcadero.com/RADStudio/Sydney/en/Operator_Overloading_%28Delphi%29
    - Class operator documentation

*)

{$ENDREGION}

uses
  System.Classes, System.SysUtils, System.Generics.Collections,
  JD.Uom.Expr;

type

  /// <summary>
  /// Main type to be used across UOM library for all UOM numbers.
  /// Was Double, but Extended fixed several issues.
  /// </summary>
  UOMNum = Extended;



const

  /// <summary>
  /// Specifies which characters are part of a number.
  /// Used for deserializing UOM strings.
  /// </summary>
  PartOfNumber = ['0'..'9', '.', ','];
  /// <summary>
  /// The format for converting value to String for display.
  /// </summary>
  NumFormat = '#,###,###,###,###,###,###,##0.##################################';
  /// <summary>
  /// The format for converting value to String for internal use.
  /// </summary>
  NumInternalFormat = '#####################0.##################################';



  //Standard dictionary numbers
  //https://en.wikipedia.org/wiki/Names_of_large_numbers
  NUM_HUNDRED:          UOMNum = 1e2;
  NUM_THOUSAND:         UOMNum = 1e3;
  NUM_MILLION:          UOMNum = 1e6;
  NUM_BILLION:          UOMNum = 1e9;
  NUM_TRILLION:         UOMNum = 1e12;
  NUM_QUADRILLION:      UOMNum = 1e15;
  NUM_QUINTILLION:      UOMNum = 1e18;
  NUM_SEXTILLION:       UOMNum = 1e21;
  NUM_SEPTILLION:       UOMNum = 1e24;
  NUM_OCTILLION:        UOMNum = 1e27;
  NUM_NONILLION:        UOMNum = 1e30;
  NUM_DECILLION:        UOMNum = 1e33;
  NUM_UNDECILLION:      UOMNum = 1e36;
  NUM_DUODECILLION:     UOMNum = 1e39;
  NUM_TREDECILLION:     UOMNum = 1e42;
  NUM_QUATTORDECILLION: UOMNum = 1e45;
  NUM_QUINDECILLION:    UOMNum = 1e48;
  NUM_SEXDECILLION:     UOMNum = 1e51;
  NUM_SEPTENDECILLION:  UOMNum = 1e54;
  NUM_OCTODECILLION:    UOMNum = 1e57;
  NUM_NOVEMDECILLION:   UOMNum = 1e60;
  NUM_VIGINTILLION:     UOMNum = 1e63;

  //TODO: Fill in more...

  NUM_GOOGOL:           UOMNum = 1e100;

  NUM_CENTILLION:       UOMNum = 1e303;

  NUM_GOOGOLPLEX:       String = '10^NUM_GOOGOL';
  NUM_GOOGOLPLEXPLEX:   String = '10^NUM_GOOGOLPLEX';


  //Common Metric conversion factors
  METRIC_YOCTO: UOMNum = 1e-24;
  METRIC_ZEPTO: UOMNum = 1e-21;
  METRIC_ATTO:  UOMNum = 1e-18;
  METRIC_FEMTO: UOMNum = 1e-15;
  METRIC_PICO:  UOMNum = 1e-12;
  METRIC_NANO:  UOMNum = 1e-09;
  METRIC_MICRO: UOMNum = 1e-06;
  METRIC_MILLI: UOMNum = 1e-03;
  METRIC_CENTI: UOMNum = 1e-02;
  METRIC_DECI:  UOMNum = 1e-01;
  METRIC_BASE:  UOMNum = 1e+00;
  METRIC_DECA:  UOMNum = 1e+01;
  METRIC_HECTO: UOMNum = 1e+02;
  METRIC_KILO:  UOMNum = 1e+03;
  METRIC_MEGA:  UOMNum = 1e+06;
  METRIC_GIGA:  UOMNum = 1e+09;
  METRIC_TERA:  UOMNum = 1e+12;
  METRIC_PETA:  UOMNum = 1e+15;
  METRIC_EXA:   UOMNum = 1e+18;
  METRIC_ZETA:  UOMNum = 1e+21;
  METRIC_YOTTA: UOMNum = 1e+24;



type
  TUOMMetricUtils = class;
  TUOM = class;
  TUOMUtils = class;



  /// <summary>
  /// Base exception type for all exceptions raised from UOM library.
  /// </summary>
  EUOMException = Exception;
  /// <summary>
  /// Exception indicating a specified UOM is invalid.
  /// </summary>
  EUOMInvalidUnitException = EUOMException;
  /// <summary>
  /// Exception indicating a specified value is outside of its possible range.
  /// </summary>
  EUOMOutOfRangeException = EUOMException;
  /// <summary>
  /// Exception indicating a value cannot be duplicated.
  /// </summary>
  EUOMDuplicateException = EUOMException;
  /// <summary>
  /// Exception indicating string expression evaluation failed.
  /// </summary>
  EUOMEvalException = EUOMException;
  /// <summary>
  /// Exception indicating failure to load a UOM file.
  /// </summary>
  EUOMFileException = EUOMException;



  TUOMDictNum = (dnHundred, dnThousand, dnMillion, dnBillion, dnTrillion,
    dnQuadrillion, dnQuintillion, dnSextillion, dnSeptillion, dnOctillion,
    cnNonillion, dnDecillion, dnUndecillion, dnDuodecillion, dnTredecillion,
    dnQuatrodecillion, dnQuindecillion, dnSexdecillion, dnSeptendecillion,
    dnOctodecillion, dnNovemdecillion, dnVigintillion,

    dnGoogol, dnCentillion, dnGoogolplex, dnGoogolplexplex);

  TUOMDictNumUtils = class
  public
    class function Name(const Num: TUOMDictNum): String;
    class function Factor(const Num: TUOMDictNum): Extended;
  end;



  /// <summary>
  /// Enum representing a specific Metric system size.
  /// </summary>
  TUOMMetricUnit = (msYocto, msZepto, msAtto,
    msFemto, msPico, msNano, msMicro, msMilli, msCenti, msDeci,
    msBase, msDeca, msHecto, msKilo, msMega, msGiga, msTera, msPeta,
    msExa, msZeta, msYotta);

  /// <summary>
  /// Set of `TMetricUnit` enum.
  /// </summary>
  TUOMMetricUnits = set of TUOMMetricUnit;

  /// <summary>
  /// Class providing several helpful tools to manage UOMs of the Metric system.
  /// </summary>
  TUOMMetricUtils = class
  public
    /// <summary>
    /// Returns the general name of a given metric unit.
    /// </summary>
    class function MetricName(const U: TUOMMetricUnit): String; static;
    /// <summary>
    /// Returns the general suffix of a given metric unit.
    /// </summary>
    class function MetricSuffix(const U: TUOMMetricUnit): String; static;
    /// <summary>
    /// Returns the base conversion factor of a given metric unit.
    /// </summary>
    class function MetricFactor(const U: TUOMMetricUnit): UOMNum; static;
    /// <summary>
    /// Automatically generates and registers UOMs for each specified metric unit.
    /// </summary>
    class procedure ProduceUOMs(const Category: String; const Name: String;
      const Suffix: String; const Units: TUOMMetricUnits; const Base: String = '';
      const Systems: String = 'Metric'; const OffsetBase: String = '');
  end;



  /// <summary>
  /// (NOT READY)
  /// Abstract record to encapsulate a single possible value attached to a specific UOM category
  /// </summary>
  TUOMValue = record
  private
    FUOM: String;
    FBaseValue: UOMNum;
    procedure SetUOM(const Value: String);
    procedure SetBaseValue(const Value: UOMNum);
    function GetConvertedValue: UOMNum;
    procedure SetConvertedValue(const Value: UOMNum);
    function GetUomObj: TUOM;
  public
    class operator Implicit(const Value: TUOMValue): UOMNum;
    class operator Implicit(const Value: UOMNum): TUOMValue;
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
    property BaseValue: UOMNum read FBaseValue write SetBaseValue;
    /// <summary>
    /// Returns the value converted FROM the base TO the specified unit.
    /// </summary>
    property ConvertedValue: UOMNum read GetConvertedValue write SetConvertedValue;
  end;



  /// <summary>
  /// (NOT READY)
  /// Specifies the number of values in use in a TUOMCombinedValue record.
  /// </summary>
  TUOMCombinedValueCount = (cvcTwo, cvcThree);

  /// <summary>
  /// (NOT READY)
  /// Specifies the type of values in use in a TUOMCombinedValue record.
  /// </summary>
  TUOMCombinedValueType = (cvtDivide, cvtMultiply, cvtAdd);

  /// <summary>
  /// (NOT READY)
  /// A record that can be reused for things like Speed or Frequency,
  ///   where two different UOM categories are compared with each other.
  ///   For example, Miles per Hour, Meters per Second, Bananas per Cubic Yard...
  /// Task #45
  /// </summary>
  TUOMCombinedValue = record
  private
    FCount: TUOMCombinedValueCount;
    FValue1: TUOMValue;
    FValue2: TUOMValue;
    FValue3: TUOMValue;
    procedure SetCount(const Value: TUOMCombinedValueCount);
    procedure SetValue1(const Value: TUOMValue);
    procedure SetValue2(const Value: TUOMValue);
    procedure SetValue3(const Value: TUOMValue);
  public
    procedure Invalidate;
    property Count: TUOMCombinedValueCount read FCount write SetCount;
    property Value1: TUOMValue read FValue1 write SetValue1;
    property Value2: TUOMValue read FValue2 write SetValue2;
    property Value3: TUOMValue read FValue3 write SetValue3;
  end;



  /// <summary>
  /// Defines how the UOM shall be registered and how conversion shall be performed.
  /// </summary>
  TUOMType = (uomFactor, uomFormula, uomMetric, uomCombo);

  /// <summary>
  /// Base object for each possible UOM unit.
  /// </summary>
  TUOM = class(TObject)
  private
    FOwner: TUOM;
    FUOMType: TUOMType;
    FCategory: String;
    FSystems: TStringList;
    FNameSingular: String;
    FNamePlural: String;
    FSuffix: String;
    FConvertToBaseFormula: String;
    FConvertFromBaseFormula: String;
    FFactor: UOMNum;
    FAliases: TStringList;
    function GetSystems: TStrings;
    procedure SystemsChanged(Sender: TObject);
    procedure AliasesChanged(Sender: TObject);
    procedure SetNamePlural(const Value: String);
    procedure SetNameSingular(const Value: String);
    procedure SetSuffix(const Value: String);
    procedure SetSystems(const Value: TStrings);
    procedure SetCategory(const Value: String);
    procedure SetConvertFromBaseFormula(const Value: String);
    procedure SetConvertToBaseFormula(const Value: String);
    procedure SetFactor(const Value: UOMNum);
    procedure SetUOMType(const Value: TUOMType);
    function GetAlias(const Index: Integer): String;
  public
    constructor Create(const AOwner: TUOM);
    destructor Destroy; override;
    /// <summary>
    /// Assigns this UOM as the base UOM of its specified Category.
    /// </summary>
    function SetAsBase: TUOM;
    /// <summary>
    /// Returns a value converted FROM the base unit.
    /// </summary>
    function ConvertFromBase(const Value: UOMNum): UOMNum;
    /// <summary>
    /// Returns a value converted TO the base unit.
    /// </summary>
    function ConvertToBase(const Value: UOMNum): UOMNum;
  public

    property Owner: TUOM read FOwner;

    /// <summary>
    /// A string containing a mathematical expression to convert the `Value` FROM the base UOM.
    /// </summary>
    property ConvertFromBaseFormula: String read FConvertFromBaseFormula write SetConvertFromBaseFormula;
    /// <summary>
    /// A string containing a mathematical expression to convert the `Value` TO the base UOM.
    /// </summary>
    property ConvertToBaseFormula: String read FConvertToBaseFormula write SetConvertToBaseFormula;
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
    /// Suffix showing after a given UOM value.
    /// Also a unique CASE-SENSITIVE identifer - cannot create duplicates.
    /// </summary>
    property Suffix: String read FSuffix write SetSuffix;
    /// <summary>
    /// Conversion factor for simple UOM conversions.
    /// </summary>
    property Factor: UOMNum read FFactor write SetFactor;
    /// <summary>
    /// Type of UOM, defining core behavior.
    /// </summary>
    property UOMType: TUOMType read FUOMType write SetUOMType;
    /// <summary>
    /// Adds a string to be used as a unique identifer for UOM.
    /// </summary>
    function AddAlias(const S: String): TUOM;
    /// <summary>
    /// Returns the total number of Aliases associated with this UOM.
    /// </summary>
    function AliasCount: Integer;
    /// <summary>
    /// Returns a string representing a specific Alias by its list inded.
    /// </summary>
    property Aliases[const Index: Integer]: String read GetAlias;
    /// <summary>
    /// Returns a string representing all Aliases of UOM, comma separated.
    /// </summary>
    function AllAliases: String;
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
    class var FEvalInst: TUOMEvaluator;
  public
    class constructor Create;
    class destructor Destroy;

    class function Evaluate(const Value: UOMNum; const Expr: String): UOMNum;
    class function Calculate(const Expr: String): String;

    /// <summary>
    /// (NOT READY) Splits a given UOM String into respective Number and Suffix values.
    /// </summary>
    class procedure ParseSuffix(AValue: String; var ANumber: UOMNum;
      var ASuffix: String); static;
    /// <summary>
    /// (NOT READY) Parses a given UOM String into `TUOMValue` record type.
    /// </summary>
    class function StrToUOMValue(const Str: String): TUOMValue;
    /// <summary>
    /// A change has been made which requires Systems cache to be refreshed.
    /// </summary>
    class procedure InvalidateSystems; virtual;
    /// <summary>
    /// A change has been made which requires Categories cache to be refreshed.
    /// </summary>
    class procedure InvalidateCategories; virtual;
    /// <summary>
    /// A change has been made which requires all cache to be refreshed.
    /// </summary>
    class procedure InvalidateUOMs; virtual;
    /// <summary>
    /// Returns a TUOM object based on a given list inded of the master UOM list.
    /// </summary>
    class function GetUOMByIndex(const Index: Integer): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's unique SINGULAR name.
    /// </summary>
    class function GetUOMByName(const Name: String): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's unique suffix (case sensitive).
    /// </summary>
    class function GetUOMBySuffix(const Suffix: String): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's Alias.
    /// </summary>
    class function GetUOMByAlias(const Value: String): TUOM; static;
    /// <summary>
    /// Returns a TUOM object based on a given UOM's unique identifier of any kind.
    /// </summary>
    class function FindUOM(const Value: String): TUOM; static;
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
    /// Registers a new TUOM object into the UOM system based on a variety of parameters.
    /// NOTE: Validation will be done to ensure unique (case sensitive) UOM names,
    /// as well as unique suffixes.
    /// </summary>
    class function RegisterUOM(const ACategory, ANameSingular, ANamePlural,
      ASuffix, ASystems: String;
      const AFromBase: String; const AToBase: String;
      const AFactor: UOMNum = 0;
      const AOwner: TUOM = nil): TUOM; static;
    /// <summary>
    /// Registers a new SIMPLE TUOM object into the UOM system based on a variety of parameters.
    /// </summary>
    class function RegisterSimpleUOM(const ACategory, ANameSingular,
      ANamePlural, ASuffix, ASystems: String; const ABaseFactor: UOMNum;
      const AOwner: TUOM = nil): TUOM; static;
    /// <summary>
    /// Registers the BASE UOM for a given UOM Category. For example,
    /// Meters for Distance, Grams for Mass, Celsius for Temperature...
    /// </summary>
    class procedure RegisterBaseUOM(const ACategory: String; const AUnit: TUOM); static;
    /// <summary>
    /// Unregisters the specified UOM from the system.
    /// </summary>
    class procedure UnregisterUOM(const UOM: TUOM);
    /// <summary>
    /// Converts a given `Value` from a given `FromUOM` to a given `ToUOM` as a `UOMNum`.
    /// </summary>
    class function Convert(const Value: UOMNum; const FromUOM, ToUOM: String): UOMNum; static;
    /// <summary>
    /// Accesses any registered TUOM object by its given master list index.
    /// </summary>
    class property UOMs[const Index: Integer]: TUOM read GetUOMByIndex; default;
  end;

function UOMValue(const Value: UOMNum; const UOM: String): TUOMValue;

////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////

function UOMValue(const Value: UOMNum; const UOM: String): TUOMValue;
begin
  Result.FUOM:= UOM;
  Result.ConvertedValue:= Value;
end;

{ TUOM }

constructor TUOM.Create(const AOwner: TUOM);
begin
  FOwner:= AOwner;
  FAliases:= TStringList.Create;
  FAliases.Delimiter:= ',';
  FAliases.StrictDelimiter:= True;
  FAliases.OnChange:= AliasesChanged;
  FSystems:= TStringList.Create;
  FSystems.Delimiter:= ',';
  FSystems.StrictDelimiter:= True;
  FSystems.OnChange:= SystemsChanged;
end;

destructor TUOM.Destroy;
begin
  FreeAndNil(FSystems);
  FreeAndNil(FAliases);
end;

function TUOM.AddAlias(const S: String): TUOM;
begin
  //TODO: Prevent duplicate...
  FAliases.Add(S);
  Result:= Self;
end;

function TUOM.AliasCount: Integer;
begin
  Result:= FAliases.Count;
end;

procedure TUOM.AliasesChanged(Sender: TObject);
begin
  TUOMUtils.InvalidateUOMs;
end;

function TUOM.AllAliases: String;
begin
  Result:= FAliases.DelimitedText;
end;

function TUOM.ConvertFromBase(const Value: UOMNum): UOMNum;
var
  B: TUOM;
begin
  B:= TUOMUtils.GetBaseUOM(FCategory);
  Result:= TUOMUtils.Convert(Value, B.FNameSingular, FNameSingular);
end;

function TUOM.ConvertToBase(const Value: UOMNum): UOMNum;
var
  B: TUOM;
begin
  B:= TUOMUtils.GetBaseUOM(FCategory);
  Result:= TUOMUtils.Convert(Value, FNameSingular, B.FNameSingular);
end;

function TUOM.SetAsBase: TUOM;
begin
  TUOMUtils.RegisterBaseUOM(FCategory, Self);
  Result:= Self;
end;

procedure TUOM.SetConvertFromBaseFormula(const Value: String);
begin
  FConvertFromBaseFormula := Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetConvertToBaseFormula(const Value: String);
begin
  FConvertToBaseFormula := Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetFactor(const Value: UOMNum);
begin
  FFactor := Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

function TUOM.GetAlias(const Index: Integer): String;
begin
  Result:= FAliases[Index];
end;

function TUOM.GetSystems: TStrings;
begin
  Result:= TStrings(FSystems);
end;

procedure TUOM.SetNamePlural(const Value: String);
begin
  FNamePlural:= Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetNameSingular(const Value: String);
begin
  FNameSingular:= Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetSuffix(const Value: String);
begin
  FSuffix:= Value;
  //TODO: Validate...
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetSystems(const Value: TStrings);
begin
  FSystems.Assign(Value);
  TUOMUtils.InvalidateSystems;
end;

procedure TUOM.SetUOMType(const Value: TUOMType);
begin
  FUOMType := Value;
  TUOMUtils.InvalidateUOMs;
end;

procedure TUOM.SetCategory(const Value: String);
begin
  FCategory:= Value;
  TUOMUtils.InvalidateCategories;
end;

procedure TUOM.SystemsChanged(Sender: TObject);
begin
  TUOMUtils.InvalidateUOMs;
end;

{ TUOMUtils }

class constructor TUOMUtils.Create;
begin
  FUOMs:= TObjectList<TUOM>.Create(True);
  FBaseUOMs:= TDictionary<String, TUOM>.Create;
  FSystems:= TStringList.Create;
  FCategories:= TStringList.Create;
  FEvalInst:= TUOMEvaluator.Create;
end;

class destructor TUOMUtils.Destroy;
begin
  FreeAndNil(FEvalInst);
  FreeAndNil(FCategories);
  FreeAndNil(FSystems);
  FreeAndNil(FBaseUOMs);
  FreeAndNil(FUOMs);
end;

class function TUOMUtils.Convert(const Value: UOMNum; const FromUOM,
  ToUOM: String): UOMNum;
var
  F, T: TUOM;
begin
  //MAIN CONVERSION FUNCTION - Dynamically calls relevant unit conversion methods.
  Result:= 0;
  F:= GetUOMByName(FromUOM);
  if not Assigned(F) then begin
    raise EUOMInvalidUnitException.Create('Conversion from unit "'+F.NameSingular+'" not found.');
  end;
  T:= GetUOMByName(ToUOM);
  if not Assigned(T) then begin
    raise EUOMInvalidUnitException.Create('Conversion to unit "'+F.NameSingular+'" not found.');
  end;

  if F.FUOMType = uomFormula then begin
    if Trim(F.ConvertFromBaseFormula) = '' then begin
      raise EUOMException.Create('Conversion from formula is blank.');
    end;
  end else begin
    if F.Factor = 0 then begin
      raise EUOMException.Create('Conversion from factor is 0.');
    end;
  end;

  if T.FUOMType = uomFormula then begin
    if Trim(T.ConvertToBaseFormula) = '' then begin
      raise EUOMException.Create('Conversion to formula is blank.');
    end;
  end else begin
    if T.Factor = 0 then begin
      raise EUOMException.Create('Conversion to factor is 0.');
    end;
  end;


  try

    //TODO: Change to respect new enum FUOMType: TUOMType...

    //From input value to base...
    case F.UOMType of
      uomFactor, uomMetric: begin
        Result:= Value * F.Factor;
      end;
      uomFormula: begin
        Result:= Evaluate(Value, F.ConvertToBaseFormula);
      end;
      uomCombo: begin
        //TODO
        Result:= Evaluate(Value, F.ConvertToBaseFormula);
      end;
    end;

    //From base to output value...
    case T.UOMType of
      uomFactor, uomMetric: begin
        Result:= Result / T.Factor;
      end;
      uomFormula: begin
        Result:= Evaluate(Result, T.ConvertFromBaseFormula);
      end;
      uomCombo: begin
        //TODO
        Result:= Evaluate(Result, T.ConvertFromBaseFormula);
      end;
    end;


    //From input value to base...
    //Result:= Evaluate(Value, F.ConvertToBaseFormula);
    //From base to output value...
    //Result:= Evaluate(Result, T.ConvertFromBaseFormula);
  except
    on E: Exception do begin
      raise EUOMException.Create('Convert function failed: '+E.Message);
    end;
  end;
end;

class function TUOMUtils.Evaluate(const Value: UOMNum; const Expr: String): UOMNum;

begin
  Result:= FEvalInst.Evaluate(Value, Expr);
end;

class procedure TUOMUtils.InvalidateCategories;
begin
  ListCategories(FCategories);
end;

class procedure TUOMUtils.InvalidateSystems;
begin
  ListSystems(FSystems);
end;

class procedure TUOMUtils.InvalidateUOMs;
begin
  InvalidateCategories;
  InvalidateSystems;
end;

class procedure TUOMUtils.ParseSuffix(AValue: String; var ANumber: UOMNum; var ASuffix: String);
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

class function TUOMUtils.GetBaseUOM(const Category: String): TUOM;
begin
  Result:= nil;
  if FBaseUOMs.ContainsKey(Category) then
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
    if (SameText(Trim(Name), Trim(FUOMs[X].FNameSingular))) or
       (SameText(Trim(Name), Trim(FUOMs[X].FNamePlural))) then
    begin
      Result:= FUOMs[X];
      Break;
    end;
  end;
end;

class function TUOMUtils.GetUOMByAlias(const Value: String): TUOM;
var
  X, Y: Integer;
  U: TUOM;
begin
  Result:= nil;
  for X := 0 to FUOMs.Count-1 do begin
    U:= FUOMs[X];
    for Y := 0 to U.AliasCount-1 do begin
      if Trim(U.Aliases[Y]) = Trim(Value) then begin
        Result:= U;
        Break;
      end;
    end;
    if Result <> nil then Break;
  end;
end;

class function TUOMUtils.FindUOM(const Value: String): TUOM;
begin
  Result:= GetUOMByName(Value);
  if Result = nil then
    Result:= GetUOMBySuffix(Value);
  if Result = nil then
    Result:= GetUOMByAlias(Value);
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
var
  T: TUOM;
begin
  T:= TUOMUtils.GetBaseUOM(ACategory);
  if T = nil then
    FBaseUOMs.Add(ACategory, AUnit)
  else
    FBaseUOMs[ACategory]:= AUnit;
  InvalidateUOMs;
  InvalidateCategories;
end;

class function TUOMUtils.RegisterSimpleUOM(const ACategory, ANameSingular,
  ANamePlural, ASuffix, ASystems: String;
  const ABaseFactor: UOMNum; const AOwner: TUOM = nil): TUOM;
var
  F: String;
begin
  F:= FormatFloat(NumInternalFormat, ABaseFactor);
  Result:= RegisterUOM(ACategory, ANameSingular, ANamePlural, ASuffix, ASystems,
    'Value / '+F, 'Value * '+F, ABaseFactor, AOwner);
  //TODO: Change to record factor INSTEAD of formula...
  Result.FUOMType:= TUOMType.uomFactor;
end;

class function TUOMUtils.RegisterUOM(const ACategory, ANameSingular,
  ANamePlural, ASuffix, ASystems: String;
  const AFromBase, AToBase: String; const AFactor: UOMNum = 0; const AOwner: TUOM = nil): TUOM;
begin

  if Trim(ANameSingular) = '' then begin
    raise EUOMInvalidUnitException.Create('Cannot register blank unit singular name.');
  end;

  if Trim(ANamePlural) = '' then begin
    raise EUOMInvalidUnitException.Create('Cannot register blank unit plural name.');
  end;

  if Trim(ASuffix) = '' then begin
    raise EUOMInvalidUnitException.Create('Cannot register blank unit suffix.');
  end;

  if Assigned(GetUOMByName(ANameSingular)) or
    Assigned(GetUOMByName(ANamePlural)) then
  begin
    raise EUOMDuplicateException.Create('Cannot register duplicate unit name '+ANameSingular);
  end;

  if Assigned(GetUOMBySuffix(ASuffix)) then begin
    raise EUOMDuplicateException.Create('Cannot register duplicate unit suffix '+ASuffix);
  end;



  Result:= TUOM.Create(AOwner);
  try
    Result.FUOMType:= TUOMType.uomFormula;
    Result.FCategory:= ACategory;
    Result.FNameSingular:= ANameSingular;
    Result.FNamePlural:= ANamePlural;
    Result.FSuffix:= ASuffix;
    Result.FSystems.Delimiter:= ',';
    Result.FSystems.StrictDelimiter:= True;
    Result.FSystems.DelimitedText:= ASystems;
    Result.FConvertFromBaseFormula:= AFromBase;
    Result.FConvertToBaseFormula:= AToBase;
    Result.FFactor:= AFactor;
  finally
    FUOMs.Add(Result);
  end;
  InvalidateUOMs;
end;

class function TUOMUtils.StrToUOMValue(const Str: String): TUOMValue;
var
  Val: UOMNum;
  Txt: String;
  U: TUOM;
begin
  //Parse Str and return a corresponding TUOMValue record...
  Result.FBaseValue:= 0;
  Result.FUOM:= '';
  TUOMUtils.ParseSuffix(Str, Val, Txt);
  U:= TUOMUtils.FindUOM(Txt);
  if Assigned(U) then begin
    Result.FUOM:= U.NameSingular;
    Result.ConvertedValue:= Val;
  end;
end;

class function TUOMUtils.SystemCount: Integer;
begin
  //Invalidate;
  Result:= FSystems.Count;
end;

class procedure TUOMUtils.UnregisterUOM(const UOM: TUOM);
var
  I: Integer;
begin
  //TODO: If this is metric, also unregister all child UOMs
  I:= FUOMs.IndexOf(UOM);
  if I > -1 then begin
    FUOMs.Delete(I);
    InvalidateUOMs;
  end;
end;

class function TUOMUtils.UOMCount: Integer;
begin
  Result:= FUOMs.Count;
end;

class function TUOMUtils.Calculate(const Expr: String): String;
begin
  Result:= FEvalInst.Calculate(Expr);
end;

class function TUOMUtils.CategoryCount: Integer;
begin
  //Invalidate;
  Result:= FCategories.Count;
end;

{ TUOMValue }

class operator TUOMValue.Implicit(const Value: TUOMValue): UOMNum;
var
  U: TUOM;
begin
  //Implicitly return the CONVERTED value...
  U:= TUOMUtils.GetUOMByName(Value.FUOM);
  if U = nil then
    raise EUOMInvalidUnitException.Create('UOM "'+Value.FUOM+'" not found!');
  Result:= U.ConvertFromBase(Value.FBaseValue);
end;

class operator TUOMValue.Implicit(const Value: UOMNum): TUOMValue;
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
  V: UOMNum;
begin
  //Implicitly return the CONVERTED value formatted as string...
  U:= TUOMUtils.GetUOMByName(Value.FUOM);
  if U = nil then
    raise EUOMInvalidUnitException.Create('UOM "'+Value.FUOM+'" not found!');
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

function TUOMValue.GetConvertedValue: UOMNum;
var
  U: TUOM;
begin
  U:= GetUomObj;
  if not Assigned(U) then
    raise EUOMInvalidUnitException.Create('Failed to get converted value: Unit-of-Measure "'+FUOM+'" not found.');
  Result:= U.ConvertFromBase(FBaseValue);
end;

function TUOMValue.GetUomObj: TUOM;
begin
  Result:= TUOMUtils.GetUOMByName(FUOM);
end;

procedure TUOMValue.SetConvertedValue(const Value: UOMNum);
var
  U: TUOM;
begin
  U:= GetUomObj;
  if not Assigned(U) then
    raise EUOMInvalidUnitException.Create('Failed to set converted value: Unit-of-Measure "'+FUOM+'" not found.');
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

procedure TUOMValue.SetBaseValue(const Value: UOMNum);
begin
  FBaseValue := Value;
end;

{ TUOMMetricUtils }

class function TUOMMetricUtils.MetricFactor(const U: TUOMMetricUnit): UOMNum;
begin
  case U of
    msYocto:  Result:= METRIC_YOCTO;
    msZepto:  Result:= METRIC_ZEPTO;
    msAtto:   Result:= METRIC_ATTO;
    msFemto:  Result:= METRIC_FEMTO;
    msPico:   Result:= METRIC_PICO;
    msNano:   Result:= METRIC_NANO;
    msMicro:  Result:= METRIC_MICRO;
    msMilli:  Result:= METRIC_MILLI;
    msCenti:  Result:= METRIC_CENTI;
    msDeci:   Result:= METRIC_DECI;
    msBase:   Result:= METRIC_BASE;
    msDeca:   Result:= METRIC_DECA;
    msHecto:  Result:= METRIC_HECTO;
    msKilo:   Result:= METRIC_KILO;
    msMega:   Result:= METRIC_MEGA;
    msGiga:   Result:= METRIC_GIGA;
    msTera:   Result:= METRIC_TERA;
    msPeta:   Result:= METRIC_PETA;
    msExa:    Result:= METRIC_EXA;
    msZeta:   Result:= METRIC_ZETA;
    msYotta:  Result:= METRIC_YOTTA;
    else      Result:= METRIC_BASE;
  end;
end;

class function TUOMMetricUtils.MetricName(
  const U: TUOMMetricUnit): String;
begin
  //https://www.techtarget.com/searchstorage/definition/Kilo-mega-giga-tera-peta-and-all-that
  case U of
    msYocto:  Result:= 'Yocto';
    msZepto:  Result:= 'Zepto';
    msAtto:   Result:= 'Atto';
    msFemto:  Result:= 'Femto';
    msPico:   Result:= 'Pico';
    msNano:   Result:= 'Nano';
    msMicro:  Result:= 'Micro';
    msMilli:  Result:= 'Milli';
    msCenti:  Result:= 'Centi';
    msDeci:   Result:= 'Deci';
    msBase:   Result:= '';
    msDeca:   Result:= 'Deca';
    msHecto:  Result:= 'Hecto';
    msKilo:   Result:= 'Kilo';
    msMega:   Result:= 'Mega';
    msGiga:   Result:= 'Giga';
    msTera:   Result:= 'Tera';
    msPeta:   Result:= 'Peta';
    msExa:    Result:= 'Exa';
    msZeta:   Result:= 'Zeta';
    msYotta:  Result:= 'Yotta';
    else      Result:= '';
  end;
end;

class function TUOMMetricUtils.MetricSuffix(const U: TUOMMetricUnit): String;
begin
  case U of
    msYocto:  Result:= 'y';
    msZepto:  Result:= 'z';
    msAtto:   Result:= 'a';
    msFemto:  Result:= 'f';
    msPico:   Result:= 'p';
    msNano:   Result:= 'n';
    msMicro:  Result:= 'μ';
    msMilli:  Result:= 'm';
    msCenti:  Result:= 'c';
    msDeci:   Result:= 'd';
    msBase:   Result:= '';
    msDeca:   Result:= 'da';
    msHecto:  Result:= 'h';
    msKilo:   Result:= 'k';
    msMega:   Result:= 'M';
    msGiga:   Result:= 'G';
    msTera:   Result:= 'T';
    msPeta:   Result:= 'P';
    msExa:    Result:= 'E';
    msZeta:   Result:= 'Z';
    msYotta:  Result:= 'Y';
    else      Result:= '';
  end;
end;

class procedure TUOMMetricUtils.ProduceUOMs(const Category, Name,
  Suffix: String; const Units: TUOMMetricUnits; const Base: String = '';
  const Systems: String = 'Metric'; const OffsetBase: String = '');
var
  U, BU: TUOM;
  MU: TUOMMetricUnit;
  MN, MS: String;
  MF: UOMNum;
  NS, NP, S: String;
  L: TStringList;
  X: Integer;
begin
  //Validate...
  if not (msBase in Units) then
    raise EUOMException.Create('Metric UOM must include base!');

  //Proceed...
  L:= TStringList.Create;
  try
    L.Delimiter:= ',';

    //Register base UOM...
    //Apply offset of specified...
    MF:= MetricFactor(msBase);
    if OffsetBase <> '' then begin
      MF:= TUOMUtils.Evaluate(MF, OffsetBase);
    end;
    BU:= TUOMUtils.RegisterSimpleUOM(Category, Name, Name+'s', Suffix, Systems, MF);
    BU.FUOMType:= TUOMType.uomMetric;

    for MU:= Low(TUOMMetricUnit) to High(TUOMMetricUnit) do begin
      if (MU in Units) and (MU <> msBase) then begin
        //Get base Metric info...
        MN:= Trim(MetricName(MU));
        MS:= Trim(MetricSuffix(MU));
        MF:= MetricFactor(MU);
        //Produce singular and plural names...
        NS:= MN + LowerCase(Name);
        NP:= MN + LowerCase(Name) + 's';
        //Capitalize first letter...
        NS[1]:= UpCase(NS[1]);
        NP[1]:= UpCase(NP[1]);
        //Produce suffix...
        S:= MS + Suffix;

        //Apply offset of specified...
        if OffsetBase <> '' then begin
          MF:= TUOMUtils.Evaluate(MF, OffsetBase);
        end;

        //Produce system name(s)...
        L.DelimitedText:= Systems;
        for X := 0 to L.Count-1 do begin
          if MF <= METRIC_MICRO then
            L[X]:= L[X] + ' (Tiny)'
          else if MF >= METRIC_MEGA then
            L[X]:= L[X] + ' (Huge)';
        end;

        //Register child UOM...
        U:= TUOMUtils.RegisterSimpleUOM(Category, NS, NP, S, L.DelimitedText, MF, BU);
        U.FUOMType:= TUOMType.uomMetric;
      end;
    end;

    //Register base UOM...
    if Base <> '' then begin
      U:= TUOMUtils.GetUOMByName(Base);
      if Assigned(U) then
        U.SetAsBase;
    end;
  finally
    L.Free;
  end;

end;

{ TUOMCombinedValue }

procedure TUOMCombinedValue.Invalidate;
begin

end;

procedure TUOMCombinedValue.SetCount(const Value: TUOMCombinedValueCount);
begin
  FCount := Value;
  Invalidate;
end;

procedure TUOMCombinedValue.SetValue1(const Value: TUOMValue);
begin
  FValue1 := Value;
  Invalidate;
end;

procedure TUOMCombinedValue.SetValue2(const Value: TUOMValue);
begin
  FValue2 := Value;
  Invalidate;
end;

procedure TUOMCombinedValue.SetValue3(const Value: TUOMValue);
begin
  FValue3 := Value;
  Invalidate;
end;

{ TUOMDictNumUtils }

class function TUOMDictNumUtils.Factor(const Num: TUOMDictNum): Extended;
begin
  Result:= 0;
  case Num of
    dnHundred:          ;
    dnThousand:         ;
    dnMillion:          ;
    dnBillion:          ;
    dnTrillion:         ;
    dnQuadrillion:      ;
    dnQuintillion:      ;
    dnSextillion:       ;
    dnSeptillion:       ;
    dnOctillion:        ;
    cnNonillion:        ;
    dnDecillion:        ;
    dnUndecillion:      ;
    dnDuodecillion:     ;
    dnTredecillion:     ;
    dnQuatrodecillion:  ;
    dnQuindecillion:    ;
    dnSexdecillion:     ;
    dnSeptendecillion:  ;
    dnOctodecillion:    ;
    dnNovemdecillion:   ;
    dnVigintillion:     ;
    dnGoogol:           ;
    dnCentillion:       ;
    dnGoogolplex:       ;
    dnGoogolplexplex:   ;
  end;
end;

class function TUOMDictNumUtils.Name(const Num: TUOMDictNum): String;
begin
  Result:= '';
  case Num of
    dnHundred:          Result:= 'Hundred';
    dnThousand:         Result:= 'Thousand';
    dnMillion:          Result:= 'Million';
    dnBillion:          Result:= 'Billion';
    dnTrillion:         Result:= 'Trillion';
    dnQuadrillion:      Result:= 'Quadrillion';
    dnQuintillion:      Result:= 'Quintillion';
    dnSextillion:       Result:= 'Sextillion';
    dnSeptillion:       Result:= 'Septillion';
    dnOctillion:        Result:= 'Octillion';
    cnNonillion:        Result:= 'Nonillion';
    dnDecillion:        Result:= 'Decillion';
    dnUndecillion:      Result:= 'Undecillion';
    dnDuodecillion:     Result:= 'Duodecillion';
    dnTredecillion:     Result:= 'Tredecillion';
    dnQuatrodecillion:  Result:= 'Quatrodecillion';
    dnQuindecillion:    Result:= 'Quindecillion';
    dnSexdecillion:     Result:= 'Sexdecillion';
    dnSeptendecillion:  Result:= 'Septendecillion';
    dnOctodecillion:    Result:= 'Octodecillion';
    dnNovemdecillion:   Result:= 'Novemdecillion';
    dnVigintillion:     Result:= 'Vigintillion';

    dnGoogol:           Result:= 'Googol';

    dnCentillion:       Result:= 'Centillion';

    dnGoogolplex:       Result:= 'Googoplex';
    dnGoogolplexplex:   Result:= 'Googoplexplex';
  end;
end;

initialization

end.
