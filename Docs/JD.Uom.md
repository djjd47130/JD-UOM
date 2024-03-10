# JD.Uom.pas

Central unit to access all possible unit of measure conversions.

### NOTE: This documentation is out of date, and will be updated soon.

## Constants

- `PartOfNumber` - Used when parsing siffix from strings.
- `NumFormat - Used when formatting values as strings externally.
- `NumInternalFormat` - Used when formatting values as strings internally.
- `METRIC_` group of constants - Base conversion factors for the various Metric units of measurement.

## Types

- `TMetricUnit` - Enum representing a specific Metric system size.
- `TMetricUnits` - Set of `TMetricUnit` enum.
- `TUOMCombinedValueCount` - Specifies the number of values in use in a `TUOMCombinedValue` record.

## `TUOMMetricUtils` - Class providing several helpful tools to manage UOMs of the Metric system.

- `class function MetricName(const U: TUOMMetricUnit): String; static;` - Returns the general name of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
- `class function MetricSuffix(const U: TUOMMetricUnit): String; static;` - Returns the general suffix of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
- `class function MetricFactor(const U: TUOMMetricUnit): Double; static;` - Returns the base conversion factor of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
- `class procedure ProduceUOMs(const Category: String; const Name: String; const Suffix: String; const Units: TUOMMetricUnits; const Base: String = '');` - Automatically generates and registers UOMs for each specified metric unit.
  - `const Category: String` - 
  - `const Name: String` - 
  - `const Suffix: String` - 
  - `const Units: TUOMMetricUnits` - 
  - `const Base: String = ''` - 

## `TUOMValue` - **(NOT READY)** - Abstract record to encapsulate a single possible value attached to a specific UOM category

- Includes class operators to implicitly cast and perform mathematic operations on UOM values.
- `property UOM: String read FUOM write SetUOM;` - Returns the specified Unit-of-Measurement associated with the value.
- `property BaseValue: Double read FBaseValue write SetBaseValue;` - Returns the raw base value, unconverted.
- `property ConvertedValue: Double read GetConvertedValue write SetConvertedValue;` - Returns the value converted FROM the base TO the specified unit.

## `TUOMCombinedValue` - **(NOT READY)** - A record that can be reused for things like Speed or Frequency, where two different UOM categories are compared with each other.

- Includes class operators to implicitly cast and perform mathematic operations on UOM values.

## `TUOM` - Base object for each possible UOM unit.

- `procedure Invalidate; virtual;` - A change has been made which requires parent TUOMUtils to refresh its cache.
- `function SetAsBase: TUOM;` - Assigns this UOM as the base UOM of its specified Category.
- `function ConvertFromBase(const Value: Double): Double;` - Returns a value converted FROM the base unit.
- `function ConvertToBase(const Value: Double): Double;` - Returns a value converted TO the base unit.
- `property ConvertFromBaseFormula: String read FConvertFromBaseFormula write SetConvertFromBaseFormula;` - A string containing a mathematical expression to convert the `Value` FROM the base UOM.
- `property ConvertToBaseFormula: String read FConvertToBaseFormula write SetConvertToBaseFormula;` - A string containing a mathematical expression to convert the `Value` TO the base UOM.
- `property Category: String read FCategory write SetCategory;` - The major group of UOMs (Distance, Area, Volume, Mass, Temperature, etc.)
- `property Systems: TStrings read GetSystems write SetSystems;` - The systematic group(s) of UOMs (Metric, Imperial, US Customary...)
- `property NameSingular: String read FNameSingular write SetNameSingular;` - The singular (value = 1) name of the UOM (Meter, Foot, Gram...). Also its unique identifier - cannot create duplicates.
- `property NamePlural: String read FNamePlural write SetNamePlural;` - The plural (value <> 1) name of the TUOM (Meters, Feet, Grams...).
- `property Suffix: String read FSuffix write SetSuffix;` - Suffix showing after a given UOM value. Also a unique CASE-SENSITIVE identifer - cannot create duplicates.

## `TUOMUtils` - Main class encapsulating entire UOM library capabilities.

- `class procedure ParseSuffix(AValue: String; var ANumber: Double; var ASuffix: String); static;` - (NOT READY) Splits a given UOM String into respective Number (Double) and Suffix (String) values.
- `class function StrToUOMValue(const Str: String): TUOMValue;` - (NOT READY) Parses a given UOM String into `TUOMValue` record type.
- `class procedure Invalidate; virtual;` - A change has been made which requires cache to be refreshed.
- `class function GetUOMByIndex(const Index: Integer): TUOM; static;` - Returns a `TUOM` object based on a given list inded of the master UOM list.
- `class function GetUOMByName(const Name: String): TUOM; static;` - Returns a `TUOM` object based on a given UOM's unique SINGULAR name.
- `class function GetUOMBySuffix(const Suffix: String): TUOM; static;` - Returns a `TUOM` object based on a given UOM's u ique suffix (case sensitive).
- `class function GetBaseUOM(const Category: String): TUOM; static;` - Returns a `TUOM` object of the given Category's base unit.
- `class function CategoryCount: Integer; static;` - Returns the number of UOM categories (Distance, Area, Temperature, Mass...).
- `class function SystemCount: Integer; static;` - Returns the number of UOM systems (Metric, Imperial, US Customary...).
- `class procedure ListCategories(AList: TStrings); static;` - Populates a given `TStrings` object with all possible Categories.
- `class procedure ListSystems(AList: TStrings); static;` - Populates a given `TStrings` object with all possible Systems.
- `class procedure ListUOMs(AList: TStrings; const ACategory: String = ''; const ASystems: String = ''); static;` - Populates a given TStrings object with all possible UOMs matching the given filter parameters.
- `class function UOMCount: Integer; static;` - Returns the total number of specific UOMs registered.
- `class function RegisterUOM(const ACategory, ANameSingular, ANamePlural, ASuffix, ASystems: String; const AFromBase: String; const AToBase: String): TUOM; static;` - Registers a new TUOM object into the UOM system based on a variety of parameters. NOTE: Validation will be done to ensure unique (case sensitive) UOM names, as well as unique suffixes.
- `class function RegisterSimpleUOM(const ACategory, ANameSingular, ANamePlural, ASuffix, ASystems: String; const ABaseFactor: Double): TUOM; static;` - Registers a new SIMPLE TUOM object into the UOM system based on a variety of parameters.
- `class procedure RegisterBaseUOM(const ACategory: String; const AUnit: TUOM); static;` - Registers the BASE UOM for a given UOM Category. For example, Meters for Distance, Grams for Mass, Celsius for Temperature...
- `class function Convert(const Value: Double; const FromUOM, ToUOM: String): Double; static;` - Converts a given `Value` from a given `FromUOM` to a given `ToUOM` as a `Double`.
- `class property UOMs[const Index: Integer]: TUOM read GetUOMByIndex; default;` - Accesses any registered TUOM object by its given master list index.

