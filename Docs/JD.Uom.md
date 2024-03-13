# JD.Uom.pas

Central unit to access all possible unit of measure conversions.

## Constants

- `PartOfNumber` - Used when parsing siffix from strings.
- `NumFormat` - Used when formatting values as strings externally.
- `NumInternalFormat` - Used when formatting values as strings internally.
- `METRIC_` group of constants - Base conversion factors for the various Metric units of measurement.

## Types

- `TMetricUnit` - Enum representing a specific Metric system size.
- `TMetricUnits` - Set of `TMetricUnit` enum.
- `TUOMCombinedValueCount` - Specifies the number of values in use in a `TUOMCombinedValue` record.

## `TUOMMetricUtils`

Class providing several helpful tools to manage UOMs of the Metric system.

- `class function MetricName` - Returns the general name of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
  - *`Function Result: TUOM`* - The `TUOM` object related to given name, if found.
- `class function MetricSuffix` - Returns the general suffix of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
  - *`Function Result: TUOM`* - The `TUOM` object related to given name, if found.
- `class function MetricFactor` - Returns the base conversion factor of a given metric unit.
  - `const U: TUOMMetricUnit` - The desired metric unit to lookup.
  - *`Function Result: TUOM`* - The `TUOM` object related to given name, if found.
- `class procedure ProduceUOMs` - Automatically generates and registers UOMs for each specified metric unit.
  - `const Category: String` - The Category associated with the UOMs.
  - `const Name: String` - The base *singular* name of the metric UOMs, such as `Meter`, `Gram`, or `Hertz`.
  - `const Suffix: String` - The base suffix of the metric UOMs, such as `m`, `g`, or `Hz`.
  - `const Units: TUOMMetricUnits` - A set of `TUOMMetricUnit` enums to specify which metric UOMs should be registered.
  - `const Base: String = ''` - The base UOM for the given Category (Default is the default metric `Name` such as `Meter`, `Gram`, or `Hertz`).

## `TUOMValue`

**(NOT READY)** - Abstract record to encapsulate a single possible value attached to a specific UOM category

- Includes class operators to implicitly cast and perform mathematic operations on UOM values.
- `property UOM: String` - Returns the specified Unit-of-Measurement associated with the value.
- `property BaseValue: Double` - Returns the raw base value, unconverted.
- `property ConvertedValue: Double` - Returns the value converted FROM the base TO the specified unit.

## `TUOMCombinedValue`

**(NOT READY)** - A record that can be reused for things like Speed or Frequency, where two different UOM categories are compared with each other.

- Includes class operators to implicitly cast and perform mathematic operations on UOM values.

## `TUOM`

Base object for each possible UOM unit.

- `procedure Invalidate` - A change has been made which requires parent TUOMUtils to refresh its cache.
- `function SetAsBase` - Assigns this UOM as the base UOM of its specified Category.
  - *`Function Result: TUOM`* - Returns itself.
- `function ConvertFromBase` - Returns a value converted FROM the base unit.
  - `const Value: Double` - The value to be converted from the base unit.
  - *`Function Result: Double`* - The converted value.
- `function ConvertToBase` - Returns a value converted TO the base unit.
  - `const Value: Double` - The value to be converted to the base unit.
  - *`Function Result: Double`* - The converted value.
- `property ConvertFromBaseFormula: String` - A string containing a mathematical expression to convert the `Value` FROM the base UOM.
- `property ConvertToBaseFormula: String` - A string containing a mathematical expression to convert the `Value` TO the base UOM.
- `property Category: String` - The major group of UOMs (Distance, Area, Volume, Mass, Temperature, etc.)
- `property Systems: TStrings` - The systematic group(s) of UOMs (Metric, Imperial, US Customary...)
- `property NameSingular: String` - The singular (value = 1) name of the UOM (Meter, Foot, Gram...). Also its unique identifier - cannot create duplicates.
- `property NamePlural: String` - The plural (value <> 1) name of the TUOM (Meters, Feet, Grams...).
- `property Suffix: String` - Suffix showing after a given UOM value. Also a unique CASE-SENSITIVE identifer - cannot create duplicates.

## `TUOMUtils`

Main class encapsulating entire UOM library capabilities.

- `class procedure ParseSuffix` - (NOT READY) Splits a given UOM String into respective Number (Double) and Suffix (String) values.
  - `AValue: String` - The string value to be parsed.
  - `var ANumber: Double` - The resulting numeric value within the given value.
  - `var ASuffix: Double` - The resulting suffix value within the given value.
- `class function StrToUOMValue` - (NOT READY) Parses a given UOM String into `TUOMValue` record type.
  - `const Str: String` - The string value to be parsed and interpreted.
  - *`Function Result: TUOMValue`* - The resulting `TUOMValue` record containing value and related unit.
- `class procedure Invalidate` - A change has been made which requires cache to be refreshed.
- `class function GetUOMByIndex` - Returns a `TUOM` object based on a given list inded of the master UOM list.
  - `const Index: Integer` - The index value within the master UOM list.
  - *`Function Result: TUOM`* - The `TUOM` object, if found.
- `class function GetUOMByName` - Returns a `TUOM` object based on a given UOM's unique SINGULAR name (case sensitive).
  - `const Name: String` - The unique SINGULAR name to find in the master UOM list.
  - *`Function Result: TUOM`* - The `TUOM` object, if found.
- `class function GetUOMBySuffix` - Returns a `TUOM` object based on a given UOM's unique suffix (case sensitive).
  - `const Suffix: String` - The unique suffix to find in the master UOM list.
  - *`Function Result: TUOM`* - The `TUOM` object, if found.
- `class function GetBaseUOM` - Returns a `TUOM` object of the given Category's base unit.
  - `const Category: String` - The category in which to find the base.
  - *`Function Result: TUOM`* - The `TUOM` object, if found.
- `class function CategoryCount` - Returns the number of UOM categories (Distance, Area, Temperature, Mass...).
  - *`Function Result: Integer`* - The number of Categories registered.
- `class function SystemCount` - Returns the number of UOM systems (Metric, Imperial, US Customary...).
  - *`Function Result: Integer`* - The number of Systems registered.
- `class procedure ListCategories` - Populates a given `TStrings` object with all possible Categories.
  - `const AList: TStrings` - The `TStrings` object to be populated with Categories.
- `class procedure ListSystems` - Populates a given `TStrings` object with all possible Systems.
  - `const AList: TStrings` - The `TStrings` object to be populated with Systems.
- `class procedure ListUOMs` - Populates a given TStrings object with all possible UOMs matching the given filter parameters.
  - `AList: TStrings` - The `TStrings` object to be populated with UOMs.
  - `ACategory: String` - Filters by a specific Category (all by default).
  - `ASystems: String` - Filters by multiple specific Systems (all by default), comma-separated.
- `class function UOMCount` - Returns the total number of specific UOMs registered.
  - *`Function Result: Integer`* - The total number of UOMs registered.
- `class function RegisterUOM` - Registers a new `TUOM` object into the UOM system based on a variety of parameters. NOTE: Validation will be done to ensure unique (case sensitive) UOM names, as well as unique suffixes.
  - `const ACategory: String` - The main Category of the UOM (Distance, Mass, Volume...)
  - `const ANameSingular: String` - The unique *singular* name of the UOM (Milligram, Inch, Mile...)
  - `const ANamePlural: String` - The *plural* name of the UOM (Milligrams, Inches, Miles...)
  - `const ASuffix: String` - The unique suffix to appear at the end of UOM values (mg, in, mi...)
  - `const ASystems: String` -  Comma-separated string of multiple Systems (Metric,Imperial,US Customary...)
  - `const AFromBase: String` - The formula to convert *from* the base unit *to* this unit.
  - `const AToBase: String` - The formula to convert *from* this unit *to* the base unit.
  - *`Function Result: TUOM`* - The resulting `TUOM` object created from registering.
- `class function RegisterSimpleUOM` - Registers a new SIMPLE `TUOM` object into the UOM system based on a variety of parameters.
  - `const ACategory: String` - The main Category of the UOM (Distance, Mass, Volume...)
  - `const ANameSingular: String` - The unique *singular* name of the UOM (Milligram, Inch, Mile...)
  - `const ANamePlural: String` - The *plural* name of the UOM (Milligrams, Inches, Miles...)
  - `const ASuffix: String` - The unique suffix to appear at the end of UOM values (mg, in, mi...)
  - `const ASystems: String` -  Comma-separated string of multiple Systems (Metric,Imperial,US Customary...)
  - `const ABaseFactor: Double` - The conversion factor to the base unit.
  - *`Function Result: TUOM`* - The resulting `TUOM` object created from registering.
- `class procedure RegisterBaseUOM` - Registers the BASE UOM for a given UOM Category. For example, Meters for Distance, Grams for Mass, Celsius for Temperature...
  - `const ACategory: String` - The Category in which this base unit will be assigned.
  - `const AUnit: TUOM` - The specific `TUOM` object to be used as the Category's base unit.
- `class function Convert` - Converts a given `Value` from a given `FromUOM` to a given `ToUOM` as a `Double`.
  - `const Value: Double` - The given `Double` value to be converted.
  - `const FromUOM: String` - The unique UOM name of the given Value to be converted.
  - `const ToUOM: String` - The unique UOM name of the target converted result.
  - `Function Result: Double` - The resulting converted value.
- `class property UOMs` - Accesses any registered TUOM object by its given master list index.
  - `[const Index: Integer]` - The index value within the master UOM list.
