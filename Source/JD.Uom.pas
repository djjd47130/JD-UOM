﻿unit JD.Uom;

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

  What's In This Library:
  - Enumeration types for all possible metrics
    - Divided among different units for different UOMs
    - TUOMSystem: Different measurement systems (Metric, US Customary...)
    [OLD] - TUOM: Different types of measurement (Length, Area, Volume, Weight...)
    - TUOMLengthUnit, TUOMAreaUnit, TUOMVolumeUnit...: Different UOMs within a certain type
    - Every enum has a corresponding set type with a plural "s" at the end of the type
  - Classes with class methods for all UOM types
    - For exapmle, TUOMLengthUtils, or TUOMWeightUtils
    - Provides functions to convert between any given unit types
  - Classes to encapsulate each UOM and their units.
  - Conversion of metrics across different systems
  - Implicit record types to consume measurement data

  Measurement Systems (TUOMSystem / TUOMSystems):
  - Any (Not specific, applicable to any)
  - Metric (Meters, Liters, Kilograms...)
  - US Customary (Feet, Gallons, Pounds...)
  - Imperial (Similar to US Customary but UK based)

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
    - Newly discovered reference with many more units

*)


{$ENDREGION}

{ $DEFINE USE_JEDI}

uses
  System.Classes, System.SysUtils, System.Generics.Collections
  {$IFDEF USE_JEDI}
  , JclExprEval
  {$ENDIF}
  ;

const
  PartOfNumber = ['0'..'9', '.', ','];
  NumFormat = '#,###,###,###,##0.#############';

type
  TUOMBase = class;
  TUOMUnitBase = class;
  TUOMUtils = class;


  /// <summary>
  /// Overall categorization of specific units within any given UOM.
  /// </summary>
  TUOMSystem = (ustAny, ustMetric, ustUSCustomary, ustImperial, ustNatural);

  /// <summary>
  /// A set of `TUOMSystem`. Used by UOMs to identify which systems they're related to.
  /// </summary>
  TUOMSystems = set of TUOMSystem;

  /// <summary>
  /// Abstract refrence to a particular UOM.
  /// </summary>
  TUOMBaseClass = class of TUOMBase;

  /// <summary>
  /// Abstract refrence to a particular unit within a UOM.
  /// </summary>
  TUOMUnitClass = class of TUOMUnitBase;

  /// <summary>
  /// Base abstract class for all specific UOM measurement unit classes.
  /// </summary>
  TUOMUnitBase = class
  public
    class function UOM: TUOMBaseClass; virtual; abstract;
    class function UnitID: String; virtual; abstract;
    class function NamePlural: String; virtual;
    class function NameSingular: String; virtual; abstract;
    class function UnitDescription: String; virtual; abstract;
    class function Systems: TUOMSystems; virtual; abstract;
    class function Prefix: String; virtual; abstract;
    class function Suffix: String; virtual; abstract;
    class function ConvertToBase(const AValue: Double): Double; virtual; abstract;
    class function ConvertFromBase(const AValue: Double): Double; virtual; abstract;
  end;

  /// <summary>
  /// Base abstract class for all UOM utils classes.
  /// </summary>
  TUOMBase = class
  public
    class function UOMID: String; virtual; abstract;
    class function UOMName: String; virtual; abstract;
    class function UnitCount: Integer; virtual; abstract;
    class function GetUnit(const Index: Integer): TUOMUnitClass; virtual; abstract;
    class procedure UnitList(AList: TStrings; ASystem: TUOMSystem = ustAny); virtual;
    class function UnitName(const Index: Integer): String; virtual;
    class function UnitPrefix(const Index: Integer): String; virtual;
    class function UnitSuffix(const Index: Integer): String; virtual;
    class function BaseUnit: TUOMUnitClass; virtual; abstract;
  end;

  /// <summary>
  /// Base list of all possible unit utils classes.
  /// </summary>
  TUOMUtils = class
  private
    class var FItems: TList<TUOMBaseClass>;
  public
    class constructor Create;
    class destructor Destroy;
    class procedure RegisterUOM(AClass: TUOMBaseClass);
    class function Count: Integer;
    class function UOM(const Index: Integer): TUOMBaseClass;
    class function IndexOf(AClass: TUOMBaseClass): Integer; overload;
    class function IndexOf(AClass: String): Integer; overload;
    class function Convert(const AValue: Double;
      const AFromUnit, AToUnit: TUOMUnitClass): Double; overload;
  end;





//TODO: New concept of a lookup table with mathematical expressions.
//https://wiki.delphi-jedi.org/wiki/JCL_Help:JclExprEval.pas
//OR
//https://gobestcode.com/html/math_parser_for_delphi.html
//Meanwhile, this supports calling a common function reference to convert.

type
  TUOMLookupUnit = class;
  TUOMLookupTable = class;

  TConvertProc = Reference to function(const Value: Double): Double;

  /// <summary>
  /// NEW base object for each possible UOM unit.
  /// </summary>
  TUOMLookupUnit = class(TObject)
  private
    FUOM: String;
    FSystems: TStringList;
    FNameSingular: String;
    FNamePlural: String;
    FPrefix: String;
    FSuffix: String;
    {$IFDEF USE_JEDI}
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
    procedure SetUOM(const Value: String);
    {$IFDEF USE_JEDI}
    procedure SetConvertFromBaseFormula(const Value: TConvertProc);
    procedure SetConvertToBaseFormula(const Value: TConvertProc);
    {$ELSE}
    procedure SetConvertFromBaseProc(const Value: TConvertProc);
    procedure SetConvertToBaseProc(const Value: TConvertProc);
    {$ENDIF}
  public
    constructor Create; overload;
    constructor Create(const AUOM, ANameSingular, ANamePlural, APrefix, ASuffix,
      ASystems: String; const AFromBase: TConvertProc = nil; const AToBase: TConvertProc = nil); overload;
    destructor Destroy; override;
    procedure Invalidate; virtual;
    {$IFDEF USE_JEDI}
    property ConvertFromBaseFormula: String read FConvertFromBaseFormula write SetConvertFromBaseFormula;
    property ConvertToBaseFormula: String read FConvertToBaseFormula write SetConvertToBaseFormula;
    {$ELSE}
    property ConvertFromBaseProc: TConvertProc read FConvertFromBaseProc write SetConvertFromBaseProc;
    property ConvertToBaseProc: TConvertProc read FConvertToBaseProc write SetConvertToBaseProc;
    {$ENDIF}
    property UOM: String read FUOM write SetUOM;
    property Systems: TStrings read GetSystems write SetSystems;
    property NameSingular: String read FNameSingular write SetNameSingular;
    property NamePlural: String read FNamePlural write SetNamePlural;
    property Prefix: String read FPrefix write SetPrefix;
    property Suffix: String read FSuffix write SetSuffix;
    function ConvertFromBase(const AValue: Double): Double;
    function ConvertToBase(const AValue: Double): Double;
  end;

  TUOMLookupTable = class
  private
    class var FUnits: TObjectList<TUOMLookupUnit>;
    class var FBaseUnits: TDictionary<String, TUOMLookupUnit>;
    class var FSystems: TStringList;
    class var FUOMs: TStringList;
    {$IFDEF USE_JEDI}
    class var FEval: TEvaluator;
    {$ENDIF}
  public
    class constructor Create;
    class destructor Destroy;
    class procedure Invalidate; virtual;
    class function GetUnits(const Index: Integer): TUOMLookupUnit; static;
    class function GetUnitByName(const Name: String): TUOMLookupUnit; static;
    class function GetUnitByPrefix(const Prefix: String): TUOMLookupUnit; static;
    class function GetUnitBySuffix(const Suffix: String): TUOMLookupUnit; static;
    class function GetBaseUnit(const UOM: String): TUOMLookupUnit; static;
    class function UOMCount: Integer; static;
    class function SystemCount: Integer; static;
    class procedure ListUOMs(AList: TStrings); static;
    class procedure ListSystems(AList: TStrings); static;
    class procedure ListUnits(AList: TStrings; const AUOM: String = '';
      const ASystems: String = ''); static;
    class function UnitCount: Integer; static;
    class property Units[const Index: Integer]: TUOMLookupUnit read GetUnits;

    class procedure RegisterUnit(const AUnit: TUOMLookupUnit); static;
    class procedure RegisterBaseUnit(const AUOM: String; const AUnit: TUOMLookupUnit); static;
    class function Convert(const Value: Double; const FromUnit, ToUnit: String): Double; static;
  end;

  //TODO: New abstract record to encapsulate a single possible value attached to a specific UOM
  TUOMValue = record
  private
    FMeasureUnit: String;
    FValue: Double;
    procedure SetMeasureUnit(const Value: String);
    procedure SetValue(const Value: Double);
  public
    property MreasureUnit: String read FMeasureUnit write SetMeasureUnit;
    property Value: Double read FValue write SetValue;
  end;



{$REGION 'TUnitOfMeasurement'}

{ TUnitOfMeasurement }

type
  /// <summary>
  /// --- NOT READY ---
  /// Encapsulates full conversion process with easy to use properties.
  /// Can be used to quickly access all different possible units of measurement.
  /// Can be integrated into a component since it's a TPersistent
  /// TODO: Property editors in a design-time package...
  /// </summary>
  TUnitOfMeasurement = class(TPersistent)
  private
    FUOMIndex: Integer;
    FValue: Double;
    FUnitToIndex: Integer;
    FUnitFromUndex: Integer;
    procedure SetUnitFromUndex(const Value: Integer);
    procedure SetUnitToIndex(const Value: Integer);
    procedure SetUOMIndex(const Value: Integer);
    procedure SetValue(const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ListUoms(AList: TStrings);
    procedure ListUomUnits(AList: TStrings);
    procedure ListUomSystems(AList: TStrings);
  published
    property UOMIndex: Integer read FUOMIndex write SetUOMIndex;
    property Value: Double read FValue write SetValue;
    property UnitFromUndex: Integer read FUnitFromUndex write SetUnitFromUndex;
    property UnitToIndex: Integer read FUnitToIndex write SetUnitToIndex;
  end;

{$ENDREGION}


{$REGION 'TUOM'}

  /// <summary>
  /// --- NOT READY ---
  /// Component to encapsulate entire UOM infrastructure and allow access
  /// to UOM details and conversion methods.
  /// </summary>
  TUOM = class(TComponent)
  private
    FUOM: TUnitOfMeasurement;
    procedure SetUOM(const Value: TUnitOfMeasurement);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property UOM: TUnitOfMeasurement read FUOM write SetUOM;
  end;

{$ENDREGION}


{$REGION 'TMultiStringList'}

type
  TMultiStringList = class;
  TMultiListRef = class;

  //TODO: WHY DID I MAKE THESE? WHERE WERE THEY FOR?

  ///  <summary>
  /// --- NOT READY ---
  ///  Manages contents of multiple TStrings references from one central place
  ///  </summary>
  TMultiStringList = class(TPersistent)
  private
    FReferences: TObjectList<TMultiListRef>;
    FStrings: TStringList;
    function GetStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    constructor Create;
    destructor Destroy; override;
    function ReferenceCount: Integer;
    function AddReference(const AStrings: TStrings): TMultiListRef;
    procedure DeleteReference(const Index: Integer);
    procedure Broadcast;
  published
    property Strings: TStrings read GetStrings write SetStrings;
  end;

  /// --- NOT READY ---
  TMultiListRef = class(TObject)
  private
    FOwner: TMultiStringList;
    FStrings: TStrings;
    procedure SetStrings(const Value: TStrings);
  public
    constructor Create(AOwner: TMultiStringList);
    destructor Destroy; override;
    property Strings: TStrings read FStrings write SetStrings;
  end;

{$ENDREGION}


{ Defaults }

var
  DefaultUOMSystem: TUOMSystem;


////////////////////////////////////////////////////////////////////////////////
implementation
////////////////////////////////////////////////////////////////////////////////


{ TUOMUnitBase }

class function TUOMUnitBase.NamePlural: String;
begin
  //Default, can override.
  Result:= NameSingular + 's';
end;

{ TUOMBase }

class procedure TUOMBase.UnitList(AList: TStrings; ASystem: TUOMSystem);
var
  X: Integer;
  U: TUOMUnitClass;
begin
  AList.Clear;
  for X := 0 to UnitCount-1 do begin
    U:= GetUnit(X);
    if ASystem in U.Systems then
      AList.Add(U.UnitName);
  end;
end;

class function TUOMBase.UnitName(const Index: Integer): String;
begin
  Result:= GetUnit(Index).UnitName;
end;

class function TUOMBase.UnitPrefix(const Index: Integer): String;
begin
  Result:= GetUnit(Index).Prefix;
end;

class function TUOMBase.UnitSuffix(const Index: Integer): String;
begin
  Result:= GetUnit(Index).Suffix;
end;

{ TUOMUtils }

class constructor TUOMUtils.Create;
begin
  FItems:= TList<TUOMBaseClass>.Create;
end;

class destructor TUOMUtils.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

class function TUOMUtils.Convert(const AValue: Double; const AFromUnit,
  AToUnit: TUOMUnitClass): Double;
begin
  Result:= AFromUnit.ConvertToBase(AValue);
  Result:= AToUnit.ConvertFromBase(Result);
end;

class function TUOMUtils.Count: Integer;
begin
  Result:= FItems.Count;
end;

class function TUOMUtils.IndexOf(AClass: TUOMBaseClass): Integer;
var
  X: Integer;
begin
  Result:= -1;
  for X := 0 to FItems.Count-1 do begin
    if AClass = FItems[X] then begin
      Result:= X;
      Break;
    end;
  end;
end;

class function TUOMUtils.IndexOf(AClass: String): Integer;
var
  X: Integer;
begin
  Result:= -1;
  for X := 0 to FItems.Count-1 do begin
    if AClass = FItems[X].UOMName then begin
      Result:= X;
      Break;
    end;
  end;
end;

class procedure TUOMUtils.RegisterUOM(AClass: TUOMBaseClass);
begin
  FItems.Add(AClass);
end;

class function TUOMUtils.UOM(const Index: Integer): TUOMBaseClass;
begin
  Result:= FItems[Index];
end;

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

{ TUnitOfMeasurement }

{$REGION 'TUnitOfMeasurement'}

constructor TUnitOfMeasurement.Create;
begin

end;

destructor TUnitOfMeasurement.Destroy;
begin

  inherited;
end;

procedure TUnitOfMeasurement.Assign(Source: TPersistent);
var
  V: TUnitOfMeasurement;
begin
  if Source is TUnitOfMeasurement then begin
    V:= TUnitOfMeasurement(Source);
    Self.FValue:= V.FValue;
    Self.FUOMIndex:= V.FUOMIndex;
    Self.FUnitFromUndex:= V.FUnitFromUndex;
    Self.FUnitToIndex:= V.FUnitToIndex;
  end else
    inherited;
end;

procedure TUnitOfMeasurement.ListUoms(AList: TStrings);
begin
  //TUOMList.ListUOMs(AList); //TODO
end;

procedure TUnitOfMeasurement.ListUomSystems(AList: TStrings);
begin
  //TUOMList.ListUOMSystems(AList);
end;

procedure TUnitOfMeasurement.ListUomUnits(AList: TStrings);
begin
  //TUOMList.ListUOMUnits(FUOMIndex, AList);
end;

procedure TUnitOfMeasurement.SetUnitFromUndex(const Value: Integer);
begin
  FUnitFromUndex := Value;
end;

procedure TUnitOfMeasurement.SetUnitToIndex(const Value: Integer);
begin
  FUnitToIndex := Value;
end;

procedure TUnitOfMeasurement.SetUOMIndex(const Value: Integer);
begin
  FUOMIndex := Value;
end;

procedure TUnitOfMeasurement.SetValue(const Value: Double);
begin
  FValue := Value;
end;

{$ENDREGION}

{ TMultiStringList }

procedure TMultiStringList.Broadcast;
var
  X: Integer;
  R: TMultiListRef;
begin
  for X := 0 to FReferences.Count-1 do begin
    R:= FReferences[X];
    R.Strings.BeginUpdate;
    try
      try
        R.Strings:= FStrings;
      except
        on E: Exception do begin
          //Typically access violation due to destroyed reference.
          //We could remove this reference if this occurs...
        end;
      end;
    finally
      R.Strings.EndUpdate;
    end;
  end;
end;

constructor TMultiStringList.Create;
begin
  FReferences:= TObjectList<TMultiListRef>.Create(True);
  FStrings:= TStringList.Create;
end;

destructor TMultiStringList.Destroy;
begin
  FreeAndNil(FStrings);
  FreeAndNil(FReferences);
  inherited;
end;

function TMultiStringList.AddReference(const AStrings: TStrings): TMultiListRef;
begin
  Result:= TMultiListRef.Create(Self);
  Result.FStrings:= AStrings;
end;

procedure TMultiStringList.DeleteReference(const Index: Integer);
begin
  FReferences.Delete(Index);
end;

function TMultiStringList.GetStrings: TStrings;
begin
  Result:= FStrings;
end;

function TMultiStringList.ReferenceCount: Integer;
begin
  Result:= FReferences.Count;
end;

procedure TMultiStringList.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

{ TMultiListRef }

constructor TMultiListRef.Create(AOwner: TMultiStringList);
begin
  FOwner:= AOwner;
end;

destructor TMultiListRef.Destroy;
begin

  inherited;
end;

procedure TMultiListRef.SetStrings(const Value: TStrings);
begin
  FStrings.Assign(Value);
end;

{ TUOM }

constructor TUOM.Create(AOwner: TComponent);
begin
  inherited;
  FUOM:= TUnitOfMeasurement.Create;
end;

destructor TUOM.Destroy;
begin
  FreeAndNil(FUOM);
  inherited;
end;

procedure TUOM.SetUOM(const Value: TUnitOfMeasurement);
begin
  FUOM.Assign(Value);
end;

{ TUOMLookupUnit }

constructor TUOMLookupUnit.Create;
begin
  FSystems:= TStringList.Create;
  FSystems.OnChange:= SystemsChanged;
end;

constructor TUOMLookupUnit.Create(const AUOM, ANameSingular, ANamePlural, APrefix, ASuffix,
  ASystems: String; const AFromBase: TConvertProc = nil; const AToBase: TConvertProc = nil);
begin
  Create;
  FUOM:= AUOM;
  //FUnitID:= AID;
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
  TUOMLookupTable.ListUOMs(TUOMLookupTable.FUOMs);
  TUOMLookupTable.ListSystems(TUOMLookupTable.FSystems);
end;

destructor TUOMLookupUnit.Destroy;
begin
  FreeAndNil(FSystems);
end;

procedure TUOMLookupUnit.Invalidate;
begin
  TUOMLookupTable.Invalidate;
end;

function TUOMLookupUnit.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= Self.FConvertFromBaseProc(AValue);
end;

function TUOMLookupUnit.ConvertToBase(const AValue: Double): Double;
begin
  Result:= Self.FConvertToBaseProc(AValue);
end;

{$IFDEF USE_JEDI}

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

procedure TUOMLookupUnit.SetConvertFromBaseProc(const Value: TConvertProc);
begin
  FConvertFromBaseProc := Value;
  //TODO: Validate...
  Invalidate;
end;

procedure TUOMLookupUnit.SetConvertToBaseProc(const Value: TConvertProc);
begin
  FConvertToBaseProc := Value;
  //TODO: Validate...
  Invalidate;
end;

{$ENDIF}

function TUOMLookupUnit.GetSystems: TStrings;
begin
  Result:= TStrings(FSystems);
end;

{
procedure TUOMLookupUnit.SetUnitID(const Value: String);
begin
  FUnitID:= Value;
  //TODO: Validate...
end;
}

procedure TUOMLookupUnit.SetNamePlural(const Value: String);
begin
  FNamePlural:= Value;
  //TODO: Validate...
end;

procedure TUOMLookupUnit.SetNameSingular(const Value: String);
begin
  FNameSingular:= Value;
  //TODO: Validate...
end;

procedure TUOMLookupUnit.SetPrefix(const Value: String);
begin
  FPrefix:= Value;
  //TODO: Validate...
end;

procedure TUOMLookupUnit.SetSuffix(const Value: String);
begin
  FSuffix:= Value;
  //TODO: Validate...
end;

procedure TUOMLookupUnit.SetSystems(const Value: TStrings);
begin
  FSystems.Assign(Value);
  TUOMLookupTable.ListSystems(TUOMLookupTable.FSystems);
end;

procedure TUOMLookupUnit.SetUOM(const Value: String);
begin
  FUOM:= Value;
  TUOMLookupTable.ListUOMs(TUOMLookupTable.FUOMs);
end;

procedure TUOMLookupUnit.SystemsChanged(Sender: TObject);
begin
  TUOMLookupTable.ListSystems(TUOMLookupTable.FSystems);
end;

{ TUOMLookupTable }

class constructor TUOMLookupTable.Create;
begin
  FUnits:= TObjectList<TUOMLookupUnit>.Create(True);
  FBaseUnits:= TDictionary<String, TUOMLookupUnit>.Create;
  FSystems:= TStringList.Create;
  FUOMs:= TStringList.Create;
end;

class destructor TUOMLookupTable.Destroy;
begin
  FreeAndNil(FUOMs);
  FreeAndNil(FSystems);
  FreeAndNil(FBaseUnits);
  FreeAndNil(FUnits);
end;

class function TUOMLookupTable.Convert(const Value: Double; const FromUnit,
  ToUnit: String): Double;
var
  F, T: TUOMLookupUnit;
begin
  //MAIN CONVERSION FUNCTION - Dynamically calls relevant unit conversion procs.

  F:= GetUnitByName(FromUnit);
  if not Assigned(F) then begin
    raise Exception.Create('Conversion from unit "'+F.NameSingular+'" not found.');
  end;

  T:= GetUnitByName(ToUnit);
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
    {$IFDEF USE_JEDI}
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

class procedure TUOMLookupTable.Invalidate;
begin
  ListUOMs(FUOMs);
  ListSystems(FSystems);
end;

class function TUOMLookupTable.GetBaseUnit(const UOM: String): TUOMLookupUnit;
begin
  Result:= FBaseUnits[UOM];
end;

class function TUOMLookupTable.GetUnits(const Index: Integer): TUOMLookupUnit;
begin
  Result:= FUnits[Index];
end;

class function TUOMLookupTable.GetUnitByName(const Name: String): TUOMLookupUnit;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    if Trim(Name) = Trim(FUnits[X].FNameSingular) then begin
      Result:= FUnits[X];
      Break;
    end;
  end;
end;

class function TUOMLookupTable.GetUnitByPrefix(const Prefix: String): TUOMLookupUnit;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    if Trim(Prefix) = Trim(FUnits[X].FPrefix) then begin
      Result:= FUnits[X];
      Break;
    end;
  end;
end;

class function TUOMLookupTable.GetUnitBySuffix(const Suffix: String): TUOMLookupUnit;
var
  X: Integer;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    if Trim(Suffix) = Trim(FUnits[X].FSuffix) then begin
      Result:= FUnits[X];
      Break;
    end;
  end;
end;

class procedure TUOMLookupTable.ListSystems(AList: TStrings);
var
  X, Y: Integer;
  UN: String;
begin
  AList.Clear;
  for X := 0 to FUnits.Count-1 do begin
    for Y := 0 to FUnits[X].FSystems.Count-1 do begin
      UN:= FUnits[X].FSystems[Y];
      if AList.IndexOf(UN) < 0 then
        AList.Append(UN);
    end;
  end;
end;

class procedure TUOMLookupTable.ListUnits(AList: TStrings; const AUOM: String = '';
  const ASystems: String = '');
var
  Inc: Boolean;
  X, Y: Integer;
  U: TUOMLookupUnit;
  L: TStringList;
begin
  AList.Clear;
  Inc:= True;

  for X := 0 to FUnits.Count-1 do begin
    U:= FUnits[X];

    //Filter by UOM...
    if (AUOM <> '') then begin
      Inc:= SameText(U.UOM, AUOM)
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

class procedure TUOMLookupTable.ListUOMs(AList: TStrings);
var
  X: Integer;
  UN: String;
begin
  AList.Clear;
  for X := 0 to FUnits.Count-1 do begin
    UN:= FUnits[X].FUOM;
    if AList.IndexOf(UN) < 0 then
      AList.Append(UN);
  end;
end;

class procedure TUOMLookupTable.RegisterBaseUnit(const AUOM: String;
  const AUnit: TUOMLookupUnit);
begin
  FBaseUnits.Add(AUOM, AUnit);
end;

class procedure TUOMLookupTable.RegisterUnit(const AUnit: TUOMLookupUnit);
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

  if Assigned(GetUnitByName(AUnit.FNameSingular)) or
    Assigned(GetUnitByName(AUnit.FNamePlural)) then
  begin
    raise Exception.Create('Cannot register duplicate unit name '+AUnit.FNameSingular);
  end;

  if Assigned(GetUnitBySuffix(AUnit.FSuffix)) then begin
    raise Exception.Create('Cannot register duplicate unit suffix '+AUnit.FSuffix);
  end;

  FUnits.Add(AUnit);
  Invalidate;
end;

class function TUOMLookupTable.SystemCount: Integer;
begin
  Invalidate;
  Result:= FSystems.Count;
end;

class function TUOMLookupTable.UnitCount: Integer;
begin
  Result:= FUnits.Count;
end;

class function TUOMLookupTable.UOMCount: Integer;
begin
  Invalidate;
  Result:= FUOMs.Count;
end;

{ TUOMValue }

procedure TUOMValue.SetMeasureUnit(const Value: String);
begin
  FMeasureUnit := Value;
end;

procedure TUOMValue.SetValue(const Value: Double);
begin
  FValue := Value;
end;

initialization
  DefaultUOMSystem:= TUOMSystem.ustMetric;

end.
