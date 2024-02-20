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

  What's In This Library:

  - Enumeration types for all possible metrics
    - Divided among different units for different UOMs
    - TUOMSystem: Different measurement systems (Metric, US Customary...)
    - TUOM: Different types of measurement (Length, Area, Volume, Weight...)
    - TUOMLengthUnit, TUOMAreaUnit, TUOMVolumeUnit...: Different UOMs within a certain type
    - Every enum has a corresponding set type with a plural "s" at the end of the type
  - Classes with class methods for all UOM types
    - For exapmle, TUOMLengthUtils, or TUOMWeightUtils
    - Provides functions to convert between any given unit types
    - For example, Feet:= TUOMLengthUtils.KilometersToFeet(1520.86);
  - Basic information about different units of measurement
    - Enums defining which units are used by each measurement system for any given UOM
    - Descriptive names accessible via UnitName functions
  - Conversion of metrics within a Double system
  - Conversion of metrics across different systems
  - Implicit record types to consume measurement data
  - Foldable Code Regions
  - Default Unit of Each UOM for Internal Storage

  Measurement Systems (TUOMSystem / TUOMSystems):
  - Any (Not specific, applicable to any)
  - Metric (Meters, Liters, Kilograms...)
  - US Customary (Feet, Gallons, Pounds...)

  Units of Measurement (TUOM):
  - [COMPLETE] Length (Feet, Inches, Miles...)
  - [COMPLETE] Area (Square Feet, Square Inches, Square Miles...)
  - [IN PROGRESS] Volume (Cubic Feet, Cubic Inches, Fuid Ounces...)
  - [IN PROGRESS] Weight (Pounds, Ounces, Kilograms...)
  - [COMPLETE] Temperature (Celcius, Farenheit, Kelvin...)
  - [TODO] Energy (Joules, Food Calories...)
  - [TODO] Speed (Miles per Hour, Kilometers per Hour, Mach...)
  - [TODO] Time (Seconds, Hours, Years...)
  - [TODO] Power (Watts, Horse Power...)
  - [TODO] Data (Bytes, Gigabits...)
  - [TODO] Pressure (Bars, Pascals...)
  - [TODO] Angle (Degrees, Radians...)
  - [TODO] Resistance (Ohms, Megaohms...)
  - [TODO] Capacitance (Farad, Microfarad...)
  - [TODO] Voltage (Volts, Microvolts...)
  - [TODO] Current (Amps, Milliamps...)
  - [TODO] Density
  - [TODO] Gravity
  - [TODO] Radiation


  https://www.metric-conversions.org/

*)

{$ENDREGION}

uses
  System.Classes, System.SysUtils, System.Generics.Collections
  ;


{ General }

const
  PartOfNumber = ['0'..'9', '.', ','];
  NumFormat = '#,###,###,###,##0.#############';

type
  TUOMSystem = (ustAny, ustMetric, ustUSCustomary, ustImperial);
  TUOMSystems = set of TUOMSystem;

type
  /// <summary>
  /// Represents a unit of measurement type.
  /// TODO: Get rid of the need of this, so units register themselves...
  /// </summary>
  TUOM = (umLength, umArea, umVolume, umWeight, umTemperature,
    umEnergy, umSpeed, umTime, umPower, umData, umPressure, umAngle,
    umResistance, umCapacitance, umVoltage, umCurrent, umDensity, umGravity,
    umRadiation, umFrequency, umMass, umResolution);
  TUOMs = set of TUOM;

  /// <summary>
  /// Provides utilities for all units of measurements.
  /// </summary>
  TUOMUtils = class
  public
    class procedure ListUOMSystems(AList: TStrings); static;
    class procedure ListUOMs(AList: TStrings); static;
    class procedure ListUOMUnits(const AUOM: TUOM; AList: TStrings;
      const ASystem: TUOMSystem = TUOMSystem.ustAny); static;
    class function UOMName(const AUOM: TUOM): String; static;
    class procedure ParseSuffix(AValue: String; var ANumber: Double; var ASuffix: String); static;
  end;

{ TUnitOfMeasurement }

{$REGION 'TUnitOfMeasurement'}

type
  /// <summary>
  /// Encapsulates full conversion process with easy to use properties.
  /// Can be used to quickly access all different possible units of measurement.
  /// Can be integrated into a component since it's a TPersistent
  /// </summary>
  TUnitOfMeasurement = class(TPersistent)
  private
    FUOM: TUOM;
    FValue: Double;
    FUnitToIndex: Integer;
    FUnitFromUndex: Integer;
    procedure SetUnitFromUndex(const Value: Integer);
    procedure SetUnitToIndex(const Value: Integer);
    procedure SetUOM(const Value: TUOM);
    procedure SetValue(const Value: Double);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure ListUoms(AList: TStrings);
    procedure ListUomUnits(AList: TStrings);
    procedure ListUomSystems(AList: TStrings);
  published
    property UOM: TUOM read FUOM write SetUOM;
    property Value: Double read FValue write SetValue;
    property UnitFromUndex: Integer read FUnitFromUndex write SetUnitFromUndex;
    property UnitToIndex: Integer read FUnitToIndex write SetUnitToIndex;
  end;

{$ENDREGION}



{$REGION 'TMultiStringList'}

type
  TMultiStringList = class;
  TMultiListRef = class;

  ///  <summary>
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

uses
  JD.Uom.Angle,
  JD.Uom.Area,
  JD.Uom.Capacitance,
  JD.Uom.Current,
  JD.Uom.Data,
  JD.Uom.Density,
  JD.Uom.Energy,
  JD.Uom.Frequency,
  JD.Uom.Gravity,
  JD.Uom.Length,
  JD.Uom.Mass,
  JD.Uom.Power,
  JD.Uom.Pressure,
  JD.Uom.Radiation,
  JD.Uom.Resistance,
  JD.Uom.Resolution,
  JD.Uom.Speed,
  JD.Uom.Temperature,
  JD.Uom.Time,
  JD.Uom.Voltage,
  JD.Uom.Volume,
  JD.Uom.Weight;


{ TUOMUtils }

class procedure TUOMUtils.ListUOMSystems(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Any System');
  AList.Append('Metric');
  AList.Append('US Customary');
  AList.Append('Imperial');
  //TODO...
end;

class procedure TUOMUtils.ListUOMUnits(const AUOM: TUOM; AList: TStrings;
  const ASystem: TUOMSystem = TUOMSystem.ustAny);
begin
  AList.Clear;
  case AUOM of
    umLength:       TUOMLengthUtils.UnitList(AList, ASystem);
    umArea:         TUOMAreaUtils.UnitList(AList);
    umVolume:       TUOMVolumeUtils.UnitList(AList);
    umWeight:       TUOMWeightUtils.UnitList(AList);
    umTemperature:  TUOMTemperatureUtils.UnitList(AList);
    umEnergy:       TUOMEnergyUtils.UnitList(AList);
    umSpeed:        TUOMSpeedUtils.UnitList(AList);
    umTime:         TUOMTimeUtils.UnitList(AList);
    umPower:        TUOMPowerUtils.UnitList(AList);
    umData:         TUOMDataUtils.UnitList(AList);
    umPressure:     TUOMPressureUtils.UnitList(AList);
    umAngle:        ;
    umResistance:   ;
    umCapacitance:  ;
    umVoltage:      ;
    umCurrent:      ;
    umDensity:      ;
    umGravity:      ;
    umFrequency:    ;
    umMass:         ;
    umResolution:   ;
  end;
end;

class function TUOMUtils.UOMName(const AUOM: TUOM): String;
begin
  case AUOM of
    umLength:       Result:= 'Length';
    umArea:         Result:= 'Area';
    umVolume:       Result:= 'Volume';
    umWeight:       Result:= 'Weight';
    umTemperature:  Result:= 'Temperature';
    umEnergy:       Result:= 'Energy';
    umSpeed:        Result:= 'Speed';
    umTime:         Result:= 'Time';
    umPower:        Result:= 'Power';
    umData:         Result:= 'Data';
    umPressure:     Result:= 'Pressure';
    umAngle:        Result:= 'Angle';
    umResistance:   Result:= 'Resistance';
    umCapacitance:  Result:= 'Capacitance';
    umVoltage:      Result:= 'Voltage';
    umCurrent:      Result:= 'Current';
    umDensity:      Result:= 'Density';
    umGravity:      Result:= 'Gravity';
    umRadiation:    Result:= 'Radiation';
  end;
end;

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

class procedure TUOMUtils.ListUOMs(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Length');
  AList.Append('Area');
  AList.Append('Volume');
  AList.Append('Weight');
  AList.Append('Temperature');
  AList.Append('Energy');
  AList.Append('Speed');
  AList.Append('Time');
  AList.Append('Power');
  AList.Append('Data');
  AList.Append('Pressure');
  AList.Append('Angle');
  AList.Append('Resistance');
  AList.Append('Capacitance');
  AList.Append('Voltage');
  AList.Append('Current');
  AList.Append('Density');
  AList.Append('Gravity');
  AList.Append('Radiation');
end;




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
    Self.FUOM:= V.FUOM;
    Self.FUnitFromUndex:= V.FUnitFromUndex;
    Self.FUnitToIndex:= V.FUnitToIndex;
  end else
    inherited;
end;

procedure TUnitOfMeasurement.ListUoms(AList: TStrings);
begin
  TUOMUtils.ListUOMs(AList);
end;

procedure TUnitOfMeasurement.ListUomSystems(AList: TStrings);
begin
  TUOMUtils.ListUOMSystems(AList);
end;

procedure TUnitOfMeasurement.ListUomUnits(AList: TStrings);
begin
  TUOMUtils.ListUOMUnits(FUOM, AList);
end;

procedure TUnitOfMeasurement.SetUnitFromUndex(const Value: Integer);
begin
  FUnitFromUndex := Value;
end;

procedure TUnitOfMeasurement.SetUnitToIndex(const Value: Integer);
begin
  FUnitToIndex := Value;
end;

procedure TUnitOfMeasurement.SetUOM(const Value: TUOM);
begin
  FUOM := Value;
end;

procedure TUnitOfMeasurement.SetValue(const Value: Double);
begin
  FValue := Value;
end;

{$ENDREGION}



{ TMultiList }

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

initialization
  DefaultUOMSystem:= TUOMSystem.ustUSCustomary;

end.
