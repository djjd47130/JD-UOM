unit JD.Uom.Length;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  JD.Uom;

type
  TUOMLengthUnit = (umlNanometers, umlMicrons, umlMillimeters, umlCentimeters,
    umlMeters, umlKilometers, umlInches, umlFeet, umlYards, umlMiles, umlNauticalMiles);
  TUOMLengthUnits = set of TUOMLengthUnit;



  //TODO: Implement new concept of conversion matrix to replace
  //current boilerplate conversion code...
  TUOMLengthMatrixProc = procedure(const A, B: TUOMLengthUnit; var Value: Double);
  TUOMLengthUnitMatrix = array[TUOMLengthUnit,TUOMLengthUnit] of TUOMLengthMatrixProc;

  TUOMLengthMatrix = class
  private
    class var FMatrix: TUOMLengthUnitMatrix;
  public
    class function Convert(const A, B: TUOMLengthUnit; const Val: Double): Double;
  end;



  TUOMLengthUtils = class
  public
    class procedure UnitList(AList: TStrings; ASystem: TUOMSystem = ustAny); static;
    class function UnitSuffix(const AValue: TUOMLengthUnit): String; static;
    class function UnitSystem(const AValue: TUOMLengthUnit): TUOMSystem; static;
    class function UnitsOfSystem(const ASystem: TUOMSystem): TUOMLengthUnits;
    class function UnitName(const AValue: TUOMLengthUnit): String; static;
    class function StrToUnit(const AValue: String): TUOMLengthUnit;

    { US Customary }

    //Inches
    class function FeetToInches(const AFeet: Double): Double; overload; static;
    class function FeetToInches(const AFeet, AInches: Integer): Double; overload; static;
    class function YardsToInches(const AYards: Double): Double; static;
    class function MilesToInches(const AMiles: Double): Double; static;
    class function NauticalMilesToInches(ANauticalMiles: Double): Double; static;
    //Feet
    class function InchesToFeet(const AInches: Double): Double; static;
    class function YardsToFeet(const AYards: Double): Double; static;
    class function MilesToFeet(const AMiles: Double): Double; static;
    class function NauticalMilesToFeet(const ANauticalMiles: Double): Double; static;
    //Yards
    class function InchesToYards(const AInches: Double): Double; static;
    class function FeetToYards(const AFeet: Double): Double; static;
    class function MilesToYards(const AMiles: Double): Double; static;
    class function NauticalMilesToYards(const ANauticalMiles: Double): Double; static;
    //Miles
    class function InchesToMiles(const AInches: Double): Double; static;
    class function FeetToMiles(const AFeet: Double): Double; static;
    class function YardsToMiles(const AYards: Double): Double; static;
    class function NauticalMilesToMiles(const ANauticalMiles: Double): Double; static;
    //Nautical Miles
    class function InchesToNauticalMiles(const AInches: Double): Double; static;
    class function FeetToNauticalMiles(const AFeet: Double): Double; static;
    class function YardsToNauticalMiles(const AYards: Double): Double; static;
    class function MilesToNauticalMiles(const AMiles: Double): Double; static;

    { Metric }

    //Nanometers
    class function MicronsToNanometers(const AMicrons: Double): Double; static;
    class function MillimetersToNanometers(const AMillimeters: Double): Double; static;
    class function CentimetersToNanometers(const ACentimeters: Double): Double; static;
    class function MetersToNanometers(const AMeters: Double): Double; static;
    class function KilometersToNanometers(const AKilometers: Double): Double; static;
    //Microns
    class function NanometersToMicrons(const ANanometers: Double): Double; static;
    class function MillimetersToMicrons(const AMillimeters: Double): Double; static;
    class function CentimetersToMicrons(const ACentimeters: Double): Double; static;
    class function MetersToMicrons(const AMeters: Double): Double; static;
    class function KilometersToMicrons(const AKilometers: Double): Double; static;
    //Millimeters
    class function MicronsToMillimeters(const AMicrons: Double): Double; static;
    class function NanometersToMillimeters(const ANanometers: Double): Double; static;
    class function CentimetersToMillimeters(const ACentimeters: Double): Double; static;
    class function MetersToMillimeters(const AMeters: Double): Double; static;
    class function KilometersToMillimeters(const AKilometers: Double): Double; static;
    //Centimeters
    class function MicronsToCentimeters(const AMicrons: Double): Double; static;
    class function NanometersToCentimeters(const ANanometers: Double): Double; static;
    class function MillimetersToCentimeters(const AMillimeters: Double): Double; static;
    class function MetersToCentimeters(const AMeters: Double): Double; static;
    class function KilometersToCentimeters(const AKilometers: Double): Double; static;
    //Meters
    class function MicronsToMeters(const AMicrons: Double): Double; static;
    class function NanometersToMeters(const ANanometers: Double): Double; static;
    class function MillimetersToMeters(const AMillimeters: Double): Double; static;
    class function CentimetersToMeters(const ACentimeters: Double): Double; static;
    class function KilometersToMeters(const AKilometers: Double): Double; static;
    //Kilometers
    class function MicronsToKilometers(const AMicrons: Double): Double; static;
    class function NanometersToKilometers(const ANanometers: Double): Double; static;
    class function MillimetersToKilometers(const AMillimeters: Double): Double; static;
    class function CentimetersToKilometers(const ACentimeters: Double): Double; static;
    class function MetersToKilometers(const AMeters: Double): Double; static;

    { US Customary to Metric Conversion }

    //Nanometers
    class function InchesToNanometers(const AInches: Double): Double; static;
    class function FeetToNanometers(const AFeet: Double): Double; static;
    class function YardsToNanometers(const AYards: Double): Double; static;
    class function MilesToNanometers(const AMiles: Double): Double; static;
    class function NauticalMilesToNanometers(const ANauticalMiles: Double): Double; static;
    //Microns
    class function InchesToMicrons(const AInches: Double): Double; static;
    class function FeetToMicrons(const AFeet: Double): Double; static;
    class function YardsToMicrons(const AYards: Double): Double; static;
    class function MilesToMicrons(const AMiles: Double): Double; static;
    class function NauticalMilesToMicrons(const ANauticalMiles: Double): Double; static;
    //Millimeteres
    class function InchesToMillimeters(const AInches: Double): Double; static;
    class function FeetToMillimeters(const AFeet: Double): Double; static;
    class function YardsToMillimeters(const AYards: Double): Double; static;
    class function MilesToMillimeters(const AMiles: Double): Double; static;
    class function NauticalMilesToMillimeters(const ANauticalMiles: Double): Double; static;
    //Centimeters
    class function InchesToCentimeters(const AInches: Double): Double; static;
    class function FeetToCentimeters(const AFeet: Double): Double; static;
    class function YardsToCentimeters(const AYards: Double): Double; static;
    class function MilesToCentimeters(const AMiles: Double): Double; static;
    class function NauticalMilesToCentimeters(const ANauticalMiles: Double): Double; static;
    //Meters
    class function InchesToMeters(const AInches: Double): Double; static;
    class function FeetToMeters(const AFeet: Double): Double; static;
    class function YardsToMeters(const AYards: Double): Double; static;
    class function MilesToMeters(const AMiles: Double): Double; static;
    class function NauticalMilesToMeters(const ANauticalMiles: Double): Double; static;
    //Kilomenters
    class function InchesToKilometers(const AInches: Double): Double; static;
    class function FeetToKilometers(const AFeet: Double): Double; static;
    class function YardsToKilometers(const AYards: Double): Double; static;
    class function MilesToKilometers(const AMiles: Double): Double; static;
    class function NauticalMilesToKilometers(const ANauticalMiles: Double): Double; static;

    { Metric to US Customary Conversion }

    //Inches
    class function NanometersToInches(const ANanometers: Double): Double; static;
    class function MicronsToInches(const AMicrons: Double): Double; static;
    class function MillimetersToInches(const AMillimeters: Double): Double; static;
    class function CentimetersToInches(const ACentimeters: Double): Double; static;
    class function MetersToInches(const AMeters: Double): Double; static;
    class function KilometersToInches(const AKilometers: Double): Double; static;
    //Feet
    class function NanometersToFeet(const ANanometers: Double): Double; static;
    class function MicronsToFeet(const AMicrons: Double): Double; static;
    class function MillimetersToFeet(const AMillimeters: Double): Double; static;
    class function CentimetersToFeet(const ACentimeters: Double): Double; static;
    class function MetersToFeet(const AMeters: Double): Double; static;
    class function KilometersToFeet(const AKilometers: Double): Double; static;
    //Yards
    class function NanometersToYards(const ANanometers: Double): Double; static;
    class function MicronsToYards(const AMicrons: Double): Double; static;
    class function MillimetersToYards(const AMillimeters: Double): Double; static;
    class function CentimetersToYards(const ACentimeters: Double): Double; static;
    class function MetersToYards(const AMeters: Double): Double; static;
    class function KilometersToYards(const AKilometers: Double): Double; static;
    //Miles
    class function NanometersToMiles(const ANanometers: Double): Double; static;
    class function MicronsToMiles(const AMicrons: Double): Double; static;
    class function MillimetersToMiles(const AMillimeters: Double): Double; static;
    class function CentimetersToMiles(const ACentimeters: Double): Double; static;
    class function MetersToMiles(const AMeters: Double): Double; static;
    class function KilometersToMiles(const AKilometers: Double): Double; static;
    //Nautical Miles
    class function NanometersToNauticalMiles(const ANanometers: Double): Double; static;
    class function MicronsToNauticalMiles(const AMicrons: Double): Double; static;
    class function MillimetersToNauticalMiles(const AMillimeters: Double): Double; static;
    class function CentimetersToNauticalMiles(const ACentimeters: Double): Double; static;
    class function MetersToNauticalMiles(const AMeters: Double): Double; static;
    class function KilometersToNauticalMiles(const AKilometers: Double): Double; static;

  end;

  TUOMLength = record
  private
    FUnit: TUOMLengthUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMLengthUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMLengthUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMLength;
    class operator implicit(const AValue: TUOMLength): Double;
    class operator implicit(const AValue: String): TUOMLength;
    class operator implicit(const AValue: TUOMLength): String;
    class operator Equal(const A, B: TUOMLength): Boolean;
    class operator GreaterThan(const A, B: TUOMLength): Boolean;
    class operator LessThan(const A, B: TUOMLength): Boolean;
    class operator Add(const A, B: TUOMLength): TUOMLength;
    class operator Subtract(const A, B: TUOMLength): TUOMLength;
    class operator Multiply(const A, B: TUOMLength): TUOMLength;
    class operator Divide(const A, B: TUOMLength): TUOMLength;
  public
    //TODO: Shouldn't I change these to TUOMLength instead of Double?!
    function GetAsNanometers: Double;
    function GetAsMicrons: Double;
    function GetAsMillimeters: Double;
    function GetAsCentimeters: Double;
    function GetAsMeters: Double;
    function GetAsKilometers: Double;
    function GetAsInches: Double;
    function GetAsFeet: Double;
    function GetAsYards: Double;
    function GetAsMiles: Double;
    function GetAsNauticalMiles: Double;
    procedure SetAsCentimeters(const Value: Double);
    procedure SetAsFeet(const Value: Double);
    procedure SetAsInches(const Value: Double);
    procedure SetAsKilometers(const Value: Double);
    procedure SetAsMeters(const Value: Double);
    procedure SetAsMicrons(const Value: Double);
    procedure SetAsMiles(const Value: Double);
    procedure SetAsMillimeters(const Value: Double);
    procedure SetAsNanometers(const Value: Double);
    procedure SetAsNauticalMiles(const Value: Double);
    procedure SetAsYards(const Value: Double);
  public
    property AsNanometers: Double read GetAsNanometers write SetAsNanometers;
    property AsMicrons: Double read GetAsMicrons write SetAsMicrons;
    property AsMillimeters: Double read GetAsMillimeters write SetAsMillimeters;
    property AsCentimeters: Double read GetAsCentimeters write SetAsCentimeters;
    property AsMeters: Double read GetAsMeters write SetAsMeters;
    property AsKilometers: Double read GetAsKilometers write SetAsKilometers;
    property AsInches: Double read GetAsInches write SetAsInches;
    property AsFeet: Double read GetAsFeet write SetAsFeet;
    property AsYards: Double read GetAsYards write SetAsYards;
    property AsMiles: Double read GetAsMiles write SetAsMiles;
    property AsNauticalMiles: Double read GetAsNauticalMiles write SetAsNauticalMiles;
  end;

implementation

uses
  System.Math;

var
  DefaultLengthUnit: TUOMLengthUnit;

{ TUOMLengthUtils }

class function TUOMLengthUtils.UnitSuffix(const AValue: TUOMLengthUnit): String;
begin
  case AValue of
    umlNanometers:    Result:= 'nm';
    umlMicrons:       Result:= 'μm';
    umlMillimeters:   Result:= 'mm';
    umlCentimeters:   Result:= 'cm';
    umlMeters:        Result:= 'm';
    umlKilometers:    Result:= 'km';
    umlInches:        Result:= '"';
    umlFeet:          Result:= '''';
    umlYards:         Result:= 'yd';
    umlMiles:         Result:= 'mi';
    umlNauticalMiles: Result:= 'nmi';
  end;
end;

class function TUOMLengthUtils.StrToUnit(const AValue: String): TUOMLengthUnit;
  procedure Chk(const U: TUOMLengthUnit; const S: String);
  begin
    if Trim(LowerCase(AValue)) = Trim(LowerCase(S)) then
      Result:= U;
  end;
begin
  Chk(TUOMLengthUnit.umlNanometers,     'nm');
  Chk(TUOMLengthUnit.umlMicrons,        'μm');
  Chk(TUOMLengthUnit.umlMillimeters,    'mm');
  Chk(TUOMLengthUnit.umlCentimeters,    'cm');
  Chk(TUOMLengthUnit.umlMeters,         'm');
  Chk(TUOMLengthUnit.umlKilometers,     'km');
  Chk(TUOMLengthUnit.umlInches,         '"');
  Chk(TUOMLengthUnit.umlFeet,           '''');
  Chk(TUOMLengthUnit.umlYards,          'yd');
  Chk(TUOMLengthUnit.umlMiles,          'mi');
  Chk(TUOMLengthUnit.umlNauticalMiles,  'nmi');
end;

class procedure TUOMLengthUtils.UnitList(AList: TStrings; ASystem: TUOMSystem = ustAny);
var
  Units: TUOMLengthUnits;
  procedure A(const U: TUOMLengthUnit; const S: String);
  begin
    if U in Units then
      AList.AddObject(S, Pointer(Integer(U)));
  end;
begin
  AList.Clear;
  Units:= TUOMLengthUtils.UnitsOfSystem(ASystem);
  A(TUOMLengthUnit.umlNanometers,     'Nanometers');
  A(TUOMLengthUnit.umlMicrons,        'Microns');
  A(TUOMLengthUnit.umlMillimeters,    'Millimeters');
  A(TUOMLengthUnit.umlCentimeters,    'Centimeters');
  A(TUOMLengthUnit.umlMeters,         'Meters');
  A(TUOMLengthUnit.umlKilometers,     'Kilometers');
  A(TUOMLengthUnit.umlInches,         'Inches');
  A(TUOMLengthUnit.umlFeet,           'Feet');
  A(TUOMLengthUnit.umlYards,          'Yards');
  A(TUOMLengthUnit.umlMiles,          'Miles');
  A(TUOMLengthUnit.umlNauticalMiles,  'Nautical Miles');
end;

class function TUOMLengthUtils.UnitName(const AValue: TUOMLengthUnit): String;
begin
  case AValue of
    umlNanometers:    Result:= 'Nanometers';
    umlMicrons:       Result:= 'Microns';
    umlMillimeters:   Result:= 'Millimeters';
    umlCentimeters:   Result:= 'Centimeters';
    umlMeters:        Result:= 'Meters';
    umlKilometers:    Result:= 'Kilometers';
    umlInches:        Result:= 'Inches';
    umlFeet:          Result:= 'Feet';
    umlYards:         Result:= 'Yards';
    umlMiles:         Result:= 'Miles';
    umlNauticalMiles: Result:= 'Nautical Miles';
  end;
end;

class function TUOMLengthUtils.UnitsOfSystem(
  const ASystem: TUOMSystem): TUOMLengthUnits;
begin
  case ASystem of
    ustAny:         Result:= [umlNanometers, umlMicrons, umlMillimeters, umlCentimeters,
      umlMeters, umlKilometers, umlInches, umlFeet, umlYards, umlMiles, umlNauticalMiles];
    ustMetric:      Result:= [umlNanometers, umlMicrons, umlMillimeters, umlCentimeters,
      umlMeters, umlKilometers];
    ustUSCustomary: Result:= [umlInches, umlFeet, umlYards, umlMiles, umlNauticalMiles];
    ustImperial:    Result:= [umlNanometers, umlMicrons, umlMillimeters, umlCentimeters,
      umlMeters, umlKilometers];
  end;
end;

class function TUOMLengthUtils.UnitSystem(const AValue: TUOMLengthUnit): TUOMSystem;
begin
  Result:= TUOMSystem.ustAny;
  case AValue of
    umlNanometers:    Result:= TUOMSystem.ustMetric;
    umlMicrons:       Result:= TUOMSystem.ustAny;
    umlMillimeters:   Result:= TUOMSystem.ustMetric;
    umlCentimeters:   Result:= TUOMSystem.ustMetric;
    umlMeters:        Result:= TUOMSystem.ustMetric;
    umlKilometers:    Result:= TUOMSystem.ustMetric;
    umlInches:        Result:= TUOMSystem.ustUSCustomary;
    umlFeet:          Result:= TUOMSystem.ustUSCustomary;
    umlYards:         Result:= TUOMSystem.ustUSCustomary;
    umlMiles:         Result:= TUOMSystem.ustUSCustomary;
    umlNauticalMiles: Result:= TUOMSystem.ustAny;
  end;
end;

class function TUOMLengthUtils.FeetToInches(const AFeet: Double): Double;
begin
  Result:= AFeet * 12;
end;

class function TUOMLengthUtils.FeetToInches(const AFeet, AInches: Integer): Double;
begin
  Result:= FeetToInches(AFeet) + AInches;
end;

class function TUOMLengthUtils.InchesToFeet(const AInches: Double): Double;
begin
  Result:= AInches / 12;
end;

class function TUOMLengthUtils.YardsToFeet(const AYards: Double): Double;
begin
  Result:= AYards * 3;
end;

class function TUOMLengthUtils.FeetToYards(const AFeet: Double): Double;
begin
  Result:= AFeet / 3;
end;

class function TUOMLengthUtils.YardsToInches(const AYards: Double): Double;
begin
  Result:= FeetToInches(YardsToFeet(AYards));
end;

class function TUOMLengthUtils.InchesToYards(const AInches: Double): Double;
begin
  Result:= FeetToYards(InchesToFeet(AInches));
end;

class function TUOMLengthUtils.FeetToMicrons(const AFeet: Double): Double;
begin
  Result:= AFeet * 304800;
end;

class function TUOMLengthUtils.FeetToMiles(const AFeet: Double): Double;
begin
  Result:= AFeet / 5280;
end;

class function TUOMLengthUtils.MilesToFeet(const AMiles: Double): Double;
begin
  Result:= AMiles * 5280;
end;

class function TUOMLengthUtils.YardsToMicrons(const AYards: Double): Double;
begin
  Result:= AYards * 914400;
end;

class function TUOMLengthUtils.YardsToMiles(const AYards: Double): Double;
begin
  Result:= AYards / 1760;
end;

class function TUOMLengthUtils.MilesToYards(const AMiles: Double): Double;
begin
  Result:= AMiles * 1760;
end;

class function TUOMLengthUtils.InchesToMicrons(const AInches: Double): Double;
begin
  Result:= AInches * 25400;
end;

class function TUOMLengthUtils.InchesToMiles(const AInches: Double): Double;
begin
  Result:= FeetToMiles(InchesToFeet(AInches));
end;

class function TUOMLengthUtils.MilesToInches(const AMiles: Double): Double;
begin
  Result:= FeetToInches(MilesToFeet(AMiles));
end;

class function TUOMLengthUtils.MillimetersToCentimeters(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters / 10;
end;

class function TUOMLengthUtils.CentimetersToMillimeters(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 10;
end;

class function TUOMLengthUtils.MillimetersToMeters(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters / 1000;
end;

class function TUOMLengthUtils.KilometersToMeters(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 1000;
end;

class function TUOMLengthUtils.MetersToKilometers(const AMeters: Double): Double;
begin
  Result:= AMeters / 1000;
end;

class function TUOMLengthUtils.CentimetersToInches(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 0.393701;
end;

class function TUOMLengthUtils.MillimetersToInches(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters * 0.0393701;
end;

class function TUOMLengthUtils.InchesToCentimeters(const AInches: Double): Double;
begin
  Result:= AInches / 2.54;
end;

class function TUOMLengthUtils.MetersToMillimeters(const AMeters: Double): Double;
begin
  Result:= AMeters * 1000;
end;

class function TUOMLengthUtils.MetersToNauticalMiles(const AMeters: Double): Double;
begin
  Result:= AMeters * 0.000539957;
end;

class function TUOMLengthUtils.MetersToMiles(const AMeters: Double): Double;
begin
  Result:= AMeters * 0.000621371;
end;

class function TUOMLengthUtils.MillimetersToMicrons(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters * 1000;
end;

class function TUOMLengthUtils.MillimetersToYards(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters * 0.00109361;
end;

class function TUOMLengthUtils.NanometersToMicrons(const ANanometers: Double): Double;
begin
  Result:= ANanometers * 0.001;
end;

class function TUOMLengthUtils.NauticalMilesToCentimeters(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 185200;
end;

class function TUOMLengthUtils.NauticalMilesToFeet(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 6076.12
end;

class function TUOMLengthUtils.NauticalMilesToInches(ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 72913.4
end;

class function TUOMLengthUtils.NauticalMilesToKilometers(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 1.852;
end;

class function TUOMLengthUtils.NauticalMilesToMeters(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 1852;
end;

class function TUOMLengthUtils.NauticalMilesToMiles(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 1.15078;
end;

class function TUOMLengthUtils.NauticalMilesToYards(const ANauticalMiles: Double): Double;
begin
  Result:= ANauticalMiles * 2025.37;
end;

class function TUOMLengthUtils.KilometersToCentimeters(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 100000;
end;

class function TUOMLengthUtils.KilometersToFeet(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 3280.84;
end;

class function TUOMLengthUtils.KilometersToInches(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 39370.1;
end;

class function TUOMLengthUtils.CentimetersToFeet(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 0.0328084;
end;

class function TUOMLengthUtils.MetersToCentimeters(const AMeters: Double): Double;
begin
  Result:= AMeters * 100;
end;

class function TUOMLengthUtils.MetersToFeet(const AMeters: Double): Double;
begin
  Result:= AMeters * 3.28084;
end;

class function TUOMLengthUtils.FeetToMeters(const AFeet: Double): Double;
begin
  Result:= AFeet * 0.3048;
end;

class function TUOMLengthUtils.MetersToYards(const AMeters: Double): Double;
begin
  Result:= AMeters * 1.09361;
end;

class function TUOMLengthUtils.YardsToMeters(const AYards: Double): Double;
begin
  Result:= AYards * 0.9144;
end;

class function TUOMLengthUtils.MetersToInches(const AMeters: Double): Double;
begin
  Result:= AMeters * 39.3701;
end;

class function TUOMLengthUtils.InchesToMeters(const AInches: Double): Double;
begin
  Result:= AInches / 39.3701;
end;

class function TUOMLengthUtils.KilometersToMiles(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 0.621371;
end;

class function TUOMLengthUtils.KilometersToNauticalMiles(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 0.539957;
end;

class function TUOMLengthUtils.KilometersToYards(const AKilometers: Double): Double;
begin
  Result:= AKilometers * 1093.61;
end;

class function TUOMLengthUtils.MilesToKilometers(const AMiles: Double): Double;
begin
  Result:= AMiles * 1.60934;
end;

class function TUOMLengthUtils.MilesToMeters(const AMiles: Double): Double;
begin
  Result:= AMiles * 1609.34;
end;

class function TUOMLengthUtils.MilesToNauticalMiles(const AMiles: Double): Double;
begin
  Result:= AMiles * 0.868976;
end;

class function TUOMLengthUtils.InchesToMillimeters(const AInches: Double): Double;
begin
  Result:= AInches * 25.4;
end;

class function TUOMLengthUtils.YardsToMillimeters(const AYards: Double): Double;
begin
  Result:= AYards * 914.4;
end;

class function TUOMLengthUtils.YardsToNauticalMiles(const AYards: Double): Double;
begin
  Result:= AYards * 0.000493737;
end;

class function TUOMLengthUtils.FeetToMillimeters(const AFeet: Double): Double;
begin
  Result:= AFeet * 304.8;
end;

class function TUOMLengthUtils.FeetToNauticalMiles(const AFeet: Double): Double;
begin
  Result:= AFeet * 0.000164579;
end;

class function TUOMLengthUtils.MicronsToMillimeters(const AMicrons: Double): Double;
begin
  Result:= AMicrons * 0.001;
end;

class function TUOMLengthUtils.MicronsToNanometers(const AMicrons: Double): Double;
begin
  Result:= AMicrons * 1000;
end;

class function TUOMLengthUtils.MilesToCentimeters(const AMiles: Double): Double;
begin
  Result:= AMiles * 160934;
end;

class function TUOMLengthUtils.YardsToKilometers(const AYards: Double): Double;
begin
  Result:= AYards * 0.0009144;
end;

class function TUOMLengthUtils.YardsToCentimeters(const AYards: Double): Double;
begin
  Result:= AYards * 91.44;
end;

class function TUOMLengthUtils.FeetToCentimeters(const AFeet: Double): Double;
begin
  Result:= AFeet * 30.48;
end;

class function TUOMLengthUtils.FeetToKilometers(const AFeet: Double): Double;
begin
  Result:= AFeet * 0.0003048;
end;

class function TUOMLengthUtils.CentimetersToMeters(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 0.01;
end;

class function TUOMLengthUtils.CentimetersToMicrons(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 10000;
end;

class function TUOMLengthUtils.MillimetersToFeet(const AMillimeters: Double): Double;
begin
  Result:= AMillimeters * 0.00328084;
end;

class function TUOMLengthUtils.CentimetersToYards(const ACentimeters: Double): Double;
begin
  Result:= ACentimeters * 0.0109361;
end;

class function TUOMLengthUtils.NanometersToMillimeters(const ANanometers: Double): Double;
begin
  Result:= MicronsToMillimeters(NanometersToMicrons(ANanometers));
end;

class function TUOMLengthUtils.NanometersToInches(const ANanometers: Double): Double;
begin
  Result:= MillimetersToInches(NanometersToMillimeters(ANanometers));
end;

class function TUOMLengthUtils.KilometersToMillimeters(const AKilometers: Double): Double;
begin
  Result:= MetersToMillimeters(KilometersToMeters(AKilometers));
end;

class function TUOMLengthUtils.InchesToNanometers(const AInches: Double): Double;
begin
  Result:= MillimetersToNanometers(InchesToMillimeters(AInches));
end;

class function TUOMLengthUtils.NanometersToCentimeters(const ANanometers: Double): Double;
begin
  Result:= MillimetersToCentimeters(NanometersToMillimeters(ANanometers));
end;

class function TUOMLengthUtils.NanometersToFeet(const ANanometers: Double): Double;
begin
  Result:= MillimetersToFeet(NanometersToMillimeters(ANanometers));
end;

class function TUOMLengthUtils.NanometersToMeters(const ANanometers: Double): Double;
begin
  Result:= MillimetersToMeters(NanometersToMillimeters(ANanometers));
end;

class function TUOMLengthUtils.NanometersToKilometers(const ANanometers: Double): Double;
begin
  Result:= MetersToKilometers(NanometersToMeters(ANanometers));
end;

class function TUOMLengthUtils.CentimetersToNanometers(const ACentimeters: Double): Double;
begin
  Result:= MillimetersToNanometers(CentimetersToMillimeters(ACentimeters));
end;

class function TUOMLengthUtils.MetersToNanometers(const AMeters: Double): Double;
begin
  Result:= CentimetersToNanometers(MetersToCentimeters(AMeters));
end;

class function TUOMLengthUtils.MillimetersToNanometers(const AMillimeters: Double): Double;
begin
  Result:= MicronsToNanometers(MillimetersToMicrons(AMillimeters));
end;

class function TUOMLengthUtils.MetersToMicrons(const AMeters: Double): Double;
begin
  Result:= MillimetersToMicrons(MetersToMillimeters(AMeters));
end;

class function TUOMLengthUtils.CentimetersToKilometers(const ACentimeters: Double): Double;
begin
  Result:= MetersToKilometers(CentimetersToMeters(ACentimeters));
end;

class function TUOMLengthUtils.NanometersToYards(const ANanometers: Double): Double;
begin
  Result:= InchesToYards(NanometersToInches(ANanometers));
end;

class function TUOMLengthUtils.MillimetersToKilometers(const AMillimeters: Double): Double;
begin
  Result:= MetersToKilometers(MillimetersToMeters(AMillimeters));
end;

class function TUOMLengthUtils.KilometersToMicrons(const AKilometers: Double): Double;
begin
  Result:= CentimetersToMicrons(KilometersToCentimeters(AKilometers));
end;

class function TUOMLengthUtils.KilometersToNanometers(const AKilometers: Double): Double;
begin
  Result:= CentimetersToNanometers(KilometersToCentimeters(AKilometers));
end;

class function TUOMLengthUtils.MicronsToCentimeters(const AMicrons: Double): Double;
begin
  Result:= MillimetersToCentimeters(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToFeet(const AMicrons: Double): Double;
begin
  Result:= MillimetersToFeet(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToInches(const AMicrons: Double): Double;
begin
  Result:= MillimetersToInches(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToKilometers(const AMicrons: Double): Double;
begin
  Result:= MillimetersToKilometers(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToMeters(const AMicrons: Double): Double;
begin
  Result:= MillimetersToMeters(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToYards(const AMicrons: Double): Double;
begin
  Result:= MillimetersToYards(MicronsToMillimeters(AMicrons));
end;

class function TUOMLengthUtils.MicronsToMiles(const AMicrons: Double): Double;
begin
  Result:= InchesToYards(MicronsToInches(AMicrons));
end;

class function TUOMLengthUtils.FeetToNanometers(const AFeet: Double): Double;
begin
  Result:= InchesToNanometers(FeetToInches(AFeet));
end;

class function TUOMLengthUtils.YardsToNanometers(const AYards: Double): Double;
begin
  Result:= InchesToNanometers(YardsToInches(AYards));
end;

class function TUOMLengthUtils.InchesToKilometers(const AInches: Double): Double;
begin
  Result:= CentimetersToKilometers(InchesToCentimeters(AInches));
end;

class function TUOMLengthUtils.CentimetersToNauticalMiles(const ACentimeters: Double): Double;
begin
  Result:= KilometersToNauticalMiles(CentimetersToKilometers(ACentimeters));
end;

class function TUOMLengthUtils.MicronsToNauticalMiles(const AMicrons: Double): Double;
begin
  Result:= KilometersToNauticalMiles(MicronsToKilometers(AMicrons));
end;

class function TUOMLengthUtils.MillimetersToMiles(const AMillimeters: Double): Double;
begin
  Result:= KilometersToMiles(MillimetersToKilometers(AMillimeters));
end;

class function TUOMLengthUtils.MillimetersToNauticalMiles(const AMillimeters: Double): Double;
begin
  Result:= KilometersToNauticalMiles(MillimetersToKilometers(AMillimeters));
end;

class function TUOMLengthUtils.CentimetersToMiles(const ACentimeters: Double): Double;
begin
  Result:= KilometersToMiles(CentimetersToKilometers(ACentimeters));
end;

class function TUOMLengthUtils.NanometersToMiles(const ANanometers: Double): Double;
begin
  Result:= KilometersToMiles(NanometersToKilometers(ANanometers));
end;

class function TUOMLengthUtils.NanometersToNauticalMiles(const ANanometers: Double): Double;
begin
  Result:= KilometersToNauticalMiles(NanometersToKilometers(ANanometers));
end;

class function TUOMLengthUtils.NauticalMilesToMillimeters(const ANauticalMiles: Double): Double;
begin
  Result:= KilometersToMillimeters(NauticalMilesToKilometers(ANauticalMiles));
end;

class function TUOMLengthUtils.NauticalMilesToMicrons(const ANauticalMiles: Double): Double;
begin
  Result:= KilometersToMicrons(NauticalMilesToKilometers(ANauticalMiles));
end;

class function TUOMLengthUtils.NauticalMilesToNanometers(const ANauticalMiles: Double): Double;
begin
  Result:= KilometersToNanometers(NauticalMilesToKilometers(ANauticalMiles));
end;

class function TUOMLengthUtils.MilesToMicrons(const AMiles: Double): Double;
begin
  Result:= KilometersToMicrons(MilesToKilometers(AMiles));
end;

class function TUOMLengthUtils.MilesToMillimeters(const AMiles: Double): Double;
begin
  Result:= KilometersToMillimeters(MilesToKilometers(AMiles));
end;

class function TUOMLengthUtils.MilesToNanometers(const AMiles: Double): Double;
begin
  Result:= KilometersToNanometers(MilesToKilometers(AMiles));
end;

class function TUOMLengthUtils.InchesToNauticalMiles(const AInches: Double): Double;
begin
  Result:= KilometersToMiles(InchesToKilometers(AInches));
end;

{ TUOMLength }

class operator TUOMLength.implicit(const AValue: Double): TUOMLength;
begin
  Result.FUnit:= DefaultLengthUnit;
  Result.FValue:= AValue;
end;

class operator TUOMLength.implicit(const AValue: TUOMLength): Double;
begin
  Result:= 0;
  case DefaultLengthUnit of
    umlNanometers:    Result:= AValue.GetAsNanometers;
    umlMicrons:       Result:= AValue.GetAsMicrons;
    umlMillimeters:   Result:= AValue.GetAsMillimeters;
    umlCentimeters:   Result:= AValue.GetAsCentimeters;
    umlMeters:        Result:= AValue.GetAsMeters;
    umlKilometers:    Result:= AValue.GetAsKilometers;
    umlInches:        Result:= AValue.GetAsInches;
    umlFeet:          Result:= AValue.GetAsFeet;
    umlYards:         Result:= AValue.GetAsYards;
    umlMiles:         Result:= AValue.GetAsMiles;
    umlNauticalMiles: Result:= AValue.GetAsNauticalMiles;
  end;
end;

class operator TUOMLength.implicit(const AValue: TUOMLength): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  Result:= Result + TUOMLengthUtils.UnitSuffix(AValue.&Unit);
end;

class operator TUOMLength.implicit(const AValue: String): TUOMLength;
begin
  //TODO: Parse string...

end;

class operator TUOMLength.Equal(const A, B: TUOMLength): Boolean;
begin
  //TODO: Use Epsilon?
  Result:= System.Math.CompareValue(A.GetAsMeters, B.GetAsMeters) = EqualsValue;
end;

class operator TUOMLength.GreaterThan(const A, B: TUOMLength): Boolean;
begin
  //TODO: Use Epsilon?
  Result:= System.Math.CompareValue(A.GetAsMeters, B.GetAsMeters) = GreaterThanValue;
end;

class operator TUOMLength.LessThan(const A, B: TUOMLength): Boolean;
begin
  //TODO: Use Epsilon?
  Result:= System.Math.CompareValue(A.GetAsMeters, B.GetAsMeters) = LessThanValue;
end;

class operator TUOMLength.Add(const A, B: TUOMLength): TUOMLength;
begin
  //TODO: Will this work?
  Result.&Unit:= TUOMLengthUnit.umlMeters;
  Result.Value:= System.Math.Sum([A.GetAsMeters,B.GetAsMeters]);
  Result.&Unit:= A.&Unit;
end;

class operator TUOMLength.Divide(const A, B: TUOMLength): TUOMLength;
begin
  //TODO: Will this work?
  Result.&Unit:= TUOMLengthUnit.umlMeters;
  Result.Value:= A.GetAsMeters / B.GetAsMeters;
  Result.&Unit:= A.&Unit;
end;

class operator TUOMLength.Multiply(const A, B: TUOMLength): TUOMLength;
begin
  //TODO: Will this work?
  Result.&Unit:= TUOMLengthUnit.umlMeters;
  Result.Value:= A.GetAsMeters * B.GetAsMeters;
  Result.&Unit:= A.&Unit;
end;

class operator TUOMLength.Subtract(const A, B: TUOMLength): TUOMLength;
begin
  //TODO: Will this work?
  Result.&Unit:= TUOMLengthUnit.umlMeters;
  Result.Value:= A.GetAsMeters - B.GetAsMeters;
  Result.&Unit:= A.&Unit;
end;

procedure TUOMLength.SetValue(const Value: Double);
begin
  //Unit of FValue depends on value of FUnit.
  FValue := Value;
end;

procedure TUOMLength.SetUnit(const Value: TUOMLengthUnit);
begin
  //First convert the value from its current unit to the new unit
  case Value of
    umlNanometers:    FValue:= Self.GetAsNanometers;
    umlMicrons:       FValue:= Self.GetAsMicrons;
    umlMillimeters:   FValue:= Self.GetAsMillimeters;
    umlCentimeters:   FValue:= Self.GetAsCentimeters;
    umlMeters:        FValue:= Self.GetAsMeters;
    umlKilometers:    FValue:= Self.GetAsKilometers;
    umlInches:        FValue:= Self.GetAsInches;
    umlFeet:          FValue:= Self.GetAsFeet;
    umlYards:         FValue:= Self.GetAsYards;
    umlMiles:         FValue:= Self.GetAsMiles;
    umlNauticalMiles: FValue:= Self.GetAsNauticalMiles;
  end;
  //Now update to the new unit
  FUnit:= Value;
end;

function TUOMLength.GetAsNanometers: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= FValue; //Same
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToNanometers(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToNanometers(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToNanometers(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToNanometers(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToNanometers(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToNanometers(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToNanometers(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToNanometers(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToNanometers(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToNanometers(FValue);
  end;
end;

function TUOMLength.GetAsMicrons: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToMicrons(FValue);
    umlMicrons:       Result:= FValue; //Same
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToMicrons(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToMicrons(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToMicrons(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToMicrons(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToMicrons(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToMicrons(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToMicrons(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToMicrons(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToMicrons(FValue);
  end;
end;

function TUOMLength.GetAsMillimeters: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToMillimeters(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToMillimeters(FValue);
    umlMillimeters:   Result:= FValue; //Same
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToMillimeters(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToMillimeters(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToMillimeters(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToMillimeters(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToMillimeters(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToMillimeters(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToMillimeters(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToMillimeters(FValue);
  end;
end;

function TUOMLength.GetAsCentimeters: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToCentimeters(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToCentimeters(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToCentimeters(FValue);
    umlCentimeters:   Result:= FValue; //Same
    umlMeters:        Result:= TUOMLengthUtils.MetersToCentimeters(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToCentimeters(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToCentimeters(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToCentimeters(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToCentimeters(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToCentimeters(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToCentimeters(FValue);
  end;
end;

function TUOMLength.GetAsMeters: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToMeters(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToMeters(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToMeters(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToMeters(FValue);
    umlMeters:        Result:= FValue; //Same
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToMeters(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToMeters(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToMeters(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToMeters(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToMeters(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToMeters(FValue);
  end;
end;

function TUOMLength.GetAsKilometers: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToKilometers(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToKilometers(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToKilometers(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToKilometers(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToKilometers(FValue);
    umlKilometers:    Result:= FValue; //Same
    umlInches:        Result:= TUOMLengthUtils.InchesToKilometers(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToKilometers(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToKilometers(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToKilometers(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToKilometers(FValue);
  end;
end;

function TUOMLength.GetAsInches: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToInches(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToInches(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToInches(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToInches(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToInches(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToInches(FValue);
    umlInches:        Result:= FValue; //Same
    umlFeet:          Result:= TUOMLengthUtils.FeetToInches(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToInches(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToInches(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToInches(FValue);
  end;
end;

function TUOMLength.GetAsFeet: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToFeet(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToFeet(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToFeet(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToFeet(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToFeet(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToFeet(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToFeet(FValue);
    umlFeet:          Result:= FValue; //Same
    umlYards:         Result:= TUOMLengthUtils.YardsToFeet(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToFeet(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToFeet(FValue);
  end;
end;

function TUOMLength.GetAsYards: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToYards(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToYards(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToYards(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToYards(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToYards(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToYards(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToYards(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToYards(FValue);
    umlYards:         Result:= FValue; //Same
    umlMiles:         Result:= TUOMLengthUtils.MilesToYards(FValue);
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToYards(FValue);
  end;
end;

function TUOMLength.GetAsMiles: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToMiles(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToMiles(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToMiles(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToMiles(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToMiles(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToMiles(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToMiles(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToMiles(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToMiles(FValue);
    umlMiles:         Result:= FValue; //Same
    umlNauticalMiles: Result:= TUOMLengthUtils.NauticalMilesToMiles(FValue);
  end;
end;

function TUOMLength.GetAsNauticalMiles: Double;
begin
  Result:= 0;
  case FUnit of
    umlNanometers:    Result:= TUOMLengthUtils.NanometersToNauticalMiles(FValue);
    umlMicrons:       Result:= TUOMLengthUtils.MicronsToNauticalMiles(FValue);
    umlMillimeters:   Result:= TUOMLengthUtils.MillimetersToNauticalMiles(FValue);
    umlCentimeters:   Result:= TUOMLengthUtils.CentimetersToNauticalMiles(FValue);
    umlMeters:        Result:= TUOMLengthUtils.MetersToNauticalMiles(FValue);
    umlKilometers:    Result:= TUOMLengthUtils.KilometersToNauticalMiles(FValue);
    umlInches:        Result:= TUOMLengthUtils.InchesToNauticalMiles(FValue);
    umlFeet:          Result:= TUOMLengthUtils.FeetToNauticalMiles(FValue);
    umlYards:         Result:= TUOMLengthUtils.YardsToNauticalMiles(FValue);
    umlMiles:         Result:= TUOMLengthUtils.MilesToNauticalMiles(FValue);
    umlNauticalMiles: Result:= FValue; //Same
  end;
end;

procedure TUOMLength.SetAsCentimeters(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.CentimetersToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.CentimetersToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.CentimetersToMillimeters(Value);
    umlCentimeters:   FValue:= Value;
    umlMeters:        FValue:= TUOMLengthUtils.CentimetersToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.CentimetersToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.CentimetersToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.CentimetersToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.CentimetersToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.CentimetersToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.CentimetersToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsFeet(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.FeetToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.FeetToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.FeetToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.FeetToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.FeetToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.FeetToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.FeetToInches(Value);
    umlFeet:          FValue:= Value;
    umlYards:         FValue:= TUOMLengthUtils.FeetToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.FeetToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.FeetToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsInches(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.InchesToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.InchesToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.InchesToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.InchesToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.InchesToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.InchesToKilometers(Value);
    umlInches:        FValue:= Value;
    umlFeet:          FValue:= TUOMLengthUtils.InchesToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.InchesToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.InchesToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.InchesToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsKilometers(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.KilometersToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.KilometersToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.KilometersToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.KilometersToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.KilometersToMeters(Value);
    umlKilometers:    FValue:= Value;
    umlInches:        FValue:= TUOMLengthUtils.KilometersToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.KilometersToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.KilometersToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.KilometersToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.KilometersToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsMeters(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.MetersToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.MetersToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.MetersToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.MetersToCentimeters(Value);
    umlMeters:        FValue:= Value;
    umlKilometers:    FValue:= TUOMLengthUtils.MetersToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.KilometersToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.KilometersToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.KilometersToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.KilometersToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.KilometersToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsMicrons(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.MicronsToNanometers(Value);
    umlMicrons:       FValue:= Value;
    umlMillimeters:   FValue:= TUOMLengthUtils.MicronsToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.MicronsToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.MicronsToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.MicronsToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.MicronsToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.MicronsToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.MicronsToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.MicronsToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.MicronsToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsMiles(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.MilesToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.MilesToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.MilesToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.MilesToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.MilesToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.MilesToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.MilesToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.MilesToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.MilesToYards(Value);
    umlMiles:         FValue:= Value;
    umlNauticalMiles: FValue:= TUOMLengthUtils.MilesToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsMillimeters(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.MillimetersToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.MillimetersToMicrons(Value);
    umlMillimeters:   FValue:= Value;
    umlCentimeters:   FValue:= TUOMLengthUtils.MillimetersToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.MillimetersToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.MillimetersToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.MillimetersToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.MillimetersToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.MillimetersToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.MillimetersToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.MillimetersToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsNanometers(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= Value;
    umlMicrons:       FValue:= TUOMLengthUtils.NanometersToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.NanometersToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.NanometersToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.NanometersToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.NanometersToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.NanometersToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.NanometersToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.NanometersToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.NanometersToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.NanometersToNauticalMiles(Value);
  end;
end;

procedure TUOMLength.SetAsNauticalMiles(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.NauticalMilesToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.NauticalMilesToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.NauticalMilesToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.NauticalMilesToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.NauticalMilesToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.NauticalMilesToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.NauticalMilesToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.NauticalMilesToFeet(Value);
    umlYards:         FValue:= TUOMLengthUtils.NauticalMilesToYards(Value);
    umlMiles:         FValue:= TUOMLengthUtils.NauticalMilesToMiles(Value);
    umlNauticalMiles: FValue:= Value;
  end;
end;

procedure TUOMLength.SetAsYards(const Value: Double);
begin
  case FUnit of
    umlNanometers:    FValue:= TUOMLengthUtils.YardsToNanometers(Value);
    umlMicrons:       FValue:= TUOMLengthUtils.YardsToMicrons(Value);
    umlMillimeters:   FValue:= TUOMLengthUtils.YardsToMillimeters(Value);
    umlCentimeters:   FValue:= TUOMLengthUtils.YardsToCentimeters(Value);
    umlMeters:        FValue:= TUOMLengthUtils.YardsToMeters(Value);
    umlKilometers:    FValue:= TUOMLengthUtils.YardsToKilometers(Value);
    umlInches:        FValue:= TUOMLengthUtils.YardsToInches(Value);
    umlFeet:          FValue:= TUOMLengthUtils.YardsToFeet(Value);
    umlYards:         FValue:= Value;
    umlMiles:         FValue:= TUOMLengthUtils.YardsToMiles(Value);
    umlNauticalMiles: FValue:= TUOMLengthUtils.YardsToNauticalMiles(Value);
  end;
end;

{ TUOMLengthMatrix }

class function TUOMLengthMatrix.Convert(const A, B: TUOMLengthUnit;
  const Val: Double): Double;
begin

end;

initialization
  DefaultLengthUnit:= TUOMLengthUnit.umlFeet;
end.
