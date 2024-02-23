unit JD.Uom.Length;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  JD.Uom;

type
  TUOMLengthUnit = (umlNanometers, umlMicrons, umlMillimeters, umlCentimeters,
    umlMeters, umlKilometers, umlInches, umlFeet, umlYards, umlMiles, umlNauticalMiles);
  TUOMLengthUnits = set of TUOMLengthUnit;


  TUOMLengthUtils = class(TUOMBase)
  private
    class var FUnits: TUOMUnitArray;
    class procedure RegisterUOM;
    class procedure RegisterUnits;
    class procedure RegisterUnit(const Name: String; const Systems: TUOMSystems;
      const Prefix, Suffix: String);
  public
    class constructor Create;
    class destructor Destroy;

    class function UOMID: String; override;
    class function UOMName: String; override;
    class function UnitCount: Integer; override;
    class function GetUnit(const Index: Integer): TUOMUnitInfo; override;

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
    function GetAsNanometers: TUOMLength;
    function GetAsMicrons: TUOMLength;
    function GetAsMillimeters: TUOMLength;
    function GetAsCentimeters: TUOMLength;
    function GetAsMeters: TUOMLength;
    function GetAsKilometers: TUOMLength;
    function GetAsInches: TUOMLength;
    function GetAsFeet: TUOMLength;
    function GetAsYards: TUOMLength;
    function GetAsMiles: TUOMLength;
    function GetAsNauticalMiles: TUOMLength;
    procedure SetAsCentimeters(const Value: TUOMLength);
    procedure SetAsFeet(const Value: TUOMLength);
    procedure SetAsInches(const Value: TUOMLength);
    procedure SetAsKilometers(const Value: TUOMLength);
    procedure SetAsMeters(const Value: TUOMLength);
    procedure SetAsMicrons(const Value: TUOMLength);
    procedure SetAsMiles(const Value: TUOMLength);
    procedure SetAsMillimeters(const Value: TUOMLength);
    procedure SetAsNanometers(const Value: TUOMLength);
    procedure SetAsNauticalMiles(const Value: TUOMLength);
    procedure SetAsYards(const Value: TUOMLength);
  public
    property AsNanometers: TUOMLength read GetAsNanometers write SetAsNanometers;
    property AsMicrons: TUOMLength read GetAsMicrons write SetAsMicrons;
    property AsMillimeters: TUOMLength read GetAsMillimeters write SetAsMillimeters;
    property AsCentimeters: TUOMLength read GetAsCentimeters write SetAsCentimeters;
    property AsMeters: TUOMLength read GetAsMeters write SetAsMeters;
    property AsKilometers: TUOMLength read GetAsKilometers write SetAsKilometers;
    property AsInches: TUOMLength read GetAsInches write SetAsInches;
    property AsFeet: TUOMLength read GetAsFeet write SetAsFeet;
    property AsYards: TUOMLength read GetAsYards write SetAsYards;
    property AsMiles: TUOMLength read GetAsMiles write SetAsMiles;
    property AsNauticalMiles: TUOMLength read GetAsNauticalMiles write SetAsNauticalMiles;
  end;

implementation

uses
  System.Math;

var
  DefaultLengthUnit: TUOMLengthUnit;
  _: TUOMLengthUtils;

{ TUOMLengthUtils }

class constructor TUOMLengthUtils.Create;
begin
  SetLength(FUnits, 0);
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMLengthUtils.Destroy;
begin
  SetLength(FUnits, 0);
end;

class function TUOMLengthUtils.UOMID: String;
begin
  Result:= '{B58003EA-5EDD-494F-87D5-94870AB31D49}';
end;

class function TUOMLengthUtils.UOMName: String;
begin
  Result:= 'Length';
end;

class procedure TUOMLengthUtils.RegisterUOM;
begin
  TUOMList.RegisterUOM(TUOMLengthUtils);
end;

class procedure TUOMLengthUtils.RegisterUnit(const Name: String;
  const Systems: TUOMSystems; const Prefix, Suffix: String);
var
  U: TUOMUnitInfo;
begin
  SetLength(FUnits, Length(FUnits)+1);
  U.UOM:= Self;
  U.Name:= Name;
  U.Systems:= Systems;
  U.Prefix:= Prefix;
  U.Suffix:= Suffix;
  FUnits[Length(FUnits)-1]:= U;
end;

class function TUOMLengthUtils.UnitCount: Integer;
begin
  Result:= Length(FUnits);
end;

class procedure TUOMLengthUtils.RegisterUnits;
begin
  RegisterUnit('Nanometers',     [ustMetric],  '',   'nm');
  RegisterUnit('Microns',        [ustMetric],  '',   'μm');
  RegisterUnit('Millimeters',    [ustMetric],  '',   'mm');
  RegisterUnit('Centimeters',    [ustMetric],  '',   'cm');
  RegisterUnit('Meters',         [ustMetric],  '',   'm');
  RegisterUnit('Kilometers',     [ustMetric],  '',   'km');
  RegisterUnit('Inches',         [ustImperial,ustUSCustomary],  '',   '"');
  RegisterUnit('Feet',           [ustImperial,ustUSCustomary],  '',   '''');
  RegisterUnit('Yards',          [ustImperial,ustUSCustomary],  '',   'yd');
  RegisterUnit('Miles',          [ustImperial,ustUSCustomary],  '',   'mi');
  RegisterUnit('Nautical Mies',  [ustImperial,ustUSCustomary],  '',   'nmi');
end;

{

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

}

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

class function TUOMLengthUtils.GetUnit(const Index: Integer): TUOMUnitInfo;
begin
  Result:= FUnits[Index];
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
    umlNanometers:    Result:= AValue.GetAsNanometers.Value;
    umlMicrons:       Result:= AValue.GetAsMicrons.Value;
    umlMillimeters:   Result:= AValue.GetAsMillimeters.Value;
    umlCentimeters:   Result:= AValue.GetAsCentimeters.Value;
    umlMeters:        Result:= AValue.GetAsMeters.Value;
    umlKilometers:    Result:= AValue.GetAsKilometers.Value;
    umlInches:        Result:= AValue.GetAsInches.Value;
    umlFeet:          Result:= AValue.GetAsFeet.Value;
    umlYards:         Result:= AValue.GetAsYards.Value;
    umlMiles:         Result:= AValue.GetAsMiles.Value;
    umlNauticalMiles: Result:= AValue.GetAsNauticalMiles.Value;
  end;
end;

class operator TUOMLength.implicit(const AValue: TUOMLength): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  Result:= Result + TUOMLengthUtils.UnitSuffix(Integer(AValue.&Unit));
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

function TUOMLength.GetAsNanometers: TUOMLength;
begin
  Result.FUnit:= umlNanometers;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= FValue; //Same
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToNanometers(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToNanometers(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToNanometers(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToNanometers(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToNanometers(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToNanometers(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToNanometers(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToNanometers(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToNanometers(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToNanometers(FValue);
  end;
end;

function TUOMLength.GetAsMicrons: TUOMLength;
begin
  Result.FUnit:= umlMicrons;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToMicrons(FValue);
    umlMicrons:       Result.FValue:= FValue; //Same
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToMicrons(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToMicrons(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToMicrons(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToMicrons(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToMicrons(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToMicrons(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToMicrons(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToMicrons(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToMicrons(FValue);
  end;
end;

function TUOMLength.GetAsMillimeters: TUOMLength;
begin
  Result.FUnit:= umlMillimeters;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToMillimeters(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToMillimeters(FValue);
    umlMillimeters:   Result.FValue:= FValue; //Same
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToMillimeters(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToMillimeters(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToMillimeters(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToMillimeters(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToMillimeters(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToMillimeters(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToMillimeters(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToMillimeters(FValue);
  end;
end;

function TUOMLength.GetAsCentimeters: TUOMLength;
begin
  Result.FUnit:= umlCentimeters;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToCentimeters(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToCentimeters(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToCentimeters(FValue);
    umlCentimeters:   Result.FValue:= FValue; //Same
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToCentimeters(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToCentimeters(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToCentimeters(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToCentimeters(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToCentimeters(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToCentimeters(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToCentimeters(FValue);
  end;
end;

function TUOMLength.GetAsMeters: TUOMLength;
begin
  Result.FUnit:= umlMeters;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToMeters(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToMeters(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToMeters(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToMeters(FValue);
    umlMeters:        Result.FValue:= FValue; //Same
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToMeters(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToMeters(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToMeters(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToMeters(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToMeters(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToMeters(FValue);
  end;
end;

function TUOMLength.GetAsKilometers: TUOMLength;
begin
  Result.FUnit:= umlKilometers;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToKilometers(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToKilometers(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToKilometers(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToKilometers(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToKilometers(FValue);
    umlKilometers:    Result.FValue:= FValue; //Same
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToKilometers(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToKilometers(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToKilometers(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToKilometers(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToKilometers(FValue);
  end;
end;

function TUOMLength.GetAsInches: TUOMLength;
begin
  Result.FUnit:= umlInches;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToInches(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToInches(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToInches(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToInches(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToInches(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToInches(FValue);
    umlInches:        Result.FValue:= FValue; //Same
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToInches(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToInches(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToInches(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToInches(FValue);
  end;
end;

function TUOMLength.GetAsFeet: TUOMLength;
begin
  Result.FUnit:= umlFeet;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToFeet(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToFeet(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToFeet(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToFeet(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToFeet(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToFeet(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToFeet(FValue);
    umlFeet:          Result.FValue:= FValue; //Same
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToFeet(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToFeet(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToFeet(FValue);
  end;
end;

function TUOMLength.GetAsYards: TUOMLength;
begin
  Result.FUnit:= umlYards;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToYards(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToYards(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToYards(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToYards(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToYards(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToYards(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToYards(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToYards(FValue);
    umlYards:         Result.FValue:= FValue; //Same
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToYards(FValue);
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToYards(FValue);
  end;
end;

function TUOMLength.GetAsMiles: TUOMLength;
begin
  Result.FUnit:= umlMiles;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToMiles(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToMiles(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToMiles(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToMiles(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToMiles(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToMiles(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToMiles(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToMiles(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToMiles(FValue);
    umlMiles:         Result.FValue:= FValue; //Same
    umlNauticalMiles: Result.FValue:= TUOMLengthUtils.NauticalMilesToMiles(FValue);
  end;
end;

function TUOMLength.GetAsNauticalMiles: TUOMLength;
begin
  Result.FUnit:= umlNauticalMiles;
  Result.FValue:= 0;
  case FUnit of
    umlNanometers:    Result.FValue:= TUOMLengthUtils.NanometersToNauticalMiles(FValue);
    umlMicrons:       Result.FValue:= TUOMLengthUtils.MicronsToNauticalMiles(FValue);
    umlMillimeters:   Result.FValue:= TUOMLengthUtils.MillimetersToNauticalMiles(FValue);
    umlCentimeters:   Result.FValue:= TUOMLengthUtils.CentimetersToNauticalMiles(FValue);
    umlMeters:        Result.FValue:= TUOMLengthUtils.MetersToNauticalMiles(FValue);
    umlKilometers:    Result.FValue:= TUOMLengthUtils.KilometersToNauticalMiles(FValue);
    umlInches:        Result.FValue:= TUOMLengthUtils.InchesToNauticalMiles(FValue);
    umlFeet:          Result.FValue:= TUOMLengthUtils.FeetToNauticalMiles(FValue);
    umlYards:         Result.FValue:= TUOMLengthUtils.YardsToNauticalMiles(FValue);
    umlMiles:         Result.FValue:= TUOMLengthUtils.MilesToNauticalMiles(FValue);
    umlNauticalMiles: Result.FValue:= FValue; //Same
  end;
end;

procedure TUOMLength.SetAsCentimeters(const Value: TUOMLength);
begin
  FUnit:= umlCentimeters;
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

procedure TUOMLength.SetAsFeet(const Value: TUOMLength);
begin
  FUnit:= umlFeet;
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

procedure TUOMLength.SetAsInches(const Value: TUOMLength);
begin
  FUnit:= umlInches;
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

procedure TUOMLength.SetAsKilometers(const Value: TUOMLength);
begin
  FUnit:= umlKilometers;
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

procedure TUOMLength.SetAsMeters(const Value: TUOMLength);
begin
  FUnit:= umlMeters;
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

procedure TUOMLength.SetAsMicrons(const Value: TUOMLength);
begin
  FUnit:= umlMicrons;
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

procedure TUOMLength.SetAsMiles(const Value: TUOMLength);
begin
  FUnit:= umlMiles;
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

procedure TUOMLength.SetAsMillimeters(const Value: TUOMLength);
begin
  FUnit:= umlMillimeters;
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

procedure TUOMLength.SetAsNanometers(const Value: TUOMLength);
begin
  FUnit:= umlNanometers;
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

procedure TUOMLength.SetAsNauticalMiles(const Value: TUOMLength);
begin
  FUnit:= umlNauticalMiles;
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

procedure TUOMLength.SetAsYards(const Value: TUOMLength);
begin
  FUnit:= umlYards;
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


initialization
  _:= nil;
  DefaultLengthUnit:= TUOMLengthUnit.umlFeet;
end.
