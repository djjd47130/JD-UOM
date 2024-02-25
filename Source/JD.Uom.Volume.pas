unit JD.Uom.Volume;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Length,
  JD.Uom.Area;

type
  TUOMVolumeUnit = (umvMilliliters, umvCubicCentimeters, umvLiters, umvCubicMeters,
    umvTeaSpoonsUS, umvTableSpoonsUS, umvFluidOuncesUS, umvTeaSpoonsUK,
    umvTableSpoonsUK, umvFluidOuncesUK, umvCups, umvPints, umvQuarts,
    umvGallons, umvCubicInches, umvCubicFeet, umvCubicYards);
  TUOMVolumeUnits = set of TUOMVolumeUnit;

  TUOMVolumeUtils = class;

  TUOMVolumeUnitBase = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMVolumeUnit; virtual; abstract;
  end;

  //Specific Volume Units

  TUOMVolumeMilliliters = class(TUOMVolumeUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMVolumeUnit; override;
  end;

  TUOMVolumeCubicCentimeters = class(TUOMVolumeUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMVolumeUnit; override;
  end;

  TUOMVolumeLiters = class(TUOMVolumeUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMVolumeUnit; override;
  end;

  //TODO

  {
  AList.Append('Cubic Meters');
  AList.Append('Teaspoons (US)');
  AList.Append('Tablespoons (US)');
  AList.Append('Fluid Ounces (US)');
  AList.Append('Teaspoons (UK)');
  AList.Append('Tablespoons (UK)');
  AList.Append('Fluid Ounces (UK)');
  AList.Append('Cups');
  AList.Append('Pints');
  AList.Append('Quarts');
  AList.Append('Gallons');
  AList.Append('Cubic Inches');
  AList.Append('Cubic Feet');
  AList.Append('Cubic Yards');
  }

  TUOMVolumeUtils = class(TUOMBase)
  private
    class var FUnits: TList<TUOMUnitBaseClass>;
    class procedure RegisterUOM;
    class procedure RegisterUnits;
    class procedure RegisterUnit(AUnitClass: TUOMUnitBaseClass);
  public
    class constructor Create;
    class destructor Destroy;
    class function UOMID: String; override;
    class function UOMName: String; override;
    class function UnitCount: Integer; override;
    class function GetUnit(const Index: Integer): TUOMUnitBaseClass; override;

    { Metric System }

    //Milliliters
    class function CubicCentimetersToMilliliters(const ACubicCentimeters: Double): Double; static;
    class function LitersToMilliliters(const ALiters: Double): Double; static;
    class function CubicMetersToMilliliters(const ACubicMeters: Double): Double; static;
    class function USTeaspoonsToMilliliters(const ATeaspoons: Double): Double; static;
    class function USTablespoonsToMilliliters(const ATablespoons: Double): Double; static;
    class function USFluidOuncesToMilliliters(const AFluidOunces: Double): Double; static;
    class function UKTeaspoonsToMilliliters(const ATeaspoons: Double): Double; static;
    class function UKTablespoonsToMilliliters(const ATablespoons: Double): Double; static;
    class function UKFluidOuncesToMilliliters(const AFluidOunces: Double): Double; static;
    class function CupsToMilliliters(const ACups: Double): Double; static;
    class function PintsToMilliliters(const APints: Double): Double; static;
    class function QuartsToMilliliters(const AQuarts: Double): Double; static;
    class function GallonsToMilliliters(const AGallons: Double): Double; static;
    class function CubicInchesToMilliliters(const ACubicInches: Double): Double; static;
    class function CubicFeetToMilliliters(const ACubicFeet: Double): Double; static;
    class function CubicYardsToMilliliters(const ACubicYards: Double): Double; static;
    //Cubic Centimeters
    class function MillilitersToCubicCentimeters(const AMilliliters: Double): Double; static;
    class function LitersToCubicCentimeters(const ALiters: Double): Double; static;
    class function CubicMetersToCubicCentimeters(const ACubicMeters: Double): Double; static;
    class function USTeaspoonsToCubicCentimeters(const ATeaspoons: Double): Double; static;
    class function USTablespoonsToCubicCentimeters(const ATablespoons: Double): Double; static;
    class function USFluidOuncesToCubicCentimeters(const AFluidOunces: Double): Double; static;
    class function UKTeaspoonsToCubicCentimeters(const ATeaspoons: Double): Double; static;
    class function UKTablespoonsToCubicCentimeters(const ATablespoons: Double): Double; static;
    class function UKFluidOuncesToCubicCentimeters(const AFluidOunces: Double): Double; static;
    class function CupsToCubicCentimeters(const ACups: Double): Double; static;
    class function PintsToCubicCentimeters(const APints: Double): Double; static;
    class function QuartsToCubicCentimeters(const AQuarts: Double): Double; static;
    class function GallonsToCubicCentimeters(const AGallons: Double): Double; static;
    class function CubicInchesToCubicCentimeters(const ACubicInches: Double): Double; static;
    class function CubicFeetToCubicCentimeters(const ACubicFeet: Double): Double; static;
    class function CubicYardsToCubicCentimeters(const ACubicYards: Double): Double; static;
    //Liters
    class function MillilitersToLiters(const AMilliliters: Double): Double; static;
    class function CubicCentimetersToLiters(const ACubicCentimeters: Double): Double; static;
    class function CubicMetersToLiters(const ACubicMeters: Double): Double; static;
    class function USTeaspoonsToLiters(const ATeaspoons: Double): Double; static;
    class function USTablespoonsToLiters(const ATablespoons: Double): Double; static;
    class function USFluidOuncesToLiters(const AFluidOunces: Double): Double; static;
    class function UKTeaspoonsToLiters(const ATeaspoons: Double): Double; static;
    class function UKTablespoonsToLiters(const ATablespoons: Double): Double; static;
    class function UKFluidOuncesToLiters(const AFluidOunces: Double): Double; static;
    class function CupsToLiters(const ACups: Double): Double; static;
    class function PintsToLiters(const APints: Double): Double; static;
    class function QuartsToLiters(const AQuarts: Double): Double; static;
    class function GallonsToLiters(const AGallons: Double): Double; static;
    class function CubicInchesToLiters(const ACubicInches: Double): Double; static;
    class function CubicFeetToLiters(const ACubicFeet: Double): Double; static;
    class function CubicYardsToLiters(const ACubicYards: Double): Double; static;
    //Cubic Meters
    class function MillilitersToCubicMeters(const AMilliliters: Double): Double; static;
    class function CubicCentimetersToCubicMeters(const ACubicCentimeters: Double): Double; static;
    class function CubicMetersToCubicMeters(const ACubicMeters: Double): Double; static;
    class function USTeaspoonsToCubicMeters(const ATeaspoons: Double): Double; static;
    class function USTablespoonsToCubicMeters(const ATablespoons: Double): Double; static;
    class function USFluidOuncesToCubicMeters(const AFluidOunces: Double): Double; static;
    class function UKTeaspoonsToCubicMeters(const ATeaspoons: Double): Double; static;
    class function UKTablespoonsToCubicMeters(const ATablespoons: Double): Double; static;
    class function UKFluidOuncesToCubicMeters(const AFluidOunces: Double): Double; static;
    class function CupsToCubicMeters(const ACups: Double): Double; static;
    class function PintsToCubicMeters(const APints: Double): Double; static;
    class function QuartsToCubicMeters(const AQuarts: Double): Double; static;
    class function GallonsToCubicMeters(const AGallons: Double): Double; static;
    class function CubicInchesToCubicMeters(const ACubicInches: Double): Double; static;
    class function CubicFeetToCubicMeters(const ACubicFeet: Double): Double; static;
    class function CubicYardsToCubicMeters(const ACubicYards: Double): Double; static;


    { Imperial System }

    //TODO


  end;

  TUOMVolume = record
  private
    FUnit: TUOMVolumeUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMVolumeUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMVolumeUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMVolume;
    class operator implicit(const AValue: TUOMVolume): Double;
    class operator implicit(const AValue: String): TUOMVolume;
    class operator implicit(const AValue: TUOMVolume): String;
    function ToMilliliters: Double;
    function ToCubicCentimeters: Double;
    function ToLiters: Double;
    function ToCubicMeters: Double;
    function ToUSTeaspoons: Double;
    function ToUSTablespoons: Double;
    function ToUSFluidOunces: Double;
    function ToUKTeaspoons: Double;
    function ToUKTablespoons: Double;
    function ToUKFluidOunces: Double;
    function ToCups: Double;
    function ToPints: Double;
    function ToQuarts: Double;
    function ToGallons: Double;
    function ToCubicInches: Double;
    function ToCubicFeet: Double;
    function ToCubicYards: Double;
  end;

  //TODO: A record that allows volume to be specified using three linear dimensions
  TUOMVolumeBox = record
  private
    //FWidth: TUOMLength;
    //FLength: TUOMLength;
    //FHeight: TUOMLength;
  public

  end;

implementation

var
  DefaultVolumeUnit: TUOMVolumeUnit;
  _: TUOMVolumeUtils;

{ TUOMVolumeUtils }

class constructor TUOMVolumeUtils.Create;
begin
  FUnits:= TList<TUOMUnitBaseClass>.Create;
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMVolumeUtils.Destroy;
begin
  FreeAndNil(FUnits);
end;

{
class procedure TUOMVolumeUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Milliliters');
  AList.Append('Cubic Centimeters');
  AList.Append('Liters');
  AList.Append('Cubic Meters');
  AList.Append('Teaspoons (US)');
  AList.Append('Tablespoons (US)');
  AList.Append('Fluid Ounces (US)');
  AList.Append('Teaspoons (UK)');
  AList.Append('Tablespoons (UK)');
  AList.Append('Fluid Ounces (UK)');
  AList.Append('Cups');
  AList.Append('Pints');
  AList.Append('Quarts');
  AList.Append('Gallons');
  AList.Append('Cubic Inches');
  AList.Append('Cubic Feet');
  AList.Append('Cubic Yards');
end;

class function TUOMVolumeUtils.UnitName(const AValue: TUOMVolumeUnit): String;
begin
  case AValue of
    umvMilliliters:       Result:= 'Milliliters';
    umvCubicCentimeters:  Result:= 'Cubic Centimeters';
    umvLiters:            Result:= 'Liters';
    umvCubicMeters:       Result:= 'Cubic Meters';
    umvTeaSpoonsUS:       Result:= 'Teaspoons (US)';
    umvTableSpoonsUS:     Result:= 'Tablespoons (US)';
    umvFluidOuncesUS:     Result:= 'Fluid Ounces (US)';
    umvTeaSpoonsUK:       Result:= 'Teaspoons (UK)';
    umvTableSpoonsUK:     Result:= 'Tablespoons (UK)';
    umvFluidOuncesUK:     Result:= 'Fluid Ounces (UK)';
    umvCups:              Result:= 'Cups';
    umvPints:             Result:= 'Pints';
    umvQuarts:            Result:= 'Quarts';
    umvGallons:           Result:= 'Gallons';
    umvCubicInches:       Result:= 'Cubic Inches';
    umvCubicFeet:         Result:= 'Cubic Feet';
    umvCubicYards:        Result:= 'Cubic Yards';
  end;
end;

class function TUOMVolumeUtils.UnitSuffix(const AValue: TUOMVolumeUnit): String;
begin
  case AValue of
    umvMilliliters:       Result:= 'mL';
    umvCubicCentimeters:  Result:= 'cm³';
    umvLiters:            Result:= 'l';
    umvCubicMeters:       Result:= 'm³';
    umvTeaSpoonsUS:       Result:= 'US tsp';
    umvTableSpoonsUS:     Result:= 'US Tsp';
    umvFluidOuncesUS:     Result:= 'US fl. oz';
    umvTeaSpoonsUK:       Result:= 'UK tsp';
    umvTableSpoonsUK:     Result:= 'UK Tsp';
    umvFluidOuncesUK:     Result:= 'UK fl. oz';
    umvCups:              Result:= 'cup';
    umvPints:             Result:= 'pt';
    umvQuarts:            Result:= 'oz';
    umvGallons:           Result:= 'gal';
    umvCubicInches:       Result:= 'in³';
    umvCubicFeet:         Result:= 'ft³';
    umvCubicYards:        Result:= 'yd³';
  end;
end;
}

class function TUOMVolumeUtils.GetUnit(const Index: Integer): TUOMUnitBaseClass;
begin
  Result:= FUnits[Index];
end;

class function TUOMVolumeUtils.UnitCount: Integer;
begin
  Result:= FUnits.Count;
end;

class function TUOMVolumeUtils.UOMID: String;
begin
  Result:= '{8D2E997E-199D-45AB-8AE1-A8BD5128D21A}';
end;

class function TUOMVolumeUtils.UOMName: String;
begin
  Result:= 'Volume';
end;

class procedure TUOMVolumeUtils.RegisterUnit(AUnitClass: TUOMUnitBaseClass);
begin
  FUnits.Add(AUnitClass);
end;

class procedure TUOMVolumeUtils.RegisterUnits;
begin
  RegisterUnit(TUOMVolumeMilliliters);
  RegisterUnit(TUOMVolumeCubicCentimeters);
  RegisterUnit(TUOMVolumeLiters);

  {
  AList.Append('Cubic Meters');
  AList.Append('Teaspoons (US)');
  AList.Append('Tablespoons (US)');
  AList.Append('Fluid Ounces (US)');
  AList.Append('Teaspoons (UK)');
  AList.Append('Tablespoons (UK)');
  AList.Append('Fluid Ounces (UK)');
  AList.Append('Cups');
  AList.Append('Pints');
  AList.Append('Quarts');
  AList.Append('Gallons');
  AList.Append('Cubic Inches');
  AList.Append('Cubic Feet');
  AList.Append('Cubic Yards');
  }

end;

class procedure TUOMVolumeUtils.RegisterUOM;
begin
  TUOMUtils.RegisterUOM(TUOMVolumeUtils);
end;

class function TUOMVolumeUtils.CubicMetersToMilliliters(
  const ACubicMeters: Double): Double;
begin
  Result:= ACubicMeters * 1000000;
end;

class function TUOMVolumeUtils.CupsToMilliliters(const ACups: Double): Double;
begin
  Result:= ACups * 240;
end;

class function TUOMVolumeUtils.USFluidOuncesToMilliliters(
  const AFluidOunces: Double): Double;
begin
  Result:= AFluidOunces * 29.5735;
end;

class function TUOMVolumeUtils.GallonsToMilliliters(
  const AGallons: Double): Double;
begin
  Result:= AGallons * 3785.41;
end;

class function TUOMVolumeUtils.LitersToMilliliters(
  const ALiters: Double): Double;
begin
  Result:= ALiters * 1000;
end;

class function TUOMVolumeUtils.PintsToMilliliters(const APints: Double): Double;
begin
  Result:= APints * 473.176;
end;

class function TUOMVolumeUtils.QuartsToMilliliters(
  const AQuarts: Double): Double;
begin
  Result:= AQuarts * 946.353;
end;

class function TUOMVolumeUtils.CubicYardsToCubicCentimeters(
  const ACubicYards: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicYardsToCubicMeters(const ACubicYards: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicYardsToLiters(const ACubicYards: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicYardsToMilliliters(const ACubicYards: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicMetersToCubicCentimeters(
  const ACubicMeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicMetersToCubicMeters(
  const ACubicMeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicMetersToLiters(
  const ACubicMeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CupsToCubicCentimeters(
  const ACups: Double): Double;
begin

end;

class function TUOMVolumeUtils.CupsToCubicMeters(const ACups: Double): Double;
begin

end;

class function TUOMVolumeUtils.CupsToLiters(const ACups: Double): Double;
begin

end;

class function TUOMVolumeUtils.USFluidOuncesToCubicCentimeters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.USFluidOuncesToCubicMeters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.USFluidOuncesToLiters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.GallonsToCubicCentimeters(
  const AGallons: Double): Double;
begin

end;

class function TUOMVolumeUtils.GallonsToCubicMeters(
  const AGallons: Double): Double;
begin

end;

class function TUOMVolumeUtils.GallonsToLiters(const AGallons: Double): Double;
begin

end;

class function TUOMVolumeUtils.LitersToCubicCentimeters(
  const ALiters: Double): Double;
begin

end;

class function TUOMVolumeUtils.MillilitersToCubicCentimeters(
  const AMilliliters: Double): Double;
begin

end;

class function TUOMVolumeUtils.MillilitersToCubicMeters(
  const AMilliliters: Double): Double;
begin

end;

class function TUOMVolumeUtils.MillilitersToLiters(
  const AMilliliters: Double): Double;
begin

end;

class function TUOMVolumeUtils.PintsToCubicCentimeters(
  const APints: Double): Double;
begin

end;

class function TUOMVolumeUtils.PintsToCubicMeters(const APints: Double): Double;
begin

end;

class function TUOMVolumeUtils.PintsToLiters(const APints: Double): Double;
begin

end;

class function TUOMVolumeUtils.QuartsToCubicCentimeters(
  const AQuarts: Double): Double;
begin

end;

class function TUOMVolumeUtils.QuartsToCubicMeters(
  const AQuarts: Double): Double;
begin

end;

class function TUOMVolumeUtils.QuartsToLiters(const AQuarts: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTablespoonsToCubicCentimeters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTablespoonsToCubicMeters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTablespoonsToLiters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTablespoonsToMilliliters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTeaspoonsToCubicCentimeters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTeaspoonsToCubicMeters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTeaspoonsToLiters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.USTeaspoonsToMilliliters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicCentimetersToCubicMeters(
  const ACubicCentimeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicCentimetersToLiters(
  const ACubicCentimeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicCentimetersToMilliliters(
  const ACubicCentimeters: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicFeetToCubicCentimeters(
  const ACubicFeet: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicFeetToCubicMeters(
  const ACubicFeet: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicFeetToLiters(
  const ACubicFeet: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicFeetToMilliliters(
  const ACubicFeet: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicInchesToCubicCentimeters(
  const ACubicInches: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicInchesToCubicMeters(
  const ACubicInches: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicInchesToLiters(
  const ACubicInches: Double): Double;
begin

end;

class function TUOMVolumeUtils.CubicInchesToMilliliters(
  const ACubicInches: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKFluidOuncesToCubicCentimeters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKFluidOuncesToCubicMeters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKFluidOuncesToLiters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKFluidOuncesToMilliliters(
  const AFluidOunces: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTablespoonsToCubicCentimeters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTablespoonsToCubicMeters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTablespoonsToLiters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTablespoonsToMilliliters(
  const ATablespoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTeaspoonsToCubicCentimeters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTeaspoonsToCubicMeters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTeaspoonsToLiters(
  const ATeaspoons: Double): Double;
begin

end;

class function TUOMVolumeUtils.UKTeaspoonsToMilliliters(
  const ATeaspoons: Double): Double;
begin

end;

{ TUOMVolume }

class operator TUOMVolume.implicit(const AValue: Double): TUOMVolume;
begin
  Result.FUnit:= DefaultVolumeUnit;
  Result.FValue:= AValue;
end;

class operator TUOMVolume.implicit(const AValue: TUOMVolume): Double;
begin
  case DefaultVolumeUnit of
    umvMilliliters:       Result:= AValue.ToMilliliters;
    umvCubicCentimeters:  Result:= AValue.ToCubicCentimeters;
    umvLiters:            Result:= AValue.ToLiters;
    umvCubicMeters:       Result:= AValue.ToCubicMeters;
    umvTeaSpoonsUS:       Result:= AValue.ToUSTeaspoons;
    umvTablespoonsUS:     Result:= AValue.ToUSTablespoons;
    umvFluidOuncesUS:     Result:= AValue.ToUSFluidOunces;
    umvTeaSpoonsUK:       Result:= AValue.ToUKTeaspoons;
    umvTableSpoonsUK:     Result:= AVAlue.ToUKTablespoons;
    umvFluidOuncesUK:     Result:= AValue.ToUKFluidOunces;
    umvCups:              Result:= AValue.ToCups;
    umvPints:             Result:= AValue.ToPints;
    umvQuarts:            Result:= AValue.ToQuarts;
    umvGallons:           Result:= AValue.ToGallons;
    umvCubicInches:       Result:= AValue.ToCubicInches;
    umvCubicFeet:         Result:= AValue.ToCubicFeet;
    umvCubicYards:        Result:= AValue.ToCubicYards;
  end;
end;

class operator TUOMVolume.implicit(const AValue: TUOMVolume): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  //TODO: Result:= Result + TUOMVolumeUtils.UnitSuffix(AValue.FUnit);
end;

class operator TUOMVolume.implicit(const AValue: String): TUOMVolume;
begin
  //TODO: Parse

end;

procedure TUOMVolume.SetUnit(const Value: TUOMVolumeUnit);
begin
  case FUnit of
    umvMilliliters:       FValue:= Self.ToMilliliters;
    umvCubicCentimeters:  FValue:= Self.ToCubicCentimeters;
    umvLiters:            FValue:= Self.ToLiters;
    umvCubicMeters:       FValue:= Self.ToCubicMeters;
    umvTeaSpoonsUS:       FValue:= Self.ToUSTeaspoons;
    umvTableSpoonsUS:     FValue:= Self.ToUSTablespoons;
    umvFluidOuncesUS:     FValue:= Self.ToUSFluidOunces;
    umvTeaSpoonsUK:       FValue:= Self.ToUKTeaspoons;
    umvTableSpoonsUK:     FValue:= Self.ToUKTablespoons;
    umvFluidOuncesUK:     FValue:= Self.ToUKFluidOunces;
    umvCups:              FValue:= Self.ToCups;
    umvPints:             FValue:= Self.ToPints;
    umvQuarts:            FValue:= Self.ToQuarts;
    umvGallons:           FValue:= Self.ToGallons;
    umvCubicInches:       FValue:= Self.ToCubicInches;
    umvCubicFeet:         FValue:= Self.ToCubicFeet;
    umvCubicYards:        FValue:= Self.ToCubicYards;
  end;
  FUnit:= Value;
end;

procedure TUOMVolume.SetValue(const Value: Double);
begin
  FValue := Value;
end;

function TUOMVolume.ToMilliliters: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       Result:= FValue; //Same
    umvCubicCentimeters:  Result:= TUOMVolumeUtils.CubicCentimetersToMilliliters(FValue);
    umvLiters:            Result:= TUOMVolumeUtils.LitersToMilliliters(FValue);
    umvCubicMeters:       Result:= TUOMVolumeUtils.CubicMetersToMilliliters(FValue);
    umvTeaSpoonsUS:       Result:= TUOMVolumeUtils.USTeaspoonsToMilliliters(FValue);
    umvTableSpoonsUS:     Result:= TUOMVolumeUtils.USTablespoonsToMilliliters(FValue);
    umvFluidOuncesUS:     Result:= TUOMVolumeUtils.USFluidOuncesToMilliliters(FValue);
    umvTeaSpoonsUK:       Result:= TUOMVolumeUtils.UKTeaspoonsToMilliliters(FValue);
    umvTableSpoonsUK:     Result:= TUOMVolumeUtils.UKTablespoonsToMilliliters(FValue);
    umvFluidOuncesUK:     Result:= TUOMVolumeUtils.UKFluidOuncesToMilliliters(FValue);
    umvCups:              Result:= TUOMVolumeUtils.CupsToMilliliters(FValue);
    umvPints:             Result:= TUOMVolumeUtils.PintsToMilliliters(FValue);
    umvQuarts:            Result:= TUOMVolumeUtils.QuartsToMilliliters(FValue);
    umvGallons:           Result:= TUOMVolumeUtils.GallonsToMilliliters(FValue);
    umvCubicInches:       Result:= TUOMVolumeUtils.CubicInchesToMilliliters(FValue);
    umvCubicFeet:         Result:= TUOMVolumeUtils.CubicFeetToMilliliters(FValue);
    umvCubicYards:        Result:= TUOMVolumeUtils.CubicYardsToMilliliters(FValue);
  end;
end;

function TUOMVolume.ToCubicCentimeters: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ; //Result:= TUOMVolumeUtils.MillilitersToCubicCentimeters(FValue);
    umvCubicCentimeters:  Result:= FValue; //Same
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToCubicMeters: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       Result:= FValue; //Same
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToCubicFeet: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         Result:= FValue; //Same
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToCubicInches: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       Result:= FValue; //Same
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToCubicYards: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        Result:= FValue; //Same
  end;
end;

function TUOMVolume.ToCups: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              Result:= FValue; //Same
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToGallons: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           Result:= FValue; //Same
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToLiters: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            Result:= FValue; //Same
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToPints: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             Result:= FValue; //Same
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToQuarts: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            Result:= FValue; //Same
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUSTablespoons: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     Result:= FValue; //Same
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUSTeaspoons: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       Result:= FValue; //Same
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUSFluidOunces: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     Result:= FValue; //Same
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUKTablespoons: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     Result:= FValue; //Same
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUKTeaspoons: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       Result:= FValue; //Same
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     ;
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

function TUOMVolume.ToUKFluidOunces: Double;
begin
  Result:= 0;
  case FUnit of
    umvMilliliters:       ;
    umvCubicCentimeters:  ;
    umvLiters:            ;
    umvCubicMeters:       ;
    umvTeaSpoonsUS:       ;
    umvTableSpoonsUS:     ;
    umvFluidOuncesUS:     ;
    umvTeaSpoonsUK:       ;
    umvTableSpoonsUK:     ;
    umvFluidOuncesUK:     Result:= FValue; //Same
    umvCups:              ;
    umvPints:             ;
    umvQuarts:            ;
    umvGallons:           ;
    umvCubicInches:       ;
    umvCubicFeet:         ;
    umvCubicYards:        ;
  end;
end;

{ TUOMVolumeUnitBase }

class function TUOMVolumeUnitBase.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMVolumeUnitBase.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMVolumeUnitBase.NameSingular: String;
begin
  Result:= '';
end;

class function TUOMVolumeUnitBase.Prefix: String;
begin
  Result:= '';
end;

class function TUOMVolumeUnitBase.Suffix: String;
begin
  Result:= '';
end;

class function TUOMVolumeUnitBase.Systems: TUOMSystems;
begin
  Result:= [];
end;

class function TUOMVolumeUnitBase.UnitDescription: String;
begin
  Result:= '';
end;

class function TUOMVolumeUnitBase.UnitID: String;
begin
  Result:= '';
end;

class function TUOMVolumeUnitBase.UOM: TUOMBaseClass;
begin
  Result:= TUOMVolumeUtils;
end;

{ TUOMVolumeMilliliters }

class function TUOMVolumeMilliliters.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 1000000;
end;

class function TUOMVolumeMilliliters.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 1000000;
end;

class function TUOMVolumeMilliliters.NameSingular: String;
begin
  Result:= 'Milliliter';
end;

class function TUOMVolumeMilliliters.Suffix: String;
begin
  Result:= 'mL';
end;

class function TUOMVolumeMilliliters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMVolumeMilliliters.UnitEnum: TUOMVolumeUnit;
begin
  Result:= umvMilliliters;
end;

class function TUOMVolumeMilliliters.UnitID: String;
begin
  Result:= '{DCB45B7C-0F5C-46E5-94F3-73DE0308E8FF}';
end;

{ TUOMVolumeCubicCentimeters }

class function TUOMVolumeCubicCentimeters.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 1000000;
end;

class function TUOMVolumeCubicCentimeters.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 1000000;
end;

class function TUOMVolumeCubicCentimeters.NameSingular: String;
begin
  Result:= 'Cubic Centimeter';
end;

class function TUOMVolumeCubicCentimeters.Suffix: String;
begin
  Result:= 'cm³';
end;

class function TUOMVolumeCubicCentimeters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMVolumeCubicCentimeters.UnitEnum: TUOMVolumeUnit;
begin
  Result:= umvCubicCentimeters;
end;

class function TUOMVolumeCubicCentimeters.UnitID: String;
begin
  Result:= '{133C3A39-D1F6-449E-970A-33B1ACD38058}';
end;

{ TUOMVolumeLiters }

class function TUOMVolumeLiters.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue * 1000;
end;

class function TUOMVolumeLiters.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue / 1000;
end;

class function TUOMVolumeLiters.NameSingular: String;
begin
  Result:= 'Liter';
end;

class function TUOMVolumeLiters.Suffix: String;
begin
  Result:= 'L';
end;

class function TUOMVolumeLiters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMVolumeLiters.UnitEnum: TUOMVolumeUnit;
begin
  Result:= umvLiters;
end;

class function TUOMVolumeLiters.UnitID: String;
begin
  Result:= '{F2DBAB17-4816-4C4D-90F5-88B85C002A6E}';
end;

initialization
  _:= nil;
  DefaultVolumeUnit:= TUOMVolumeUnit.umvCubicMeters;
end.
