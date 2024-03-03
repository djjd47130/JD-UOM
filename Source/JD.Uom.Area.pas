unit JD.Uom.Area;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Length;

type
  TUOMAreaUnit = (umaSquareMillimeters, umaSquareCentimeters, umaSquareMeters,
    umaHectares, umaSquareKilometers, umaSquareInches, umaSquareFeet,
    umaSquareYards, umaAcres, umaSquareMiles);
  TUOMAreaUnits = set of TUOMAreaUnit;

  TUOMAreaUnitBase = class;
  TUOMAreaUtils = class;

  TUOMAreaUnitBaseClass = class of TUOMAreaUnitBase;

  TUOMAreaUnitBase = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; virtual; abstract;
  end;

  //Specific Area Units

  TUOMAreaSquareMillimeters = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareCentimeters = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareMeters = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaHectares = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareKilometers = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareInches = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function NamePlural: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareFeet = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function NamePlural: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareYards = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaAcres = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;

  TUOMAreaSquareMiles = class(TUOMAreaUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMAreaUnit; override;
  end;



  TUOMAreaUtils = class(TUOMBase)
  private
    class var FUnits: TList<TUOMUnitClass>;
    class procedure RegisterUOM;
    class procedure RegisterUnits;
    class procedure RegisterUnit(AUnitClass: TUOMUnitClass);
  public
    class constructor Create;
    class destructor Destroy;
    class function UOMID: String; override;
    class function UOMName: String; override;
    class function UnitCount: Integer; override;
    class function GetUnit(const Index: Integer): TUOMUnitClass; override;
    class function BaseUnit: TUOMUnitClass; override;

    class function UnitByEnum(const AUnit: TUOMAreaUnit): TUOMAreaUnitBaseClass;
    class function Convert(const AValue: Double; const AFromUnit,
      AToUnit: TUOMAreaUnit): Double;

    { Metric }

    //Square Millimeters
    class function SquareCentimetersToSquareMillimeters(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToSquareMillimeters(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareMillimeters(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareMillimeters(const ASquareKilometers: Double): Double; static;
    //Square Centimeters
    class function SquareMillimetersToSquareCentimeters(const ASquareMillimeters: Double): Double; static;
    class function SquareMetersToSquareCentimeters(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareCentimeters(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareCentimeters(const ASquareKilometers: Double): Double; static;
    //Square Meters
    class function SquareMillimetersToSquareMeters(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareMeters(const ASquareCentimeters: Double): Double; static;
    class function HectaresToSquareMeters(const AHectares: Double): Double;
    class function SquareKilometersToSquareMeters(const ASquareKilometers: Double): Double; static;
    //Hectares
    class function SquareMillimetersToHectares(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToHectares(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToHectares(const ASquareMeters: Double): Double; static;
    class function SquareKilometersToHectares(const ASquareKilometers: Double): Double; static;
    //Square Kilometers
    class function SquareMillimetersToSquareKilometers(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareKilometers(const ASquareCentimeters: Double): Double; static;
    class function HectaresToSquareKilometers(const AHectares: Double): Double; static;
    class function SquareMetersToSquareKilometers(const ASquareMeters: Double): Double; static;

    { US Customary }

    //Square Inches
    class function SquareFeetToSquareInches(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareInches(const ASquareYards: Double): Double; static;
    class function AcresToSquareInches(const AAcres: Double): Double; static;
    class function SquareMilesToSquareInches(const ASquareMiles: Double): Double; static;
    //Square Feet
    class function SquareInchesToSquareFeet(const ASquareInches: Double): Double; static;
    class function SquareYardsToSquareFeet(const ASquareYards: Double): Double; static;
    class function AcresToSquareFeet(const AAcres: Double): Double; static;
    class function SquareMilesToSquareFeet(const ASquareMiles: Double): Double; static;
    //Square Yards
    class function SquareInchesToSquareYards(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareYards(const ASquareFeet: Double): Double; static;
    class function AcresToSquareYards(const AAcres: Double): Double; static;
    class function SquareMilesToSquareYards(const ASquareMiles: Double): Double; static;
    //Acres
    class function SquareInchesToAcres(const ASquareInches: Double): Double; static;
    class function SquareFeetToAcres(const ASquareFeet: Double): Double; static;
    class function SquareYardsToAcres(const ASquareYards: Double): Double; static;
    class function SquareMilesToAcres(const ASquareMiles: Double): Double; static;
    //Square Miles
    class function SquareInchesToSquareMiles(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareMiles(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareMiles(const ASquareYards: Double): Double; static;
    class function AcresToSquareMiles(const AAcres: Double): Double; static;

    { Metric to US Customary Conversion }

    //Square Inches
    class function SquareMillimetersToSquareInches(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareInches(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToSquareInches(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareInches(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareInches(const ASquareKilometers: Double): Double; static;
    //Square Feet
    class function SquareMillimetersToSquareFeet(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareFeet(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToSquareFeet(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareFeet(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareFeet(const ASquareKilometers: Double): Double; static;
    //Square Yards
    class function SquareMillimetersToSquareYards(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareYards(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToSquareYards(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareYards(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareYards(const ASquareKilometers: Double): Double; static;
    //Acres
    class function SquareMillimetersToAcres(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToAcres(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToAcres(const ASquareMeters: Double): Double; static;
    class function HectaresToAcres(const AHectares: Double): Double; static;
    class function SquareKilometersToAcres(const ASquareKilometers: Double): Double; static;
    //Square Miles
    class function SquareMillimetersToSquareMiles(const ASquareMillimeters: Double): Double; static;
    class function SquareCentimetersToSquareMiles(const ASquareCentimeters: Double): Double; static;
    class function SquareMetersToSquareMiles(const ASquareMeters: Double): Double; static;
    class function HectaresToSquareMiles(const AHectares: Double): Double; static;
    class function SquareKilometersToSquareMiles(const ASquareKilometers: Double): Double; static;

    { US Customary to Metric Conversion }

    //Square Millimeters
    class function SquareInchesToSquareMillimeters(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareMillimeters(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareMillimeters(const ASquareYards: Double): Double; static;
    class function AcresToSquareMillimeters(const AAcres: Double): Double; static;
    class function SquareMilesToSquareMillimeters(const ASquareMiles: Double): Double; static;
    //Square Centimeters
    class function SquareInchesToSquareCentimeters(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareCentimeters(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareCentimeters(const ASquareYards: Double): Double; static;
    class function AcresToSquareCentimeters(const AAcres: Double): Double; static;
    class function SquareMilesToSquareCentimeters(const ASquareMiles: Double): Double; static;
    //Square Meters
    class function SquareInchesToSquareMeters(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareMeters(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareMeters(const ASquareYards: Double): Double; static;
    class function AcresToSquareMeters(const AAcres: Double): Double; static;
    class function SquareMilesToSquareMeters(const ASquareMiles: Double): Double; static;
    //Hectares
    class function SquareInchesToHectares(const ASquareInches: Double): Double; static;
    class function SquareFeetToHectares(const ASquareFeet: Double): Double; static;
    class function SquareYardsToHectares(const ASquareYards: Double): Double; static;
    class function AcresToHectares(const AAcres: Double): Double; static;
    class function SquareMilesToHectares(const ASquareMiles: Double): Double; static;
    //Square Kilometers
    class function SquareInchesToSquareKilometers(const ASquareInches: Double): Double; static;
    class function SquareFeetToSquareKilometers(const ASquareFeet: Double): Double; static;
    class function SquareYardsToSquareKilometers(const ASquareYards: Double): Double; static;
    class function AcresToSquareKilometers(const AAcres: Double): Double; static;
    class function SquareMilesToSquareKilometers(const ASquareMiles: Double): Double; static;

  end;

  TUOMArea = record
  private
    FUnit: TUOMAreaUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMAreaUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMAreaUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMArea;
    class operator implicit(const AValue: TUOMArea): Double;
    class operator implicit(const AValue: String): TUOMArea;
    class operator implicit(const AValue: TUOMArea): String;
    //TODO: Implement class operators for math...
  public
    function ToSquareMillimeters: Double;
    function ToSquareCentimeters: Double;
    function ToSquareMeters: Double;
    function ToHectares: Double;
    function ToSquareKilometers: Double;
    function ToSquareInches: Double;
    function ToSquareFeet: Double;
    function ToSquareYards: Double;
    function ToAcres: Double;
    function ToSquareMiles: Double;
    //TODO: Change to Properties with Getters and Setters...
  end;

  //TODO: A record that allows area to be specified using two linear dimensions
  TUOMAreaRect = record
  private
    FWidth: Double;
    FLength: Double;
    procedure SetLength(const Value: Double);
    procedure SetWidth(const Value: Double);
    function GetArea: Double;
  public
    property Width: Double read FWidth write SetWidth;
    property Length: Double read FLength write SetLength;
    property Area: Double read GetArea;
    class operator implicit(const AValue: Double): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): Double;
    class operator implicit(const AValue: String): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): String;
  end;

implementation

var
  _: TUOMAreaUtils;

{ TUOMAreaUtils }

class constructor TUOMAreaUtils.Create;
begin
  FUnits:= TList<TUOMUnitClass>.Create;
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMAreaUtils.Destroy;
begin
  FreeAndNil(FUnits);
end;

class function TUOMAreaUtils.GetUnit(const Index: Integer): TUOMUnitClass;
begin
  Result:= FUnits[Index];
end;

class function TUOMAreaUtils.UOMID: String;
begin
  Result:= '{6E5AC0E9-69C3-486E-A9E1-7E088C4FE1B1}';
end;

class function TUOMAreaUtils.UOMName: String;
begin
  Result:= 'Area';
end;

class procedure TUOMAreaUtils.RegisterUnit(AUnitClass: TUOMUnitClass);
begin
  FUnits.Add(AUnitClass);
end;

class procedure TUOMAreaUtils.RegisterUnits;
begin
  RegisterUnit(TUOMAreaSquareMillimeters);
  RegisterUnit(TUOMAreaSquareCentimeters);
  RegisterUnit(TUOMAreaSquareMeters);
  RegisterUnit(TUOMAreaHectares);
  RegisterUnit(TUOMAreaSquareKilometers);
  RegisterUnit(TUOMAreaSquareInches);
  RegisterUnit(TUOMAreaSquareFeet);
  RegisterUnit(TUOMAreaSquareYards);
  RegisterUnit(TUOMAreaAcres);
  RegisterUnit(TUOMAreaSquareMiles);
end;

class procedure TUOMAreaUtils.RegisterUOM;
begin
  TUOMUtils.RegisterUOM(TUOMAreaUtils);
end;

class function TUOMAreaUtils.Convert(const AValue: Double; const AFromUnit,
  AToUnit: TUOMAreaUnit): Double;
var
  F, T: TUOMAreaUnitBaseClass;
begin
  F:= UnitByEnum(AFromUnit);
  T:= UnitByEnum(AToUnit);
  Result:= F.ConvertToBase(AValue);
  Result:= T.ConvertFromBase(Result);
end;

class function TUOMAreaUtils.UnitByEnum(
  const AUnit: TUOMAreaUnit): TUOMAreaUnitBaseClass;
var
  X: Integer;
  U: TUOMAreaUnitBaseClass;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    U:= TUOMAreaUnitBaseClass(FUnits[X]);
    if U.UnitEnum = AUnit then begin
      Result:= U;
      Break;
    end;
  end;
end;

class function TUOMAreaUtils.UnitCount: Integer;
begin
  Result:= FUnits.Count;
end;

class function TUOMAreaUtils.HectaresToSquareMeters(
  const AHectares: Double): Double;
begin
  Result:= AHectares * 10000;
end;

class function TUOMAreaUtils.SquareMillimetersToSquareCentimeters(
  const ASquareMillimeters: Double): Double;
begin
  Result:= ASquareMillimeters * 0.01;
end;

class function TUOMAreaUtils.SquareCentimetersToSquareMillimeters(
  const ASquareCentimeters: Double): Double;
begin
  Result:= ASquareCentimeters * 100;
end;

class function TUOMAreaUtils.SquareCentimetersToSquareMeters(
  const ASquareCentimeters: Double): Double;
begin
  Result:= ASquareCentimeters * 0.0001;
end;

class function TUOMAreaUtils.SquareMetersToSquareMillimeters(
  const ASquareMeters: Double): Double;
begin
  Result:= SquareCentimetersToSquareMillimeters(SquareMetersToSquareCentimeters(ASquareMeters));
end;

class function TUOMAreaUtils.SquareMillimetersToSquareMeters(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareCentimetersToSquareMeters(SquareMillimetersToSquareCentimeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.AcresToHectares(const AAcres: Double): Double;
begin
  Result:= AAcres * 0.404686;
end;

class function TUOMAreaUtils.AcresToSquareCentimeters(
  const AAcres: Double): Double;
begin
  Result:= SquareFeetToSquareCentimeters(AcresToSquareFeet(AAcres));
end;

class function TUOMAreaUtils.AcresToSquareFeet(const AAcres: Double): Double;
begin
  Result:= AAcres * 43560;
end;

class function TUOMAreaUtils.AcresToSquareInches(const AAcres: Double): Double;
begin
  Result:= SquareFeetToSquareInches(AcresToSquareFeet(AAcres));
end;

class function TUOMAreaUtils.AcresToSquareKilometers(
  const AAcres: Double): Double;
begin
  Result:= AAcres * 0.00404686;
end;

class function TUOMAreaUtils.AcresToSquareMeters(const AAcres: Double): Double;
begin
  Result:= AAcres * 4046.86;
end;

class function TUOMAreaUtils.AcresToSquareMiles(const AAcres: Double): Double;
begin
  Result:= AAcres * 0.0015625;
end;

class function TUOMAreaUtils.AcresToSquareMillimeters(
  const AAcres: Double): Double;
begin
  Result:= SquareMetersToSquareMillimeters(AcresToSquareMeters(AAcres));
end;

class function TUOMAreaUtils.AcresToSquareYards(const AAcres: Double): Double;
begin
  Result:= AAcres * 4840;
end;

class function TUOMAreaUtils.BaseUnit: TUOMUnitClass;
begin
  Result:= TUOMAreaSquareMeters;
end;

class function TUOMAreaUtils.SquareInchesToSquareFeet(
  const ASquareInches: Double): Double;
begin
  Result:= ASquareInches * 0.00694444;
end;

class function TUOMAreaUtils.SquareMilesToHectares(
  const ASquareMiles: Double): Double;
begin
  Result:= ASquareMiles * 258.999;
end;

class function TUOMAreaUtils.SquareInchesToSquareMeters(
  const ASquareInches: Double): Double;
begin
  Result:= ASquareInches * 0.00064516;
end;

class function TUOMAreaUtils.SquareInchesToSquareMillimeters(
  const ASquareInches: Double): Double;
begin
  Result:= ASquareInches * 645.16;
end;

class function TUOMAreaUtils.SquareFeetToSquareCentimeters(
  const ASquareFeet: Double): Double;
begin
  Result:= ASquareFeet * 929.03;
end;

class function TUOMAreaUtils.SquareFeetToSquareInches(
  const ASquareFeet: Double): Double;
begin
  Result:= ASquareFeet * 144;
end;

class function TUOMAreaUtils.SquareFeetToSquareMeters(
  const ASquareFeet: Double): Double;
begin
  Result:= ASquareFeet * 0.092903;
end;

class function TUOMAreaUtils.SquareFeetToSquareMillimeters(
  const ASquareFeet: Double): Double;
begin
  Result:= ASquareFeet * 92903;
end;

class function TUOMAreaUtils.SquareInchesToSquareCentimeters(
  const ASquareInches: Double): Double;
begin
  Result:= ASquareInches * 6.4516;
end;

class function TUOMAreaUtils.HectaresToAcres(const AHectares: Double): Double;
begin
  Result:= AHectares * 2.47105;
end;

class function TUOMAreaUtils.HectaresToSquareKilometers(
  const AHectares: Double): Double;
begin
  Result:= AHectares * 0.01;
end;

class function TUOMAreaUtils.HectaresToSquareYards(
  const AHectares: Double): Double;
begin
  Result:= AHectares * 11959.9;
end;

class function TUOMAreaUtils.HectaresToSquareCentimeters(
  const AHectares: Double): Double;
begin
  Result:= SquareFeetToSquareCentimeters(HectaresToSquareFeet(AHectares));
end;

class function TUOMAreaUtils.HectaresToSquareFeet(
  const AHectares: Double): Double;
begin
  Result:= AHectares * 107639;
end;

class function TUOMAreaUtils.HectaresToSquareInches(
  const AHectares: Double): Double;
begin
  Result:= SquareFeetToSquareInches(HectaresToSquareFeet(AHectares));
end;

class function TUOMAreaUtils.HectaresToSquareMiles(
  const AHectares: Double): Double;
begin
  Result:= AHectares * 0.00386102;
end;

class function TUOMAreaUtils.SquareMillimetersToHectares(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareMetersToHectares(SquareMillimetersToSquareMeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareMillimetersToSquareKilometers(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareMetersToSquareKilometers(SquareMillimetersToSquareMeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareMillimetersToSquareInches(
  const ASquareMillimeters: Double): Double;
begin
  Result:= ASquareMillimeters * 0.00155;
end;

class function TUOMAreaUtils.SquareMillimetersToSquareFeet(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareInchesToSquareFeet(SquareMillimetersToSquareInches(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareMillimetersToSquareYards(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareMetersToSquareYards(SquareMillimetersToSquareMeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareMillimetersToAcres(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareMetersToAcres(SquareMillimetersToSquareMeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareMillimetersToSquareMiles(
  const ASquareMillimeters: Double): Double;
begin
  Result:= SquareMetersToSquareMiles(SquareMillimetersToSquareMeters(ASquareMillimeters));
end;

class function TUOMAreaUtils.SquareCentimetersToSquareMiles(
  const ASquareCentimeters: Double): Double;
begin
  Result:= SquareMetersToSquareMiles(SquareCentimetersToSquareMeters(ASquareCentimeters));
end;

class function TUOMAreaUtils.SquareMetersToSquareMiles(
  const ASquareMeters: Double): Double;
begin
  Result:= AcresToSquareMiles(SquareMetersToAcres(ASquareMeters));
end;

class function TUOMAreaUtils.SquareMetersToSquareYards(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters * 1.19599;
end;

class function TUOMAreaUtils.HectaresToSquareMillimeters(
  const AHectares: Double): Double;
begin
  Result:= SquareMetersToSquareMillimeters(HectaresToSquareMeters(AHectares));
end;

class function TUOMAreaUtils.SquareCentimetersToAcres(
  const ASquareCentimeters: Double): Double;
begin
  Result:= SquareMetersToAcres(SquareCentimetersToSquareMeters(ASquareCentimeters));
end;

class function TUOMAreaUtils.SquareCentimetersToHectares(
  const ASquareCentimeters: Double): Double;
begin
  Result:= SquareMetersToHectares(SquareCentimetersToSquareMeters(ASquareCentimeters));
end;

class function TUOMAreaUtils.SquareCentimetersToSquareFeet(
  const ASquareCentimeters: Double): Double;
begin
  Result:= ASquareCentimeters * 0.00107639;
end;

class function TUOMAreaUtils.SquareCentimetersToSquareInches(
  const ASquareCentimeters: Double): Double;
begin
  Result:= ASquareCentimeters * 0.155;
end;

class function TUOMAreaUtils.SquareCentimetersToSquareKilometers(
  const ASquareCentimeters: Double): Double;
begin
  Result:= SquareMetersToSquareKilometers(SquareCentimetersToSquareMeters(ASquareCentimeters));
end;

class function TUOMAreaUtils.SquareCentimetersToSquareYards(
  const ASquareCentimeters: Double): Double;
begin
  Result:= ASquareCentimeters * 0.000119599;
end;

class function TUOMAreaUtils.SquareFeetToAcres(
  const ASquareFeet: Double): Double;
begin
  Result:= SquareMilesToAcres(SquareFeetToSquareMiles(ASquareFeet));
end;

class function TUOMAreaUtils.SquareFeetToHectares(
  const ASquareFeet: Double): Double;
begin
  Result:= SquareMilesToHectares(SquareFeetToSquareMiles(ASquareFeet));
end;

class function TUOMAreaUtils.SquareFeetToSquareKilometers(
  const ASquareFeet: Double): Double;
begin
  Result:= SquareMilesToSquareKilometers(SquareFeetToSquareMiles(ASquareFeet));
end;

class function TUOMAreaUtils.SquareFeetToSquareMiles(
  const ASquareFeet: Double): Double;
begin
  Result:= SquareYardsToSquareMiles(SquareFeetToSquareYards(ASquareFeet));
end;

class function TUOMAreaUtils.SquareFeetToSquareYards(
  const ASquareFeet: Double): Double;
begin
  Result:= ASquareFeet * 0.111111;
end;

class function TUOMAreaUtils.SquareInchesToSquareYards(
  const ASquareInches: Double): Double;
begin
  Result:= ASquareInches * 0.000771605;
end;

class function TUOMAreaUtils.SquareInchesToSquareMiles(
  const ASquareInches: Double): Double;
begin
  Result:= SquareFeetToSquareMiles(SquareInchesToSquareFeet(ASquareInches));
end;

class function TUOMAreaUtils.SquareKilometersToAcres(
  const ASquareKilometers: Double): Double;
begin
  Result:= ASquareKilometers * 247.105;
end;

class function TUOMAreaUtils.SquareKilometersToHectares(
  const ASquareKilometers: Double): Double;
begin
  Result:= ASquareKilometers * 100;
end;

class function TUOMAreaUtils.SquareKilometersToSquareCentimeters(
  const ASquareKilometers: Double): Double;
begin
  Result:= HectaresToSquareCentimeters(SquareKilometersToHectares(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareInchesToAcres(
  const ASquareInches: Double): Double;
begin
  Result:= SquareYardsToAcres(SquareInchesToSquareYards(ASquareInches));
end;

class function TUOMAreaUtils.SquareInchesToHectares(
  const ASquareInches: Double): Double;
begin
  Result:= SquareYardsToHectares(SquareInchesToSquareYards(ASquareInches));
end;

class function TUOMAreaUtils.SquareInchesToSquareKilometers(
  const ASquareInches: Double): Double;
begin
  Result:= HectaresToSquareKilometers(SquareInchesToHectares(ASquareInches));
end;

class function TUOMAreaUtils.SquareKilometersToSquareFeet(
  const ASquareKilometers: Double): Double;
begin
  Result:= HectaresToSquareFeet(SquareKilometersToHectares(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareKilometersToSquareInches(
  const ASquareKilometers: Double): Double;
begin
  Result:= HectaresToSquareInches(SquareKilometersToHectares(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareKilometersToSquareMeters(
  const ASquareKilometers: Double): Double;
begin
  Result:= HectaresToSquareMeters(SquareMetersToHectares(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareKilometersToSquareMiles(
  const ASquareKilometers: Double): Double;
begin
  Result:= ASquareKilometers * 0.386102;
end;

class function TUOMAreaUtils.SquareKilometersToSquareMillimeters(
  const ASquareKilometers: Double): Double;
begin
  Result:= SquareMetersToSquareMillimeters(SquareKilometersToSquareMeters(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareKilometersToSquareYards(
  const ASquareKilometers: Double): Double;
begin
  Result:= SquareMetersToSquareYards(SquareKilometersToSquareMeters(ASquareKilometers));
end;

class function TUOMAreaUtils.SquareMetersToAcres(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters * 0.000247105;
end;

class function TUOMAreaUtils.SquareMetersToHectares(
  const ASquareMeters: Double): Double;
begin
  Result:= SquareKilometersToHectares(SquareMetersToSquareKilometers(ASquareMeters));
end;

class function TUOMAreaUtils.SquareMetersToSquareCentimeters(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters * 10000;
end;

class function TUOMAreaUtils.SquareMetersToSquareFeet(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters * 10.7639;
end;

class function TUOMAreaUtils.SquareMetersToSquareInches(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters * 1550;
end;

class function TUOMAreaUtils.SquareMetersToSquareKilometers(
  const ASquareMeters: Double): Double;
begin
  Result:= ASquareMeters / 1000000;
end;

class function TUOMAreaUtils.SquareMilesToAcres(
  const ASquareMiles: Double): Double;
begin
  Result:= ASquareMiles * 640;
end;

class function TUOMAreaUtils.SquareMilesToSquareInches(
  const ASquareMiles: Double): Double;
begin
  Result:= AcresToSquareInches(SquareMilesToAcres(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareCentimeters(
  const ASquareMiles: Double): Double;
begin
  Result:= SquareInchesToSquareCentimeters(SquareMilesToSquareInches(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareFeet(
  const ASquareMiles: Double): Double;
begin
  Result:= AcresToSquareFeet(SquareMilesToAcres(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareKilometers(
  const ASquareMiles: Double): Double;
begin
  Result:= AcresToSquareKilometers(SquareMilesToAcres(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareMeters(
  const ASquareMiles: Double): Double;
begin
  Result:= AcresToSquareMeters(SquareMilesToAcres(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareMillimeters(
  const ASquareMiles: Double): Double;
begin
  Result:= SquareInchesToSquareMillimeters(SquareMilesToSquareInches(ASquareMiles));
end;

class function TUOMAreaUtils.SquareMilesToSquareYards(
  const ASquareMiles: Double): Double;
begin
  Result:= AcresToSquareYards(SquareMilesToAcres(ASQuareMiles));
end;

class function TUOMAreaUtils.SquareYardsToAcres(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 0.000206612;
end;

class function TUOMAreaUtils.SquareYardsToHectares(
  const ASquareYards: Double): Double;
begin
  Result:= AcresToHectares(SquareYardsToAcres(ASquareYards));
end;

class function TUOMAreaUtils.SquareYardsToSquareCentimeters(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 8361.27;
end;

class function TUOMAreaUtils.SquareYardsToSquareFeet(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 9;
end;

class function TUOMAreaUtils.SquareYardsToSquareInches(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 1296;
end;

class function TUOMAreaUtils.SquareYardsToSquareKilometers(
  const ASquareYards: Double): Double;
begin
  Result:= HectaresToSquareKilometers(SquareYardsToHectares(ASquareYards));
end;

class function TUOMAreaUtils.SquareYardsToSquareMeters(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 0.836127;
end;

class function TUOMAreaUtils.SquareYardsToSquareMiles(
  const ASquareYards: Double): Double;
begin
  Result:= AcresToSquareMiles(SquareYardsToAcres(ASquareYards));
end;

class function TUOMAreaUtils.SquareYardsToSquareMillimeters(
  const ASquareYards: Double): Double;
begin
  Result:= ASquareYards * 836127;
end;

{ TUOMArea }

procedure TUOMArea.SetUnit(const Value: TUOMAreaUnit);
begin
  case Value of
    umaSquareMillimeters: FValue:= Self.ToSquareMillimeters;
    umaSquareCentimeters: FValue:= Self.ToSquareCentimeters;
    umaSquareMeters:      FValue:= Self.ToSquareMeters;
    umaHectares:          FValue:= Self.ToHectares;
    umaSquareKilometers:  FValue:= Self.ToSquareKilometers;
    umaSquareInches:      FValue:= Self.ToSquareInches;
    umaSquareFeet:        FValue:= Self.ToSquareFeet;
    umaSquareYards:       FValue:= Self.ToSquareYards;
    umaAcres:             FValue:= Self.ToAcres;
    umaSquareMiles:       FValue:= Self.ToSquareMiles;
  end;
  FUnit:= Value;
end;

procedure TUOMArea.SetValue(const Value: Double);
begin
  FValue := Value;
end;

class operator TUOMArea.implicit(const AValue: TUOMArea): Double;
begin
  Result:= 0;
  case TUOMAreaUnitBaseClass(TUOMAreaUtils.BaseUnit).UnitEnum of
    umaSquareMillimeters: Result:= AValue.ToSquareMillimeters;
    umaSquareCentimeters: Result:= AValue.ToSquareCentimeters;
    umaSquareMeters:      Result:= AValue.ToSquareMeters;
    umaHectares:          Result:= AValue.ToHectares;
    umaSquareKilometers:  Result:= AValue.ToSquareKilometers;
    umaSquareInches:      Result:= AValue.ToSquareInches;
    umaSquareFeet:        Result:= AValue.ToSquareFeet;
    umaSquareYards:       Result:= AValue.ToSquareYards;
    umaAcres:             Result:= AValue.ToAcres;
    umaSquareMiles:       Result:= AValue.ToSquareMiles;
  end;
end;

class operator TUOMArea.implicit(const AValue: Double): TUOMArea;
begin
  Result.FUnit:= TUOMAreaUnitBaseClass(TUOMAreaUtils.BaseUnit).UnitEnum;
  Result.FValue:= AValue;
end;

class operator TUOMArea.implicit(const AValue: TUOMArea): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  //TODO:   Result:= Result + TUOMAreaUtils.UnitSuffix(AValue.&Unit);
end;

class operator TUOMArea.implicit(const AValue: String): TUOMArea;
begin
  //TODO: Parse string...

end;

function TUOMArea.ToSquareMillimeters: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= FValue; //Same
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareMillimeters(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareMillimeters(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareMillimeters(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareMillimeters(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareMillimeters(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareMillimeters(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareMillimeters(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareMillimeters(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareMillimeters(FValue);
  end;
end;

function TUOMArea.ToSquareCentimeters: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareCentimeters(FValue);
    umaSquareCentimeters: Result:= FValue; //Same
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareCentimeters(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareCentimeters(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareCentimeters(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareCentimeters(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareCentimeters(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareCentimeters(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareCentimeters(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareCentimeters(FValue);
  end;
end;

function TUOMArea.ToSquareMeters: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareMeters(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareMeters(FValue);
    umaSquareMeters:      Result:= FValue; //Same
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareMeters(Fvalue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareMeters(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareMeters(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareMeters(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareMeters(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareMeters(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareMeters(FValue);
  end;
end;

function TUOMArea.ToHectares: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToHectares(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToHectares(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToHectares(FValue);
    umaHectares:          Result:= FValue; //Same
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToHectares(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToHectares(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToHectares(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToHectares(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToHectares(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToHectares(FValue);
  end;
end;

function TUOMArea.ToSquareKilometers: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareKilometers(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareKilometers(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareKilometers(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareKilometers(FValue);
    umaSquareKilometers:  Result:= FValue; //Same
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareKilometers(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareKilometers(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareKilometers(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareKilometers(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareKilometers(FValue);
  end;
end;

function TUOMArea.ToSquareInches: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareInches(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareInches(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareInches(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareInches(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareInches(Fvalue);
    umaSquareInches:      Result:= FValue; //Same
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareInches(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareInches(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareInches(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareInches(FValue);
  end;
end;

function TUOMArea.ToSquareFeet: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareFeet(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareFeet(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareFeet(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareFeet(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareFeet(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareFeet(FValue);
    umaSquareFeet:        Result:= FValue; //Same
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareFeet(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareFeet(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareFeet(FValue);
  end;
end;

function TUOMArea.ToSquareYards: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareYards(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareYards(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareYards(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareYards(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareYards(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareYards(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareYards(FValue);
    umaSquareYards:       Result:= FValue; //Same
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareYards(FValue);
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToSquareYards(FValue);
  end;
end;

function TUOMArea.ToAcres: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToAcres(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToAcres(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToAcres(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToAcres(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToAcres(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToAcres(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToAcres(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToAcres(FValue);
    umaAcres:             Result:= FValue; //Same
    umaSquareMiles:       Result:= TUOMAreaUtils.SquareMilesToAcres(FValue);
  end;
end;

function TUOMArea.ToSquareMiles: Double;
begin
  Result:= 0;
  case FUnit of
    umaSquareMillimeters: Result:= TUOMAreaUtils.SquareMillimetersToSquareMiles(FValue);
    umaSquareCentimeters: Result:= TUOMAreaUtils.SquareCentimetersToSquareMiles(FValue);
    umaSquareMeters:      Result:= TUOMAreaUtils.SquareMetersToSquareMiles(FValue);
    umaHectares:          Result:= TUOMAreaUtils.HectaresToSquareMiles(FValue);
    umaSquareKilometers:  Result:= TUOMAreaUtils.SquareKilometersToSquareMiles(FValue);
    umaSquareInches:      Result:= TUOMAreaUtils.SquareInchesToSquareMiles(FValue);
    umaSquareFeet:        Result:= TUOMAreaUtils.SquareFeetToSquareMiles(FValue);
    umaSquareYards:       Result:= TUOMAreaUtils.SquareYardsToSquareMiles(FValue);
    umaAcres:             Result:= TUOMAreaUtils.AcresToSquareMiles(FValue);
    umaSquareMiles:       Result:= FValue; //Same
  end;
end;

{ TUOMAreaRect }

procedure TUOMAreaRect.SetLength(const Value: Double);
begin
  FLength := Value;
end;

procedure TUOMAreaRect.SetWidth(const Value: Double);
begin
  FWidth := Value;
end;

function TUOMAreaRect.GetArea: Double;
begin
  //TODO: Convert combined UOMs...
  //Result:= FWidth.Value * FLength.Value;

end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): Double;
begin
  Result:= 0; //TODO
end;

class operator TUOMAreaRect.implicit(
  const AValue: Double): TUOMAreaRect;
begin
  //TODO: Kinda hard to convert from area to width/length dimensions...
  //  Perhaps just get the square root?
end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): String;
begin
  //Result:= AValue.Width + ' x ' + AValue.Length;
end;

class operator TUOMAreaRect.implicit(
  const AValue: String): TUOMAreaRect;
begin
  //TODO: Parse string...

end;

{ TUOMAreaUnitBase }

class function TUOMAreaUnitBase.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMAreaUnitBase.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMAreaUnitBase.NameSingular: String;
begin
  Result:= '';
end;

class function TUOMAreaUnitBase.Prefix: String;
begin
  Result:= '';
end;

class function TUOMAreaUnitBase.Suffix: String;
begin
  Result:= '';
end;

class function TUOMAreaUnitBase.Systems: TUOMSystems;
begin
  Result:= [];
end;

class function TUOMAreaUnitBase.UnitDescription: String;
begin
  Result:= '';
end;

class function TUOMAreaUnitBase.UnitID: String;
begin
  Result:= '';
end;

class function TUOMAreaUnitBase.UOM: TUOMBaseClass;
begin
  Result:= TUOMAreaUtils;
end;

{ TUOMAreaSquareMillimeters }

class function TUOMAreaSquareMillimeters.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 1000000;
end;

class function TUOMAreaSquareMillimeters.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 1000000;
end;

class function TUOMAreaSquareMillimeters.NameSingular: String;
begin
  Result:= 'Square Millimeter';
end;

class function TUOMAreaSquareMillimeters.Suffix: String;
begin
  Result:= 'mm²';
end;

class function TUOMAreaSquareMillimeters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMAreaSquareMillimeters.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareMillimeters;
end;

class function TUOMAreaSquareMillimeters.UnitID: String;
begin
  Result:= '{065A9DAA-4AA5-430A-809A-7EDC997B74AC}';
end;

{ TUOMAreaSquareCentimeters }

class function TUOMAreaSquareCentimeters.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 10000;
end;

class function TUOMAreaSquareCentimeters.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 10000;
end;

class function TUOMAreaSquareCentimeters.NameSingular: String;
begin
  Result:= 'Square Centimeter';
end;

class function TUOMAreaSquareCentimeters.Suffix: String;
begin
  Result:= 'cm²';
end;

class function TUOMAreaSquareCentimeters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMAreaSquareCentimeters.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareCentimeters;
end;

class function TUOMAreaSquareCentimeters.UnitID: String;
begin
  Result:= '{C95A71DD-B1E1-4538-BD31-9241493C6AB5}';
end;

{ TUOMAreaSquareMeters }

class function TUOMAreaSquareMeters.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMAreaSquareMeters.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMAreaSquareMeters.NameSingular: String;
begin
  Result:= 'Square Meter';
end;

class function TUOMAreaSquareMeters.Suffix: String;
begin
  Result:= 'm²';
end;

class function TUOMAreaSquareMeters.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMAreaSquareMeters.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareMeters;
end;

class function TUOMAreaSquareMeters.UnitID: String;
begin
  Result:= '{2206883A-728E-46B3-B06D-EDD216C852B3}';
end;

{ TUOMAreaHectares }

class function TUOMAreaHectares.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue / 10000;
end;

class function TUOMAreaHectares.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue * 10000;
end;

class function TUOMAreaHectares.NameSingular: String;
begin
  Result:= 'Hectare';
end;

class function TUOMAreaHectares.Suffix: String;
begin
  Result:= 'ha';
end;

class function TUOMAreaHectares.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMAreaHectares.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaHectares;
end;

class function TUOMAreaHectares.UnitID: String;
begin
  Result:= '{2BFD8C78-C49C-4EA4-8ECE-2D836E161197}';
end;

{ TUOMAreaSquareKilometers }

class function TUOMAreaSquareKilometers.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 1000000;
end;

class function TUOMAreaSquareKilometers.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 1000000;
end;

class function TUOMAreaSquareKilometers.NameSingular: String;
begin
  Result:= 'Square Kilometer';
end;

class function TUOMAreaSquareKilometers.Suffix: String;
begin
  Result:= 'km²';
end;

class function TUOMAreaSquareKilometers.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMAreaSquareKilometers.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareKilometers;
end;

class function TUOMAreaSquareKilometers.UnitID: String;
begin
  Result:= '{6B9C24F9-AC2F-4AC9-9F77-1D31F8858285}';
end;

{ TUOMAreaSquareInches }

class function TUOMAreaSquareInches.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue * 1550;
end;

class function TUOMAreaSquareInches.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue / 1550;
end;

class function TUOMAreaSquareInches.NamePlural: String;
begin
  Result:= 'Square Inches';
end;

class function TUOMAreaSquareInches.NameSingular: String;
begin
  Result:= 'Square Inch';
end;

class function TUOMAreaSquareInches.Suffix: String;
begin
  Result:= 'in²';
end;

class function TUOMAreaSquareInches.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMAreaSquareInches.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareInches;
end;

class function TUOMAreaSquareInches.UnitID: String;
begin
  Result:= '{F60044CF-80C6-4C6D-9B16-74EEBF77140A}';
end;

{ TUOMAreaSquareFeet }

class function TUOMAreaSquareFeet.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue * 10.7639;
end;

class function TUOMAreaSquareFeet.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue / 10.7639;
end;

class function TUOMAreaSquareFeet.NamePlural: String;
begin
  Result:= 'Square Feet';
end;

class function TUOMAreaSquareFeet.NameSingular: String;
begin
  Result:= 'Square Foot';
end;

class function TUOMAreaSquareFeet.Suffix: String;
begin
  Result:= 'ft²';
end;

class function TUOMAreaSquareFeet.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMAreaSquareFeet.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareFeet;
end;

class function TUOMAreaSquareFeet.UnitID: String;
begin
  Result:= '{0C005487-9E82-4084-91C4-A02847D471C5}';
end;

{ TUOMAreaSquareYards }

class function TUOMAreaSquareYards.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 1.19599;
end;

class function TUOMAreaSquareYards.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue * 1.19599;
end;

class function TUOMAreaSquareYards.NameSingular: String;
begin
  Result:= 'Square Yard';
end;

class function TUOMAreaSquareYards.Suffix: String;
begin
  Result:= 'yd²';
end;

class function TUOMAreaSquareYards.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMAreaSquareYards.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareYards;
end;

class function TUOMAreaSquareYards.UnitID: String;
begin
  Result:= '{FCA85487-7FF3-4B93-9B18-477251D632DE}';
end;

{ TUOMAreaAcres }

class function TUOMAreaAcres.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue / 4046.86;
end;

class function TUOMAreaAcres.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue * 4046.86;
end;

class function TUOMAreaAcres.NameSingular: String;
begin
  Result:= 'Acre';
end;

class function TUOMAreaAcres.Suffix: String;
begin
  Result:= 'ac';
end;

class function TUOMAreaAcres.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMAreaAcres.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaAcres;
end;

class function TUOMAreaAcres.UnitID: String;
begin
  Result:= '{D437D166-C4C1-450D-918E-F74B34571D3E}';
end;

{ TUOMAreaSquareMiles }

class function TUOMAreaSquareMiles.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue / 2589988.110336;
end;

class function TUOMAreaSquareMiles.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue * 2589988.110336;
end;

class function TUOMAreaSquareMiles.NameSingular: String;
begin
  Result:= 'Square Mile';
end;

class function TUOMAreaSquareMiles.Suffix: String;
begin
  Result:= 'mi²';
end;

class function TUOMAreaSquareMiles.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMAreaSquareMiles.UnitEnum: TUOMAreaUnit;
begin
  Result:= umaSquareMiles;
end;

class function TUOMAreaSquareMiles.UnitID: String;
begin
  Result:= '{3F678D38-D7DA-49BC-AAE1-2AE6C9670E04}';
end;

initialization
  _:= nil;
end.
