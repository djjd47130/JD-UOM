unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

type
  TUOMTemperatureUnit = (umtCelsius, umtFarenheit, umtKelvin);
  TUOMTemperatureUnits = set of TUOMTemperatureUnit;

  TUOMTemperatureUnitBase = class;
  TUOMTemperatureUtils = class;

  TUOMTemperatureUnitBaseClass = class of TUOMTemperatureUnitBase;

  TUOMTemperatureUnitBase = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; virtual; abstract;
  end;

  //Specific Temperature Units

  TUOMTemperatureCelcius = class(TUOMTemperatureUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function NamePlural: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; override;
  end;

  TUOMTemperatureFarenheit = class(TUOMTemperatureUnitBase)
    class function UnitID: String; override;
    class function NamePlural: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; override;
  end;

  TUOMTemperatureKelvin = class(TUOMTemperatureUnitBase)
    class function UnitID: String; override;
    class function NamePlural: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; override;
  end;

  TUOMTemperatureUtils = class(TUOMBase)
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

    class function UnitByEnum(const AUnit: TUOMTemperatureUnit): TUOMTemperatureUnitBaseClass;
    class function Convert(const AValue: Double; const AFromUnit,
      AToUnit: TUOMTemperatureUnit): Double;

    //Celcius (Metric)
    class function FarenheitToCelcius(const AFarenheit: Double): Double; static;
    class function KelvinToCelcius(const AKelvin: Double): Double; static;
    //Farenheit (Imperial, US Customary)
    class function CelciusToFarenheit(const ACelcius: Double): Double; static;
    class function KelvinToFarenheit(const AKelvin: Double): Double; static;
    //Kelvin (Natural)
    class function CelciusToKelvin(const ACelcius: Double): Double; static;
    class function FarenheitToKelvin(const AFarenheit: Double): Double; static;

  end;

  TUOMTemperature = record
  private
    FUnit: TUOMTemperatureUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMTemperatureUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMTemperatureUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMTemperature;
    class operator implicit(const AValue: TUOMTemperature): Double;
    class operator implicit(const AValue: String): TUOMTemperature;
    class operator implicit(const AValue: TUOMTemperature): String;
    //TODO: Implement class operators for math...
  public
    function GetAsCelcius: TUOMTemperature;
    function GetAsFarenheit: TUOMTemperature;
    function GetAsKelvin: TUOMTemperature;
    procedure SetAsCelcius(const AValue: TUOMTemperature);
    procedure SetAsFarenheit(const AValue: TUOMTemperature);
    procedure SetAsKelvin(const AValue: TUOMTemperature);

    property AsCelcius: TUOMTemperature read GetAsCelcius write SetAsCelcius;
    property AsFarenheit: TUOMTemperature read GetAsFarenheit write SetAsFarenheit;
    property AsKelvin: TUOMTemperature read GetAsKelvin write SetAsKelvin;
  end;

implementation

var
  _: TUOMTemperatureUtils;

procedure RegisterUOM;
var
  Base: TUOMLookupUnit;
begin

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature', '{7409AC05-EEFC-4748-957D-17EB0CF5A278}',
    'Celsius', 'Celsius', '', '°C', 'Metric',
    function(const Value: Double): Double
    begin
      //Celsius to Celsius
      Result:= Value;
    end,
    function(const Value: Double): Double
    begin
      //Celsius to Celsius
      Result:= Value;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature', '{8C93353D-5586-42F7-9122-03296C5BC8C1}',
    'Farenheit', 'Farenheit', '', '°F', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Celsius to Farenheit
      Result:= (Value * 1.8) + 32;
    end,
    function(const Value: Double): Double
    begin
      //Farenheit to Celsius
      Result:= (Value - 32) / 1.8;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature', '{171F2412-624A-4126-8B97-EFAA4A160D91}',
    'Kelvin', 'Kelvin', '', '°K', 'Natural',
    function(const Value: Double): Double
    begin
      //Celsius to Kelvin
      Result:= Value + 272.15;
    end,
    function(const Value: Double): Double
    begin
      //Kelvin to Celsius
      Result:= Value - 272.15;
    end
  ));

  Base:= TUOMLookupTable.GetUnitByName('Celsius');
  TUOMLookupTable.RegisterBaseUnit(Base.UOM, Base);

end;

{ TUOMTemperatureUtils }

class constructor TUOMTemperatureUtils.Create;
begin
  FUnits:= TList<TUOMUnitClass>.Create;
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMTemperatureUtils.Destroy;
begin
  FreeAndNil(FUnits);
end;

class function TUOMTemperatureUtils.GetUnit(
  const Index: Integer): TUOMUnitClass;
begin
  Result:= FUnits[Index];
end;

class procedure TUOMTemperatureUtils.RegisterUnit(
  AUnitClass: TUOMUnitClass);
begin
  FUnits.Add(AUnitClass);
end;

class procedure TUOMTemperatureUtils.RegisterUnits;
begin
  RegisterUnit(TUOMTemperatureCelcius);
  RegisterUnit(TUOMTemperatureFarenheit);
  RegisterUnit(TUOMTemperatureKelvin);
end;

class procedure TUOMTemperatureUtils.RegisterUOM;
begin
  TUOMUtils.RegisterUOM(TUOMTemperatureUtils);
end;

class function TUOMTemperatureUtils.UnitByEnum(
  const AUnit: TUOMTemperatureUnit): TUOMTemperatureUnitBaseClass;
var
  X: Integer;
  U: TUOMTemperatureUnitBaseClass;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    U:= TUOMTemperatureUnitBaseClass(FUnits[X]);
    if U.UnitEnum = AUnit then begin
      Result:= U;
      Break;
    end;
  end;
end;

class function TUOMTemperatureUtils.Convert(const AValue: Double;
  const AFromUnit, AToUnit: TUOMTemperatureUnit): Double;
var
  F, T: TUOMTemperatureUnitBaseClass;
begin
  F:= UnitByEnum(AFromUnit);
  T:= UnitByEnum(AToUnit);
  Result:= F.ConvertToBase(AValue);
  Result:= T.ConvertFromBase(Result);
end;

class function TUOMTemperatureUtils.UnitCount: Integer;
begin
  Result:= FUnits.Count;
end;

class function TUOMTemperatureUtils.UOMID: String;
begin
  Result:= '{910B9CA0-ABAF-4DC4-BAD5-D0ACF1040FC8}';
end;

class function TUOMTemperatureUtils.UOMName: String;
begin
  Result:= 'Temperature';
end;

class function TUOMTemperatureUtils.BaseUnit: TUOMUnitClass;
begin
  Result:= TUOMTemperatureCelcius;
end;

class function TUOMTemperatureUtils.CelciusToFarenheit(const ACelcius: Double): Double;
begin
  Result:= Convert(ACelcius, umtCelsius, umtFarenheit);
end;

class function TUOMTemperatureUtils.FarenheitToCelcius(const AFarenheit: Double): Double;
begin
  Result:= Convert(AFarenheit, umtFarenheit, umtCelsius);
end;

class function TUOMTemperatureUtils.CelciusToKelvin(const ACelcius: Double): Double;
begin
  Result:= Convert(ACelcius, umtCelsius, umtKelvin);
end;

class function TUOMTemperatureUtils.FarenheitToKelvin(const AFarenheit: Double): Double;
begin
  Result:= Convert(AFarenheit, umtFarenheit, umtKelvin);
end;

class function TUOMTemperatureUtils.KelvinToCelcius(const AKelvin: Double): Double;
begin
  Result:= Convert(AKelvin, umtKelvin, umtCelsius);
end;

class function TUOMTemperatureUtils.KelvinToFarenheit(const AKelvin: Double): Double;
begin
  Result:= Convert(AKelvin, umtKelvin, umtFarenheit);
end;

{ TUOMTemperature }

class operator TUOMTemperature.implicit(const AValue: TUOMTemperature): Double;
begin
  Result:= 0;
  case TUOMTemperatureUnitBaseClass(TUOMTemperatureUtils.BaseUnit).UnitEnum of
    umtCelsius:   Result:= AValue.GetAsCelcius;
    umtFarenheit: Result:= AValue.GetAsFarenheit;
    umtKelvin:    Result:= AValue.GetAsKelvin;
  end;
end;

class operator TUOMTemperature.implicit(const AValue: Double): TUOMTemperature;
begin
  Result.FUnit:= TUOMTemperatureUnitBaseClass(TUOMTemperatureUtils.BaseUnit).UnitEnum;
  Result.FValue:= AValue;
end;

class operator TUOMTemperature.implicit(const AValue: TUOMTemperature): String;
var
  U: TUOMTemperatureUnitBaseClass;
begin
  U:= TUOMTemperatureUtils.UnitByEnum(AValue.FUnit);
  Result:= FormatFloat(NumFormat, AValue.FValue);
  Result:= Result + U.Suffix;
end;

class operator TUOMTemperature.implicit(const AValue: String): TUOMTemperature;
begin
  //TODO: Parse string...

end;

procedure TUOMTemperature.SetAsCelcius(const AValue: TUOMTemperature);
begin
  FUnit:= umtCelsius;
  case FUnit of
    umtCelsius:     FValue:= AValue;
    umtFarenheit:   FValue:= TUOMTemperatureUtils.CelciusToFarenheit(AValue);
    umtKelvin:      FValue:= TUOMTemperatureUtils.CelciusToKelvin(AValue);
  end;
end;

procedure TUOMTemperature.SetAsFarenheit(const AValue: TUOMTemperature);
begin
  FUnit:= umtFarenheit;
  case FUnit of
    umtCelsius:     FValue:= TUOMTemperatureUtils.FarenheitToCelcius(AValue);
    umtFarenheit:   FValue:= AValue;
    umtKelvin:      FValue:= TUOMTemperatureUtils.FarenheitToKelvin(AValue);
  end;
end;

procedure TUOMTemperature.SetAsKelvin(const AValue: TUOMTemperature);
begin
  FUnit:= umtKelvin;
  case FUnit of
    umtCelsius:     FValue:= TUOMTemperatureUtils.KelvinToCelcius(AValue);
    umtFarenheit:   FValue:= TUOMTemperatureUtils.KelvinToFarenheit(AValue);
    umtKelvin:      FValue:= AValue;
  end;
end;

procedure TUOMTemperature.SetUnit(const Value: TUOMTemperatureUnit);
begin
  case FUnit of
    umtCelsius:   FValue:= Self.GetAsCelcius;
    umtFarenheit: FValue:= Self.GetAsFarenheit;
    umtKelvin:    FValue:= Self.GetAsKelvin;
  end;
  FUnit:= Value;
end;

procedure TUOMTemperature.SetValue(const Value: Double);
begin
  FValue:= Value;
end;

function TUOMTemperature.GetAsCelcius: TUOMTemperature;
begin
  Result.FUnit:= umtCelsius;
  Result.FValue:= 0;
  case FUnit of
    umtCelsius:   Result.FValue:= FValue; //Same
    umtFarenheit: Result.FValue:= TUOMTemperatureUtils.FarenheitToCelcius(FValue);
    umtKelvin:    Result.FValue:= TUOMTemperatureUtils.KelvinToCelcius(FValue);
  end;
end;

function TUOMTemperature.GetAsFarenheit: TUOMTemperature;
begin
  Result.FUnit:= umtFarenheit;
  Result.FValue:= 0;
  case FUnit of
    umtCelsius:   Result.FValue:= TUOMTemperatureUtils.CelciusToFarenheit(FValue);
    umtFarenheit: Result.FValue:= FValue; //Same
    umtKelvin:    Result.FValue:= TUOMTemperatureUtils.KelvinToFarenheit(FValue);
  end;
end;

function TUOMTemperature.GetAsKelvin: TUOMTemperature;
begin
  Result.FUnit:= umtKelvin;
  Result.FValue:= 0;
  case FUnit of
    umtCelsius:   Result.FValue:= TUOMTemperatureUtils.CelciusToKelvin(FValue);
    umtFarenheit: Result.FValue:= TUOMTemperatureUtils.FarenheitToKelvin(FValue);
    umtKelvin:    Result.FValue:= FValue; //Same
  end;
end;

{ TUOMTemperatureUnitBase }

class function TUOMTemperatureUnitBase.ConvertFromBase(
  const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureUnitBase.ConvertToBase(
  const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureUnitBase.Prefix: String;
begin
  Result:= '';
end;

class function TUOMTemperatureUnitBase.Suffix: String;
begin
  Result:= '';
end;

class function TUOMTemperatureUnitBase.Systems: TUOMSystems;
begin
  Result:= [];
end;

class function TUOMTemperatureUnitBase.UnitDescription: String;
begin
  Result:= '';
end;

class function TUOMTemperatureUnitBase.UnitID: String;
begin
  Result:= '';
end;

class function TUOMTemperatureUnitBase.NameSingular: String;
begin
  Result:= '';
end;

class function TUOMTemperatureUnitBase.UOM: TUOMBaseClass;
begin
  Result:= TUOMTemperatureUtils;
end;

{ TUOMTemperatureCelcius }

class function TUOMTemperatureCelcius.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureCelcius.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureCelcius.Suffix: String;
begin
  Result:= '°C';
end;

class function TUOMTemperatureCelcius.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMTemperatureCelcius.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtCelsius;
end;

class function TUOMTemperatureCelcius.UnitID: String;
begin
  Result:= '{F4D3B012-EA64-4FB0-BA19-1A897C6945B5}';
end;

class function TUOMTemperatureCelcius.NamePlural: String;
begin
  Result:= 'Celcius';
end;

class function TUOMTemperatureCelcius.NameSingular: String;
begin
  Result:= 'Celcius';
end;

{ TUOMTemperatureFarenheit }

class function TUOMTemperatureFarenheit.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= (AValue * 1.8) + 32;
end;

class function TUOMTemperatureFarenheit.ConvertToBase(const AValue: Double): Double;
begin
  Result:= (AValue - 32) / 1.8;
end;

class function TUOMTemperatureFarenheit.Suffix: String;
begin
  Result:= '°F';
end;

class function TUOMTemperatureFarenheit.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMTemperatureFarenheit.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtFarenheit;
end;

class function TUOMTemperatureFarenheit.UnitID: String;
begin
  Result:= '{8E5F200E-D533-4BD4-8A6C-88BBB7E55E0F}';
end;

class function TUOMTemperatureFarenheit.NamePlural: String;
begin
  Result:= 'Farenheit';
end;

class function TUOMTemperatureFarenheit.NameSingular: String;
begin
  Result:= 'Farenheit';
end;

{ TUOMTemperatureKelvin }

class function TUOMTemperatureKelvin.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue + 272.15;
end;

class function TUOMTemperatureKelvin.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue - 272.15;
end;

class function TUOMTemperatureKelvin.Suffix: String;
begin
  Result:= '°K';
end;

class function TUOMTemperatureKelvin.Systems: TUOMSystems;
begin
  Result:= [ustNatural];
end;

class function TUOMTemperatureKelvin.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtKelvin;
end;

class function TUOMTemperatureKelvin.UnitID: String;
begin
  Result:= '{A5A247C4-DCAC-41AF-A2F5-412E14974DE7}';
end;

class function TUOMTemperatureKelvin.NamePlural: String;
begin
  Result:= 'Kelvin';
end;

class function TUOMTemperatureKelvin.NameSingular: String;
begin
  Result:= 'Kelvin';
end;

initialization
  RegisterUOM;
  _:= nil;
end.
