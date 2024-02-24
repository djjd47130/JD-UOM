unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

type
  TUOMTemperatureUnit = (umtCelsius, umtFarenheit, umtKelvin);
  TUOMTemperatureUnits = set of TUOMTemperatureUnit;

  TUOMTemperatureCelcius = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function UnitName: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; static;
  end;

  TUOMTemperatureFarenheit = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function UnitName: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; static;
  end;

  TUOMTemperatureKelvin = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function UnitName: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMTemperatureUnit; static;
  end;

  TUOMTemperatureUtils = class(TUOMBase)
  private
    class var FUnits: TList<TUOMUnitBaseClass>;
  private
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

    //Celcius
    class function FarenheitToCelcius(const AFarenheit: Double): Double; static;
    class function KelvinToCelcius(const AKelvin: Double): Double; static;
    //Farenheit
    class function CelciusToFarenheit(const ACelcius: Double): Double; static;
    class function KelvinToFarenheit(const AKelvin: Double): Double; static;
    //Kelvin
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
  public
    function GetAsCelcius: TUOMTemperature;
    function GetAsFarenheit: TUOMTemperature;
    function GetAsKelvin: TUOMTemperature;

    property AsCelcius: TUOMTemperature read GetAsCelcius;
    property AsFarenheit: TUOMTemperature read GetAsFarenheit;
    property AsKelvin: TUOMTemperature read GetAsKelvin;
  end;

implementation

var
  DefaultTemperatureUnit: TUOMTemperatureUnit;
  _: TUOMTemperatureUtils;

{ TUOMTemperatureUtils }

class constructor TUOMTemperatureUtils.Create;
begin
  FUnits:= TList<TUOMUnitBaseClass>.Create;
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMTemperatureUtils.Destroy;
begin
  FreeAndNil(FUnits);
end;

class function TUOMTemperatureUtils.GetUnit(
  const Index: Integer): TUOMUnitBaseClass;
begin
  Result:= FUnits[Index];
end;

class procedure TUOMTemperatureUtils.RegisterUnit(
  AUnitClass: TUOMUnitBaseClass);
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

class function TUOMTemperatureUtils.CelciusToFarenheit(const ACelcius: Double): Double;
begin
  Result:= (1.8 * ACelcius) + 32;
end;

class function TUOMTemperatureUtils.FarenheitToCelcius(const AFarenheit: Double): Double;
begin
  Result:= (AFarenheit - 32) / 1.8;
end;

class function TUOMTemperatureUtils.CelciusToKelvin(const ACelcius: Double): Double;
begin
  Result:= ACelcius + 273.15;
end;

class function TUOMTemperatureUtils.FarenheitToKelvin(const AFarenheit: Double): Double;
begin
  Result:= CelciusToKelvin(FarenheitToCelcius(AFarenheit));
end;

class function TUOMTemperatureUtils.KelvinToCelcius(const AKelvin: Double): Double;
begin
  Result:= AKelvin - 273.15;
end;

class function TUOMTemperatureUtils.KelvinToFarenheit(const AKelvin: Double): Double;
begin
  Result:= CelciusToFarenheit(KelvinToCelcius(AKelvin));
end;

{ TUOMTemperature }

class operator TUOMTemperature.implicit(const AValue: TUOMTemperature): Double;
begin
  Result:= 0;
  case DefaultTemperatureUnit of
    umtCelsius:   Result:= AValue.GetAsCelcius;
    umtFarenheit: Result:= AValue.GetAsFarenheit;
    umtKelvin:    Result:= AValue.GetAsKelvin;
  end;
end;

class operator TUOMTemperature.implicit(const AValue: Double): TUOMTemperature;
begin
  Result.FUnit:= DefaultTemperatureUnit;
  Result.FValue:= AValue;
end;

class operator TUOMTemperature.implicit(const AValue: TUOMTemperature): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  //TODO: Result:= Result + TUOMTemperatureUtils.UnitSuffix(AValue.&Unit);
end;

class operator TUOMTemperature.implicit(const AValue: String): TUOMTemperature;
begin
  //TODO: Parse string...

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

{ TUOMTemperatureCelcius }

class function TUOMTemperatureCelcius.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureCelcius.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMTemperatureCelcius.Prefix: String;
begin
  Result:= '';
end;

class function TUOMTemperatureCelcius.Suffix: String;
begin
  Result:= '°C';
end;

class function TUOMTemperatureCelcius.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMTemperatureCelcius.UnitDescription: String;
begin
  Result:= ''; //TODO
end;

class function TUOMTemperatureCelcius.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtCelsius;
end;

class function TUOMTemperatureCelcius.UnitID: String;
begin
  Result:= '{F4D3B012-EA64-4FB0-BA19-1A897C6945B5}';
end;

class function TUOMTemperatureCelcius.UnitName: String;
begin
  Result:= 'Celcius';
end;

class function TUOMTemperatureCelcius.UOM: TUOMBaseClass;
begin
  Result:= TUOMTemperatureUtils;
end;

{ TUOMTemperatureFarenheit }

class function TUOMTemperatureFarenheit.ConvertFromBase(const AValue: Double): Double;
begin

end;

class function TUOMTemperatureFarenheit.ConvertToBase(const AValue: Double): Double;
begin

end;

class function TUOMTemperatureFarenheit.Prefix: String;
begin
  Result:= '';
end;

class function TUOMTemperatureFarenheit.Suffix: String;
begin
  Result:= '°F';
end;

class function TUOMTemperatureFarenheit.Systems: TUOMSystems;
begin
  Result:= [ustImperial, ustUSCustomary];
end;

class function TUOMTemperatureFarenheit.UnitDescription: String;
begin
  Result:= ''; //TODO
end;

class function TUOMTemperatureFarenheit.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtFarenheit;
end;

class function TUOMTemperatureFarenheit.UnitID: String;
begin
  Result:= '{8E5F200E-D533-4BD4-8A6C-88BBB7E55E0F}';
end;

class function TUOMTemperatureFarenheit.UnitName: String;
begin
  Result:= 'Farenheit';
end;

class function TUOMTemperatureFarenheit.UOM: TUOMBaseClass;
begin
  Result:= TUOMTemperatureUtils;
end;

{ TUOMTemperatureKelvin }

class function TUOMTemperatureKelvin.ConvertFromBase(const AValue: Double): Double;
begin

end;

class function TUOMTemperatureKelvin.ConvertToBase(const AValue: Double): Double;
begin

end;

class function TUOMTemperatureKelvin.Prefix: String;
begin
  Result:= '';
end;

class function TUOMTemperatureKelvin.Suffix: String;
begin
  Result:= '°K';
end;

class function TUOMTemperatureKelvin.Systems: TUOMSystems;
begin
  Result:= [ustNatural];
end;

class function TUOMTemperatureKelvin.UnitDescription: String;
begin
  Result:= ''; //TODO
end;

class function TUOMTemperatureKelvin.UnitEnum: TUOMTemperatureUnit;
begin
  Result:= umtKelvin;
end;

class function TUOMTemperatureKelvin.UnitID: String;
begin
  Result:= '{A5A247C4-DCAC-41AF-A2F5-412E14974DE7}';
end;

class function TUOMTemperatureKelvin.UnitName: String;
begin
  Result:= 'Kelvin';
end;

class function TUOMTemperatureKelvin.UOM: TUOMBaseClass;
begin
  Result:= TUOMTemperatureUtils;
end;

initialization
  _:= nil;
  DefaultTemperatureUnit:= TUOMTemperatureUnit.umtFarenheit;
end.
