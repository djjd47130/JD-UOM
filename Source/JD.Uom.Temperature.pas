unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMTemperatureUnit = (umtCelsius, umtFarenheit, umtKelvin);
  TUOMTemperatureUnits = set of TUOMTemperatureUnit;

  TUOMTemperatureUtils = class(TUOMBase)
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
  SetLength(FUnits, 0);
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMTemperatureUtils.Destroy;
begin
  SetLength(FUnits, 0);
end;

class function TUOMTemperatureUtils.GetUnit(const Index: Integer): TUOMUnitInfo;
begin
  Result:= FUnits[Index];
end;

class procedure TUOMTemperatureUtils.RegisterUnit(const Name: String;
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

class procedure TUOMTemperatureUtils.RegisterUnits;
begin
  RegisterUnit('Celcius',     [ustMetric],  '',   '°C');
  RegisterUnit('Farenheit',   [ustImperial, ustUSCustomary],  '',   '°F');
  RegisterUnit('Kelvin',      [ustMetric],  '',   '°K');
end;

class procedure TUOMTemperatureUtils.RegisterUOM;
begin
  TUOMList.RegisterUOM(TUOMTemperatureUtils);
end;

class function TUOMTemperatureUtils.UnitCount: Integer;
begin
  Result:= Length(FUnits);
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

initialization
  _:= nil;
  DefaultTemperatureUnit:= TUOMTemperatureUnit.umtFarenheit;
end.
