unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMTemperatureUnit = (umtCelsius, umtFarenheit, umtKelvin);
  TUOMTemperatureUnits = set of TUOMTemperatureUnit;

  TUOMTemperatureUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMTemperatureUnit): String; static;
    class function UnitName(const AValue: TUOMTemperatureUnit): String; static;

    class function FarenheitToCelcius(const AFarenheit: Double): Double; static;
    class function KelvinToCelcius(const AKelvin: Double): Double; static;

    class function CelciusToFarenheit(const ACelcius: Double): Double; static;
    class function KelvinToFarenheit(const AKelvin: Double): Double; static;

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
    function ToCelcius: Double;
    function ToFarenheit: Double;
    function ToKelvin: Double;
  end;

implementation

var
  DefaultTemperatureUnit: TUOMTemperatureUnit;

{ TUOMTemperatureUtils }

class procedure TUOMTemperatureUtils.UnitList(AList: TStrings);
var
  V: TUOMTemperatureUnit;
begin
  AList.Clear;
  for V:= Low(TUOMTemperatureUnit) to High(TUOMTemperatureUnit) do begin
    AList.Append(TUOMTemperatureUtils.UnitName(V));
  end;
end;

class function TUOMTemperatureUtils.UnitName(
  const AValue: TUOMTemperatureUnit): String;
begin
  case AValue of
    umtCelsius:   Result:= 'Celsius';
    umtFarenheit: Result:= 'Farenheit';
    umtKelvin:    Result:= 'Kelvin';
  end;
end;

class function TUOMTemperatureUtils.UnitSuffix(
  const AValue: TUOMTemperatureUnit): String;
begin
  case AValue of
    umtCelsius:   Result:= '°C';
    umtFarenheit: Result:= '°F';
    umtKelvin:    Result:= '°K';
  end;
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
    umtCelsius:   Result:= AValue.ToCelcius;
    umtFarenheit: Result:= AValue.ToFarenheit;
    umtKelvin:    Result:= AValue.ToKelvin;
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
  Result:= Result + TUOMTemperatureUtils.UnitSuffix(AValue.&Unit);
end;

class operator TUOMTemperature.implicit(const AValue: String): TUOMTemperature;
begin
  //TODO: Parse string...

end;

procedure TUOMTemperature.SetUnit(const Value: TUOMTemperatureUnit);
begin
  case FUnit of
    umtCelsius:   FValue:= Self.ToCelcius;
    umtFarenheit: FValue:= Self.ToFarenheit;
    umtKelvin:    FValue:= Self.ToKelvin;
  end;
  FUnit:= Value;
end;

procedure TUOMTemperature.SetValue(const Value: Double);
begin
  FValue:= Value;
end;

function TUOMTemperature.ToCelcius: Double;
begin
  Result:= 0;
  case FUnit of
    umtCelsius:   Result:= FValue; //Same
    umtFarenheit: Result:= TUOMTemperatureUtils.FarenheitToCelcius(FValue);
    umtKelvin:    Result:= TUOMTemperatureUtils.KelvinToCelcius(FValue);
  end;
end;

function TUOMTemperature.ToFarenheit: Double;
begin
  Result:= 0;
  case FUnit of
    umtCelsius:   Result:= TUOMTemperatureUtils.CelciusToFarenheit(FValue);
    umtFarenheit: Result:= FValue; //Same
    umtKelvin:    Result:= TUOMTemperatureUtils.KelvinToFarenheit(FValue);
  end;
end;

function TUOMTemperature.ToKelvin: Double;
begin
  Result:= 0;
  case FUnit of
    umtCelsius:   Result:= TUOMTemperatureUtils.CelciusToKelvin(FValue);
    umtFarenheit: Result:= TUOMTemperatureUtils.FarenheitToKelvin(FValue);
    umtKelvin:    Result:= FValue; //Same
  end;
end;

initialization
  DefaultTemperatureUnit:= TUOMTemperatureUnit.umtFarenheit;
end.
