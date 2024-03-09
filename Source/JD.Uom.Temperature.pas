unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
begin

  TUOMUtils.RegisterUOM('Temperature',
    'Celsius', 'Celsius', '', '°C', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    'Value', 'Value'
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Temperature',
    'Farenheit', 'Farenheit', '', '°F', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    '(Value * 1.8) + 32', '(Value - 32) / 1.8'
  {$ELSE}
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
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Temperature',
    'Kelvin', 'Kelvin', '', '°K', 'Natural',
  {$IFDEF USE_MATH_EXPR}
    'Value + 272.15', 'Value - 272.15'
  {$ELSE}
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
  {$ENDIF}
  );

end;

initialization
  RegisterUOM;
end.
