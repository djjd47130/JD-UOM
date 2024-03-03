unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
var
  Base: TUOMLookupUnit;
begin

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature',
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

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature',
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

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Temperature',
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

initialization
  RegisterUOM;
end.
