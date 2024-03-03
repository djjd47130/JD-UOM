unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

uses
  System.Math;

procedure RegisterUOM;
var
  Base: TUOMLookupUnit;
begin

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Nanometer', 'Nanometers', '', 'nm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Nanometers
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Nanometers to Meters
      Result:= Value / 1000000;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Micron', 'Microns', '', 'μm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Microns
      Result:= Value * 1000000000;
    end,
    function(const Value: Double): Double
    begin
      //Microns to Meters
      Result:= Value / 1000000000;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Millimeter', 'Millimeters', '', 'mm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Millimeters
      Result:= Value * 1000;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters to Meters
      Result:= Value / 1000;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Centimeter', 'Centimeters', '', 'cm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Centimeters
      Result:= Value * 100;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters to Meters
      Result:= Value / 100;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Decimeter', 'Decimeters', '', 'dm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Decimeters
      Result:= Value * 10;
    end,
    function(const Value: Double): Double
    begin
      //Decimeters to Meters
      Result:= Value / 10;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Meter', 'Meters', '', 'm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Meters
      Result:= Value;
    end,
    function(const Value: Double): Double
    begin
      //Meters to Meters
      Result:= Value;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Decameter', 'Decameters', '', 'dam', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Decameters
      Result:= Value / 10;
    end,
    function(const Value: Double): Double
    begin
      //Decameters to Meters
      Result:= Value * 10;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Hectometer', 'Hectometers', '', 'hm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Hectometers
      Result:= Value / 100;
    end,
    function(const Value: Double): Double
    begin
      //Hectometers to Meters
      Result:= Value * 100;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Kilometer', 'Kilometers', '', 'km', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Kilometers
      Result:= Value / 1000;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers to Meters
      Result:= Value * 1000;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Inch', 'Inches', '', '"', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Inches
      Result:= Value * 39.3701;
    end,
    function(const Value: Double): Double
    begin
      //Inches to Meters
      Result:= Value / 39.3701;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Foot', 'Feet', '', '''', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Feet
      Result:= Value * 3.28084;
    end,
    function(const Value: Double): Double
    begin
      //Feet to Meters
      Result:= Value / 3.28084;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Yard', 'Yards', '', 'yd', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Yards
      Result:= Value * 1.0936133;
    end,
    function(const Value: Double): Double
    begin
      //Yards to Meters
      Result:= Value / 1.0936133;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Fathom', 'Fathoms', '', 'fath', 'Imperial',
    function(const Value: Double): Double
    begin
      //Meters to Fathoms
      Result:= Value * 1.8288; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Fathoms to Meters
      Result:= Value / 1.8288; //TODO: Correct?
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Rod', 'Rods', '', 'rd', 'Imperial',
    function(const Value: Double): Double
    begin
      //Meters to Rods
      Result:= Value / 5.0292; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Rods to Meters
      Result:= Value * 5.0292; //TODO: Correct?
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Furlong', 'Furlongs', '', 'fur', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Furlongs
      Result:= Value / 201.168; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Furlongs to Meters
      Result:= Value * 201.168; //TODO: Correct?
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Mile', 'Miles', '', 'mi', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Miles
      Result:= Value / 1609.344;
    end,
    function(const Value: Double): Double
    begin
      //Miles to Meters
      Result:= Value * 1609.344;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Nautical Mile', 'Nautical Miles', '', 'nmi', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Meters to Nautical Miles
      Result:= Value / 1852;
    end,
    function(const Value: Double): Double
    begin
      //Nautical Miles to Meters
      Result:= Value * 1852;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Light Year', 'Light Years', '', 'ly', 'Natural',
    function(const Value: Double): Double
    begin
      //Meters to Light Years
      Result:= Value / 9460730472580800;
    end,
    function(const Value: Double): Double
    begin
      //Light Years to Meters
      Result:= Value * 9460730472580800;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'Banana', 'Bananas', '', 'ban', 'Random',
    function(const Value: Double): Double
    begin
      //Meters to Bananas
      //1 Banana = 8 Inches
      Result:= Value / 0.254;
    end,
    function(const Value: Double): Double
    begin
      //Bananas to Meters
      Result:= Value * 0.254;
    end
  ));

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Distance',
    'iPhone 14 Pro Max', 'iPhone 14 Pro Maxes', '', 'iP14PM', 'Random',
    function(const Value: Double): Double
    begin
      //Meters to iPhone 14 Pro Maxes
      Result:= Value / 0.1607;
    end,
    function(const Value: Double): Double
    begin
      //iPhone 14 Pro Maxes to Meters
      Result:= Value * 0.1607;
    end
  ));

  Base:= TUOMLookupTable.GetUnitByName('Meter');
  TUOMLookupTable.RegisterBaseUnit(Base.UOM, Base);

end;

initialization
  RegisterUOM;
end.
