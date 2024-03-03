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
    '{077930C4-8ED2-444E-8053-24899B197F00}',
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
    '{B0001BAD-960B-463A-9545-07DE3F229BBD}',
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
    '{815B7612-7FD9-4325-97A6-07A7F32A1B0B}',
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
    '{E637DBDF-DA82-4FB1-85B3-87EA5DDB772A}',
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
    '{CDCFC5F0-4B37-4D18-B6D6-46CF71BF54BA}',
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
    '{CB30CEB3-C3D2-4862-A081-A27DA5E33683}',
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
    '{2CD91B24-C767-4784-85BD-653E294399F4}',
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
    '{B4D3A305-EEFC-4E92-9407-493D29973DEA}',
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
    '{8927581C-B9DF-4DD4-B52B-4E32F99DE9C2}',
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
    '{E2A5BAAE-915B-40B6-B15A-355E84992288}',
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
    '{8D8C41D7-5222-4915-B364-E4419D08FFAB}',
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
    '{DBE21FBD-3CED-41DE-ABA8-55B32747A6A8}',
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
    '{4CAE8CB8-1056-4EBB-B3C5-769AEF3EF2CE}',
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
    '{6CC6BEFA-232E-4F0B-9420-587DF124BF4B}',
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
    '{96B5331C-BEDF-4F9B-83B2-5F4E4B44D6F6}',
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
    '{A7A964FE-FD9E-4799-8AA6-5AD4D5F77CF6}',
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
    '{FA09E524-E1DD-4F33-928B-4DC187DB3E92}',
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
    '{7B1A05A8-8CDC-40EC-A4EA-2EEF3BEE8831}',
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
    '{F01B33E8-0232-407D-AD74-95756E1715D2}',
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
    '{6A59487F-0776-41DE-9B52-CA976A283DFF}',
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
