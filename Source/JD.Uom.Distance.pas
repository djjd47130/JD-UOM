unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

uses
  System.Math;

procedure RegisterUOM;
begin

  //Metric

  TUOMUtils.RegisterUOM('Distance',
    'Femtometer', 'Femtometers', '', 'fm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Femtometers
      Result:= Value / METRIC_FEMTO;
    end,
    function(const Value: Double): Double
    begin
      //Femtometers to Meters
      Result:= Value * METRIC_FEMTO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Picometer', 'Picometers', '', 'pm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Picometers
      Result:= Value / METRIC_PICO;
    end,
    function(const Value: Double): Double
    begin
      //Picometers to Meters
      Result:= Value * METRIC_PICO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Nanometer', 'Nanometers', '', 'nm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Nanometers
      Result:= Value / METRIC_NANO;
    end,
    function(const Value: Double): Double
    begin
      //Nanometers to Meters
      Result:= Value * METRIC_NANO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Micron', 'Microns', '', 'μm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Microns
      Result:= Value / METRIC_MICRO;
    end,
    function(const Value: Double): Double
    begin
      //Microns to Meters
      Result:= Value * METRIC_MICRO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Millimeter', 'Millimeters', '', 'mm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Millimeters
      Result:= Value / METRIC_MILLI;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters to Meters
      Result:= Value * METRIC_MILLI;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Centimeter', 'Centimeters', '', 'cm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Centimeters
      Result:= Value / METRIC_CENTI;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters to Meters
      Result:= Value * METRIC_CENTI;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Decimeter', 'Decimeters', '', 'dm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Decimeters
      Result:= Value / METRIC_DECI;
    end,
    function(const Value: Double): Double
    begin
      //Decimeters to Meters
      Result:= Value * METRIC_DECI;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
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
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Distance',
    'Decameter', 'Decameters', '', 'dam', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Decameters
      Result:= Value / METRIC_DECA;
    end,
    function(const Value: Double): Double
    begin
      //Decameters to Meters
      Result:= Value * METRIC_DECA;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Hectometer', 'Hectometers', '', 'hm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Hectometers
      Result:= Value / METRIC_HECTO;
    end,
    function(const Value: Double): Double
    begin
      //Hectometers to Meters
      Result:= Value * METRIC_HECTO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Kilometer', 'Kilometers', '', 'km', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Kilometers
      Result:= Value / METRIC_KILO;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers to Meters
      Result:= Value * METRIC_KILO;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Megameter', 'Megameters', '', 'Mm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Megameters
      Result:= Value / METRIC_MEGA;
    end,
    function(const Value: Double): Double
    begin
      //Megameters to Meters
      Result:= Value * METRIC_MEGA;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Gigameter', 'Gigameters', '', 'Gm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Gigameters
      Result:= Value / METRIC_GIGA;
    end,
    function(const Value: Double): Double
    begin
      //Gigameters to Meters
      Result:= Value * METRIC_GIGA;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Terameter', 'Terameters', '', 'Tm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Terameters
      Result:= Value / METRIC_TERA;
    end,
    function(const Value: Double): Double
    begin
      //Terameters to Meters
      Result:= Value * METRIC_TERA;
    end
  );

  TUOMUtils.RegisterUOM('Distance',
    'Petameter', 'Petameters', '', 'Pm', 'Metric',
    function(const Value: Double): Double
    begin
      //Meters to Petameters
      Result:= Value / METRIC_PETA;
    end,
    function(const Value: Double): Double
    begin
      //Petameters to Meters
      Result:= Value * METRIC_PETA;
    end
  );

  //Imperial / US Customary

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

  TUOMUtils.RegisterUOM('Distance',
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
  );

end;

initialization
  RegisterUOM;
end.
