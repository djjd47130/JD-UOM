unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

const
  FACTOR_INCH =       39.3701;
  FACTOR_FOOT =       3.28083999999;
  FACTOR_YARD =       1.093613299999;
  FACTOR_FATHOM =     1.82879999999;
  FACTOR_ROD =        5.0292;
  FACTOR_FURLONG =    201.168;
  FACTOR_MILE =       1609.344;
  FACTOR_NAUTICAL_MILE = 1852;
  FACTOR_LIGHT_YEAR = 9460730472580800;
  FACTOR_BANANA =     0.254;
  FACTOR_IPHONE_14_PRO_MAX = 0.1607;

implementation

uses
  System.Math;

procedure RegisterUOM;
  function F(const V: Double): String;
  begin
    Result:= FormatFloat(NumInternalFormat, V);
  end;
  function D(const V: Double): String;
  begin
    Result:= 'Value / '+F(V);
  end;
  function M(const V: Double): String;
  begin
    Result:= 'Value * '+F(V);
  end;
begin

  //Metric

  TUOMUtils.RegisterUOM('Distance',
    'Femtometer', 'Femtometers', '', 'fm', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_FEMTO), M(METRIC_FEMTO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Picometer', 'Picometers', '', 'pm', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_PICO), M(METRIC_PICO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Nanometer', 'Nanometers', '', 'nm', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_NANO), M(METRIC_NANO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Micron', 'Microns', '', 'μm', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MICRO), M(METRIC_MICRO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Millimeter', 'Millimeters', '', 'mm', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MILLI), M(METRIC_MILLI)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Centimeter', 'Centimeters', '', 'cm', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_CENTI), M(METRIC_CENTI)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Decimeter', 'Decimeters', '', 'dm', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_DECI), M(METRIC_DECI)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Meter', 'Meters', '', 'm', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_BASE), M(METRIC_BASE)
  {$ELSE}
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
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Distance',
    'Decameter', 'Decameters', '', 'dam', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_DECA), M(METRIC_DECA)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Hectometer', 'Hectometers', '', 'hm', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_HECTO), M(METRIC_HECTO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Kilometer', 'Kilometers', '', 'km', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_KILO), M(METRIC_KILO)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Megameter', 'Megameters', '', 'Mm', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MEGA), M(METRIC_MEGA)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Gigameter', 'Gigameters', '', 'Gm', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_GIGA), M(METRIC_GIGA)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Terameter', 'Terameters', '', 'Tm', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_TERA), M(METRIC_TERA)
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Petameter', 'Petameters', '', 'Pm', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_PETA), M(METRIC_PETA)
  {$ELSE}
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
  {$ENDIF}
  );

  //Imperial / US Customary

  TUOMUtils.RegisterUOM('Distance',
    'Inch', 'Inches', '', '"', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_INCH), M(FACTOR_INCH)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Inches
      Result:= Value * FACTOR_INCH;
    end,
    function(const Value: Double): Double
    begin
      //Inches to Meters
      Result:= Value / FACTOR_INCH;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Foot', 'Feet', '', '''', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_FOOT), M(FACTOR_FOOT)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Feet
      Result:= Value * FACTOR_FOOT;
    end,
    function(const Value: Double): Double
    begin
      //Feet to Meters
      Result:= Value / FACTOR_FOOT;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Yard', 'Yards', '', 'yd', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_YARD), M(FACTOR_YARD)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Yards
      Result:= Value * FACTOR_YARD;
    end,
    function(const Value: Double): Double
    begin
      //Yards to Meters
      Result:= Value / FACTOR_YARD;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Fathom', 'Fathoms', '', 'fath', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_FATHOM), M(FACTOR_FATHOM)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Fathoms
      Result:= Value * FACTOR_FATHOM; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Fathoms to Meters
      Result:= Value / FACTOR_FATHOM; //TODO: Correct?
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Rod', 'Rods', '', 'rd', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_ROD), M(FACTOR_ROD)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Rods
      Result:= Value / FACTOR_ROD; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Rods to Meters
      Result:= Value * FACTOR_ROD; //TODO: Correct?
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Furlong', 'Furlongs', '', 'fur', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_FURLONG), M(FACTOR_FURLONG)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Furlongs
      Result:= Value / FACTOR_FURLONG; //TODO: Correct?
    end,
    function(const Value: Double): Double
    begin
      //Furlongs to Meters
      Result:= Value * FACTOR_FURLONG; //TODO: Correct?
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Mile', 'Miles', '', 'mi', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_MILE), M(FACTOR_MILE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Miles
      Result:= Value / FACTOR_MILE;
    end,
    function(const Value: Double): Double
    begin
      //Miles to Meters
      Result:= Value * FACTOR_MILE;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Nautical Mile', 'Nautical Miles', '', 'nmi', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_NAUTICAL_MILE), M(FACTOR_NAUTICAL_MILE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Nautical Miles
      Result:= Value / FACTOR_NAUTICAL_MILE;
    end,
    function(const Value: Double): Double
    begin
      //Nautical Miles to Meters
      Result:= Value * FACTOR_NAUTICAL_MILE;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Light Year', 'Light Years', '', 'ly', 'Natural',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_LIGHT_YEAR), M(FACTOR_LIGHT_YEAR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Light Years
      Result:= Value / FACTOR_LIGHT_YEAR;
    end,
    function(const Value: Double): Double
    begin
      //Light Years to Meters
      Result:= Value * FACTOR_LIGHT_YEAR;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'Banana', 'Bananas', '', 'ban', 'Random',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_BANANA), M(FACTOR_BANANA)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to Bananas
      //1 Banana = 8 Inches
      Result:= Value / FACTOR_BANANA;
    end,
    function(const Value: Double): Double
    begin
      //Bananas to Meters
      Result:= Value * FACTOR_BANANA;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Distance',
    'iPhone 14 Pro Max', 'iPhone 14 Pro Maxes', '', 'iP14PM', 'Random',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_IPHONE_14_PRO_MAX), M(FACTOR_IPHONE_14_PRO_MAX)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Meters to iPhone 14 Pro Maxes
      Result:= Value / FACTOR_IPHONE_14_PRO_MAX;
    end,
    function(const Value: Double): Double
    begin
      //iPhone 14 Pro Maxes to Meters
      Result:= Value * FACTOR_IPHONE_14_PRO_MAX;
    end
  {$ENDIF}
  );

end;

initialization
  RegisterUOM;
end.
