unit JD.Uom.Volume;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance,
  JD.Uom.Area;

const
  FACTOR_US_TEASPOON = 202900;
  FACTOR_UK_TEASPOON = 168900;
  FACTOR_US_TABLESPOON = 67630;
  FACTOR_UK_TABLESPOON = 56310;
  FACTOR_US_FLOZ = 33810;
  FACTOR_UK_FLOZ = 35200;
  FACTOR_US_CUP = 4227;
  FACTOR_UK_CUP = 3520;
  FACTOR_US_PINT = 2113;
  FACTOR_UK_PINT = 1760;
  FACTOR_US_QUART = 1057;
  FACTOR_UK_QUART = 879.9;
  FACTOR_US_GALLON = 264.2;
  FACTOR_UK_GALLON = 220;

type
  TUOMVolume = record
  private
    //FUnit: TUOMVolumeUnit;
    FValue: Double;
    //procedure SetUnit(const Value: TUOMVolumeUnit);
    procedure SetValue(const Value: Double);
  public
    //property &Unit: TUOMVolumeUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMVolume;
    class operator implicit(const AValue: TUOMVolume): Double;
    class operator implicit(const AValue: String): TUOMVolume;
    class operator implicit(const AValue: TUOMVolume): String;
    //TODO: Implement class operators for math...

  end;

  //TODO: A record that allows volume to be specified using three linear dimensions
  TUOMVolumeBox = record
  private
    //FWidth: TUOMLength;
    //FLength: TUOMLength;
    //FHeight: TUOMLength;
  public

  end;

implementation

function Cube(const AValue: Double): Double;
begin
  Result:= AValue * AValue * AValue;
end;

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

  TUOMUtils.RegisterUOM('Volume',
    'Milliliter', 'Milliliters', '', 'mL', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Cube(METRIC_MILLI)), M(Cube(METRIC_MILLI))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Milliliter
      Result:= Value / Cube(METRIC_MILLI);
    end,
    function(const Value: Double): Double
    begin
      //Milliliter to Cubic Meters
      Result:= Value * Cube(METRIC_MILLI);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cubic Centimeter', 'Cubic Centimeters', '', 'cm³', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Cube(METRIC_MILLI)), M(Cube(METRIC_MILLI))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Centimeters
      //Identicial to Milliliters
      Result:= Value / Cube(METRIC_MILLI);
    end,
    function(const Value: Double): Double
    begin
      //Cubic Centimeters to Cubic Meters
      //Identicial to Milliliters
      Result:= Value * Cube(METRIC_MILLI);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Liter', 'Liters', '', 'L', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Cube(METRIC_DECI)), M(Cube(METRIC_DECI))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Liters
      Result:= Value / Cube(METRIC_DECI);
    end,
    function(const Value: Double): Double
    begin
      //Liters to Cubic Meters
      Result:= Value * Cube(METRIC_DECI);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cubic Meter', 'Cubic Meters', '', 'm³', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(Cube(METRIC_BASE)), M(Cube(METRIC_BASE))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Meters
      Result:= Value / Cube(METRIC_BASE);
    end,
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Meters
      Result:= Value * Cube(METRIC_BASE);
    end
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Volume',
    'Teaspoon (US)', 'Teaspoons (US)', '', 'US tsp', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_TEASPOON), D(FACTOR_US_TEASPOON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Teaspoons (US)
      Result:= Value * FACTOR_US_TEASPOON;
    end,
    function(const Value: Double): Double
    begin
      //Teaspoons (US) to Cubic Meters
      Result:= Value / FACTOR_US_TEASPOON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Teaspoon (UK)', 'Teaspoons (UK)', '', 'UK tsp', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_TEASPOON), D(FACTOR_UK_TEASPOON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Teaspoons (UK)
      Result:= Value * FACTOR_UK_TEASPOON;
    end,
    function(const Value: Double): Double
    begin
      //Teaspoons (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_TEASPOON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Tablespoon (US)', 'Tablespoons (US)', '', 'US Tsp', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_TABLESPOON), D(FACTOR_US_TABLESPOON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Tablespoon (US)
      Result:= Value * FACTOR_US_TABLESPOON;
    end,
    function(const Value: Double): Double
    begin
      //Tablespoon (US) to Cubic Meters
      Result:= Value / FACTOR_US_TABLESPOON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Tablespoon (UK)', 'Tablespoons (UK)', '', 'UK Tsp', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_TABLESPOON), D(FACTOR_UK_TABLESPOON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Tablespoon (UK)
      Result:= Value * FACTOR_UK_TABLESPOON;
    end,
    function(const Value: Double): Double
    begin
      //Tablespoon (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_TABLESPOON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Fluid Ounce (US)', 'Fluid Ounces (US)', '', 'US fl. oz', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_FLOZ), D(FACTOR_US_FLOZ)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Fluid Ounces (US)
      Result:= Value * FACTOR_US_FLOZ;
    end,
    function(const Value: Double): Double
    begin
      //Fluid Ounces (US) to Cubic Meters
      Result:= Value / FACTOR_US_FLOZ;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Fluid Ounce (UK)', 'Fluid Ounces (UK)', '', 'UK fl. oz', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_FLOZ), D(FACTOR_UK_FLOZ)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Fluid Ounces (UK)
      Result:= Value * FACTOR_UK_FLOZ;
    end,
    function(const Value: Double): Double
    begin
      //Fluid Ounces (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_FLOZ;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cup (US)', 'Cups (US)', '', 'US c', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_CUP), D(FACTOR_US_CUP)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cups (US)
      Result:= Value * FACTOR_US_CUP;
    end,
    function(const Value: Double): Double
    begin
      //Cups (US) to Cubic Meters
      Result:= Value / FACTOR_US_CUP;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cup (UK)', 'Cups (UK)', '', 'UK c', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_CUP), D(FACTOR_UK_CUP)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cups (UK)
      Result:= Value * FACTOR_UK_CUP;
    end,
    function(const Value: Double): Double
    begin
      //Cups (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_CUP;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Pint (US)', 'Pints (US)', '', 'US pt', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_PINT), D(FACTOR_US_PINT)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Pints (US)
      Result:= Value * FACTOR_US_PINT;
    end,
    function(const Value: Double): Double
    begin
      //Pints (US) to Cubic Meters
      Result:= Value / FACTOR_US_PINT;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Pint (UK)', 'Pints (UK)', '', 'UK pt', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_PINT), D(FACTOR_UK_PINT)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Pints (UK)
      Result:= Value * FACTOR_UK_PINT;
    end,
    function(const Value: Double): Double
    begin
      //Pints (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_PINT;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Quart (US)', 'Quarts (US)', '', 'US qt', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_QUART), D(FACTOR_US_QUART)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Quarts (US)
      Result:= Value * FACTOR_US_QUART;
    end,
    function(const Value: Double): Double
    begin
      //Quarts (US) to Cubic Meters
      Result:= Value / FACTOR_US_QUART;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Quart (UK)', 'Quarts (UK)', '', 'UK qt', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_QUART), D(FACTOR_UK_QUART)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Quarts (UK)
      Result:= Value * FACTOR_UK_QUART;
    end,
    function(const Value: Double): Double
    begin
      //Quarts (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_QUART;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Gallon (US)', 'Gallons (US)', '', 'US gal', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_US_GALLON), D(FACTOR_US_GALLON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Gallons (US)
      Result:= Value * FACTOR_US_GALLON;
    end,
    function(const Value: Double): Double
    begin
      //Gallons (US) to Cubic Meters
      Result:= Value / FACTOR_US_GALLON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Gallon (UK)', 'Gallons (UK)', '', 'UK gal', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_UK_GALLON), D(FACTOR_UK_GALLON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Gallons (UK)
      Result:= Value * FACTOR_UK_GALLON;
    end,
    function(const Value: Double): Double
    begin
      //Gallons (UK) to Cubic Meters
      Result:= Value / FACTOR_UK_GALLON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cubic Inch', 'Cubic Inches', '', '"³', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(Cube(FACTOR_INCH)), D(Cube(FACTOR_INCH))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Inches
      Result:= Value * Cube(FACTOR_INCH);
    end,
    function(const Value: Double): Double
    begin
      //Cubic Inches to Cubic Meters
      Result:= Value / Cube(FACTOR_INCH);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cubic Foot', 'Cubic Feet', '', '''³', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(Cube(FACTOR_FOOT)), D(Cube(FACTOR_FOOT))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Feet
      Result:= Value * Cube(FACTOR_FOOT);
    end,
    function(const Value: Double): Double
    begin
      //Cubic Feet to Cubic Meters
      Result:= Value / Cube(FACTOR_FOOT);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Volume',
    'Cubic Yard', 'Cubic Yards', '', 'yd³', 'US Customary (Huge)',
  {$IFDEF USE_MATH_EXPR}
    M(Cube(FACTOR_YARD)), D(Cube(FACTOR_YARD))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Yards
      Result:= Value * Cube(FACTOR_YARD);
    end,
    function(const Value: Double): Double
    begin
      //Cubic Yards to Cubic Meters
      Result:= Value / Cube(FACTOR_YARD);
    end
  {$ENDIF}
  );

end;

{ TUOMVolume }

class operator TUOMVolume.implicit(const AValue: Double): TUOMVolume;
begin
  //Result.FUnit:= TUOMVolumeUnitBaseClass(TUOMVolumeUtils.BaseUnit).UnitEnum;
  //Result.FValue:= AValue;
end;

class operator TUOMVolume.implicit(const AValue: TUOMVolume): Double;
begin
  Result:= AValue; //TODO
end;

class operator TUOMVolume.implicit(const AValue: TUOMVolume): String;
begin
  //Result:= FormatFloat(NumFormat, AValue.FValue);
  //TODO: Result:= Result + TUOMVolumeUtils.UnitSuffix(AValue.FUnit);
end;

class operator TUOMVolume.implicit(const AValue: String): TUOMVolume;
begin
  //TODO: Parse

end;

procedure TUOMVolume.SetValue(const Value: Double);
begin
  FValue := Value;
end;

initialization
  RegisterUOM;
end.
