unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

const
  FACTOR_UK_TON = 1.016e+6;
  FACTOR_US_TON = 907200;
  FACTOR_STONE = 6350;
  FACTOR_POUND = 453.592;
  FACTOR_OUNCE = 28.3495;

implementation

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

  TUOMUtils.RegisterUOM('Mass',
    'Microgram', 'Micrograms', '', 'mcg', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MICRO), M(METRIC_MICRO)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Micrograms
      Result:= Value / METRIC_MICRO;
    end,
    function(const Value: Double): Double
    begin
      //Micrograms to Grams
      Result:= Value * METRIC_MICRO;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Milligram', 'Milligrams', '', 'mg', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MILLI), M(METRIC_MILLI)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Milligrams
      Result:= Value / METRIC_MILLI;
    end,
    function(const Value: Double): Double
    begin
      //Milligrams to Grams
      Result:= Value * METRIC_MILLI;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Gram', 'Grams', '', 'g', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_BASE), M(METRIC_BASE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value / METRIC_BASE;
    end,
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value * METRIC_BASE;
    end
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Mass',
    'Decagram', 'Decagrams', '', 'dag', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_DECA), M(METRIC_DECA)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Decagrams
      Result:= Value / METRIC_DECA;
    end,
    function(const Value: Double): Double
    begin
      //Decagrams to Grams
      Result:= Value * METRIC_DECA;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Hectogram', 'Hectograms', '', 'hg', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_HECTO), M(METRIC_HECTO)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Hectograms
      Result:= Value / METRIC_HECTO;
    end,
    function(const Value: Double): Double
    begin
      //Hectograms to Grams
      Result:= Value * METRIC_HECTO;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Kilogram', 'Kilograms', '', 'kg', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_KILO), M(METRIC_KILO)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Kilograms
      Result:= Value / METRIC_KILO;
    end,
    function(const Value: Double): Double
    begin
      //Kilograms to Grams
      Result:= Value * METRIC_KILO;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Metric Ton', 'Metric Tons', '', 't', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(METRIC_MEGA), M(METRIC_MEGA)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Metric Tons
      Result:= Value / METRIC_MEGA;
    end,
    function(const Value: Double): Double
    begin
      //Metric Tons to Grams
      Result:= Value * METRIC_MEGA;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ton (UK)', 'Tons (UK)', '', 'UK t', 'Imperial (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_UK_TON), M(FACTOR_UK_TON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Tons (UK)
      Result:= Value / FACTOR_UK_TON;
    end,
    function(const Value: Double): Double
    begin
      //Tons (UK) to Grams
      Result:= Value * FACTOR_UK_TON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ton (US)', 'Tons (US)', '', 'US t', 'US Customary (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_US_TON), M(FACTOR_US_TON)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Tons (US)
      Result:= Value / FACTOR_US_TON;
    end,
    function(const Value: Double): Double
    begin
      //Tons (US) to Grams
      Result:= Value * FACTOR_US_TON;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Stone', 'Stones', '', 'st', 'Imperial',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_STONE), M(FACTOR_STONE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Stones
      Result:= Value / FACTOR_STONE;
    end,
    function(const Value: Double): Double
    begin
      //Stones to Grams
      Result:= Value * FACTOR_STONE;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Pound', 'Pounds', '', 'lbs', 'US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_POUND), M(FACTOR_POUND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Pounds
      Result:= Value / FACTOR_POUND;
    end,
    function(const Value: Double): Double
    begin
      //Pounds to Grams
      Result:= Value * FACTOR_POUND;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ounce', 'Ounces', '', 'oz', 'US Customary,Imperial',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_OUNCE), M(FACTOR_OUNCE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Grams to Ounces
      Result:= Value / FACTOR_OUNCE;
    end,
    function(const Value: Double): Double
    begin
      //Ounces to Grams
      Result:= Value * FACTOR_OUNCE;
    end
  {$ENDIF}
  );


end;

initialization
  RegisterUOM;
end.
