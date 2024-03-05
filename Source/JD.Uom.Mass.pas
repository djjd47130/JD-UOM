unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
begin

  TUOMUtils.RegisterUOM('Mass',
    'Microgram', 'Micrograms', '', 'mcg', 'Metric (Tiny)',
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
  );

  TUOMUtils.RegisterUOM('Mass',
    'Milligram', 'Milligrams', '', 'mg', 'Metric',
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
  );

  TUOMUtils.RegisterUOM('Mass',
    'Gram', 'Grams', '', 'g', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value / 1;
    end,
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value * 1;
    end
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Mass',
    'Decagram', 'Decagrams', '', 'dag', 'Metric',
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
  );

  TUOMUtils.RegisterUOM('Mass',
    'Hectogram', 'Hectograms', '', 'hg', 'Metric',
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
  );

  TUOMUtils.RegisterUOM('Mass',
    'Kilogram', 'Kilograms', '', 'kg', 'Metric',
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
  );

  TUOMUtils.RegisterUOM('Mass',
    'Metric Ton', 'Metric Tons', '', 't', 'Metric (Huge)',
    function(const Value: Double): Double
    begin
      //Grams to Metric Tons
      Result:= Value / 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Metric Tons to Grams
      Result:= Value * 1000000;
    end
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ton (UK)', 'Tons (UK)', '', 'UK t', 'Imperial (Huge)',
    function(const Value: Double): Double
    begin
      //Grams to Tons (UK)
      Result:= Value / 1.016e+6;
    end,
    function(const Value: Double): Double
    begin
      //Tons (UK) to Grams
      Result:= Value * 1.016e+6;
    end
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ton (US)', 'Tons (US)', '', 'US t', 'US Customary (Huge)',
    function(const Value: Double): Double
    begin
      //Grams to Tons (US)
      Result:= Value / 907200;
    end,
    function(const Value: Double): Double
    begin
      //Tons (US) to Grams
      Result:= Value * 907200;
    end
  );

  TUOMUtils.RegisterUOM('Mass',
    'Stone', 'Stones', '', 'st', 'Imperial',
    function(const Value: Double): Double
    begin
      //Grams to Stones
      Result:= Value / 6350;
    end,
    function(const Value: Double): Double
    begin
      //Stones to Grams
      Result:= Value * 6350;
    end
  );

  TUOMUtils.RegisterUOM('Mass',
    'Pound', 'Pounds', '', 'lbs', 'US Customary',
    function(const Value: Double): Double
    begin
      //Grams to Pounds
      Result:= Value / 453.592;
    end,
    function(const Value: Double): Double
    begin
      //Pounds to Grams
      Result:= Value * 453.592;
    end
  );

  TUOMUtils.RegisterUOM('Mass',
    'Ounce', 'Ounces', '', 'oz', 'US Customary,Imperial',
    function(const Value: Double): Double
    begin
      //Grams to Ounces
      Result:= Value / 28.3495;
    end,
    function(const Value: Double): Double
    begin
      //Ounces to Grams
      Result:= Value * 28.3495;
    end
  );

end;

initialization
  RegisterUOM;
end.
