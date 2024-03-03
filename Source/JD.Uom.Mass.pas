unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
begin

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Microgram', 'Micrograms', '', 'mcg', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Micrograms
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Micrograms to Grams
      Result:= Value / 1000000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Milligram', 'Milligrams', '', 'mg', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Milligrams
      Result:= Value * 1000;
    end,
    function(const Value: Double): Double
    begin
      //Milligrams to Grams
      Result:= Value / 1000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Gram', 'Grams', '', 'g', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value * 1;
    end,
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value / 1;
    end
  )).SetAsBase;

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Kilogram', 'Kilograms', '', 'kg', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Kilograms
      Result:= Value / 1000;
    end,
    function(const Value: Double): Double
    begin
      //Kilograms to Grams
      Result:= Value * 1000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Metric Ton', 'Metric Tons', '', 't', 'Metric',
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
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Ton (UK)', 'Tons (UK)', '', 'UK t', 'Imperial',
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
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Ton (US)', 'Tons (US)', '', 'US t', 'US Customary',
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
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
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
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
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
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
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
  ));

end;

initialization
  RegisterUOM;
end.
