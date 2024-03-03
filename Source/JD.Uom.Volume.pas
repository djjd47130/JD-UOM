unit JD.Uom.Volume;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance,
  JD.Uom.Area;

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

procedure RegisterUOM;
begin
  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Milliliter', 'Milliliters', '', 'mL', 'Metric',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Milliliter
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Milliliter to Cubic Meters
      Result:= Value / 1000000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cubic Centimeter', 'Cubic Centimeters', '', 'cm³', 'Metric',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Centimeters
      //Identicial to Milliliters
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Cubic Centimeters to Cubic Meters
      //Identicial to Milliliters
      Result:= Value / 1000000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Liter', 'Liters', '', 'L', 'Metric',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Liters
      Result:= Value * 1000;
    end,
    function(const Value: Double): Double
    begin
      //Liters to Cubic Meters
      Result:= Value / 1000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cubic Meter', 'Cubic Meters', '', 'm³', 'Metric',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Meters
      Result:= Value * 1;
    end,
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Meters
      Result:= Value / 1;
    end
  )).SetAsBase;

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Teaspoon (US)', 'Teaspoons (US)', '', 'US tsp', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Teaspoons (US)
      Result:= Value * 202900;
    end,
    function(const Value: Double): Double
    begin
      //Teaspoons (US) to Cubic Meters
      Result:= Value / 202900;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Teaspoon (UK)', 'Teaspoons (UK)', '', 'UK tsp', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Teaspoons (UK)
      Result:= Value * 168900;
    end,
    function(const Value: Double): Double
    begin
      //Teaspoons (UK) to Cubic Meters
      Result:= Value / 168900;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Tablespoon (US)', 'Tablespoons (US)', '', 'US Tsp', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Tablespoon (US)
      Result:= Value * 67630;
    end,
    function(const Value: Double): Double
    begin
      //Tablespoon (US) to Cubic Meters
      Result:= Value / 67630;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Tablespoon (UK)', 'Tablespoons (UK)', '', 'UK Tsp', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Tablespoon (UK)
      Result:= Value * 56310;
    end,
    function(const Value: Double): Double
    begin
      //Tablespoon (UK) to Cubic Meters
      Result:= Value / 56310;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Fluid Ounce (US)', 'Fluid Ounces (US)', '', 'US fl. oz', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Fluid Ounces (US)
      Result:= Value * 33810;
    end,
    function(const Value: Double): Double
    begin
      //Fluid Ounces (US) to Cubic Meters
      Result:= Value / 33810;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Fluid Ounce (UK)', 'Fluid Ounces (UK)', '', 'UK fl. oz', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Fluid Ounces (UK)
      Result:= Value * 35200;
    end,
    function(const Value: Double): Double
    begin
      //Fluid Ounces (UK) to Cubic Meters
      Result:= Value / 35200;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cup (US)', 'Cups (US)', '', 'US c', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cups (US)
      Result:= Value * 4227;
    end,
    function(const Value: Double): Double
    begin
      //Cups (US) to Cubic Meters
      Result:= Value / 4227;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cup (UK)', 'Cups (UK)', '', 'UK c', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cups (UK)
      Result:= Value * 3520;
    end,
    function(const Value: Double): Double
    begin
      //Cups (UK) to Cubic Meters
      Result:= Value / 3520;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Pint (US)', 'Pints (US)', '', 'US pt', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Pints (US)
      Result:= Value * 2113;
    end,
    function(const Value: Double): Double
    begin
      //Pints (US) to Cubic Meters
      Result:= Value / 2113;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Pint (UK)', 'Pints (UK)', '', 'UK pt', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Pints (UK)
      Result:= Value * 1760;
    end,
    function(const Value: Double): Double
    begin
      //Pints (UK) to Cubic Meters
      Result:= Value / 1760;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Quart (US)', 'Quarts (US)', '', 'US qt', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Quarts (US)
      Result:= Value * 1056.69;
    end,
    function(const Value: Double): Double
    begin
      //Quarts (US) to Cubic Meters
      Result:= Value / 1056.69;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Quart (UK)', 'Quarts (UK)', '', 'UK qt', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Quarts (UK)
      Result:= Value * 879.87848458785;
    end,
    function(const Value: Double): Double
    begin
      //Quarts (UK) to Cubic Meters
      Result:= Value / 879.87848458785;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Gallon (US)', 'Gallons (US)', '', 'US gal', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Gallons (US)
      Result:= Value * 264.172;
    end,
    function(const Value: Double): Double
    begin
      //Gallons (US) to Cubic Meters
      Result:= Value / 264.172;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Gallon (UK)', 'Gallons (UK)', '', 'UK gal', 'Imperial',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Gallons (UK)
      Result:= Value * 219.969204701183;
    end,
    function(const Value: Double): Double
    begin
      //Gallons (UK) to Cubic Meters
      Result:= Value /219.969204701183;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cubic Inch', 'Cubic Inches', '', '"³', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Inches
      Result:= Value * 61023.7;
    end,
    function(const Value: Double): Double
    begin
      //Cubic Inches to Cubic Meters
      Result:= Value / 61023.7;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cubic Foot', 'Cubic Feet', '', '''³', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Feet
      Result:= Value * 35.3147;
    end,
    function(const Value: Double): Double
    begin
      //Cubic Feet to Cubic Meters
      Result:= Value / 35.3147;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Volume',
    'Cubic Yard', 'Cubic Yards', '', 'yd³', 'US Customary',
    function(const Value: Double): Double
    begin
      //Cubic Meters to Cubic Yards
      Result:= Value * 1.30795;
    end,
    function(const Value: Double): Double
    begin
      //Cubic Yards to Cubic Meters
      Result:= Value / 1.30795;
    end
  ));

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
