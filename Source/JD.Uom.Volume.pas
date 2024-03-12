unit JD.Uom.Volume;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance,
  JD.Uom.Area;

const
  FACTOR_US_TEASPOON =    4.92892e-6;
  FACTOR_UK_TEASPOON =    5.91939e-6;
  FACTOR_US_TABLESPOON =  1.47868e-5;
  FACTOR_UK_TABLESPOON =  1.77582e-5;
  FACTOR_US_FLOZ =        2.95735e-5;
  FACTOR_UK_FLOZ =        2.84131e-5;
  FACTOR_US_CUP =         0.000236588;
  FACTOR_UK_CUP =         0.000284131;
  FACTOR_US_PINT =        0.000473176;
  FACTOR_UK_PINT =        0.000568261;
  FACTOR_US_QUART =       0.000946353;
  FACTOR_UK_QUART =       0.00113652;
  FACTOR_US_GALLON =      0.00378541;
  FACTOR_UK_GALLON =      0.00454609;
  FACTOR_EARTH_VOLUME =   1.082999999E+21;

type
  /// <summary>
  /// (NOT READY)
  /// A record that allows volume to be specified using three linear dimensions
  /// or a TUOMAreaRect and one linear dimension
  /// </summary>
  TUOMVolumeBox = record
  private
    //FWidth: TUOMLength;
    //FLength: TUOMLength;
    //FHeight: TUOMLength;
  public

  end;

function Cube(const AValue: Double): Double;

implementation

function Cube(const AValue: Double): Double;
begin
  Result:= AValue * AValue * AValue;
end;

procedure RegisterUOM;
begin

  //Metric

  TUOMMetricUtils.ProduceUOMs('Volume', 'Liter', 'L', [msFemto, msPico,
    msNano, msMicro, msMilli, msCenti, msDeci, msBase, msDeca, msHecto,
    msKilo, msMega, msGiga, msTera, msPeta]);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cubic Centimeter', 'Cubic Centimeters', 'cm³', 'Metric', Cube(METRIC_MILLI));

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cubic Meter', 'Cubic Meters', 'm³', 'Metric', Cube(METRIC_BASE)).SetAsBase;

  //Imperial / US Customary

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Teaspoon (US)', 'Teaspoons (US)', 'US tsp', 'US Customary', FACTOR_US_TEASPOON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Teaspoon (UK)', 'Teaspoons (UK)', 'UK tsp', 'Imperial', FACTOR_UK_TEASPOON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Tablespoon (US)', 'Tablespoons (US)', 'US Tsp', 'US Customary', FACTOR_US_TABLESPOON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Tablespoon (UK)', 'Tablespoons (UK)', 'UK Tsp', 'Imperial', FACTOR_UK_TABLESPOON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Fluid Ounce (US)', 'Fluid Ounces (US)', 'US fl. oz', 'US Customary', FACTOR_US_FLOZ);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Fluid Ounce (UK)', 'Fluid Ounces (UK)', 'UK fl. oz', 'Imperial', FACTOR_UK_FLOZ);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cup (US)', 'Cups (US)', 'US c', 'US Customary', FACTOR_US_CUP);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cup (UK)', 'Cups (UK)', 'UK c', 'Imperial', FACTOR_UK_CUP);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Pint (US)', 'Pints (US)', 'US pt', 'US Customary', FACTOR_US_PINT);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Pint (UK)', 'Pints (UK)', 'UK pt', 'Imperial', FACTOR_UK_PINT);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Quart (US)', 'Quarts (US)', 'US qt', 'US Customary', FACTOR_US_QUART);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Quart (UK)', 'Quarts (UK)', 'UK qt', 'Imperial', FACTOR_UK_QUART);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Gallon (US)', 'Gallons (US)', 'US gal', 'US Customary', FACTOR_US_GALLON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Gallon (UK)', 'Gallons (UK)', 'UK gal', 'Imperial', FACTOR_UK_GALLON);

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cubic Inch', 'Cubic Inches', '"³', 'US Customary', Cube(FACTOR_INCH));

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cubic Foot', 'Cubic Feet', '''³', 'US Customary', Cube(FACTOR_FOOT));

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Cubic Yard', 'Cubic Yards', 'yd³', 'US Customary (Huge)', Cube(FACTOR_YARD));

  TUOMUtils.RegisterSimpleUOM('Volume',
    'Earth Volume', 'Earth Volume', 'Earth³', 'Natural', FACTOR_EARTH_VOLUME);

end;

initialization
  RegisterUOM;
end.
