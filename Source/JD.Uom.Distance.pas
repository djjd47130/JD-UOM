unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

const
  FACTOR_INCH =       0.0254;
  FACTOR_FOOT =       0.3048;
  FACTOR_YARD =       0.9144;
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
begin

  //Metric

  {
  TUOMMetricUtils.ProduceUOMs('Distance', 'Meter', 'm', [msFemto, msPico, msNano,
    msMicro, msMilli, msCenti, msDeci,
    msBase, msDeca, msHecto, msKilo, msMega, msGiga, msTera, msPeta]);
  }

  //{
  TUOMUtils.RegisterSimpleUOM('Distance',
    'Femtometer', 'Femtometers', 'fm', 'Metric (Tiny)', METRIC_FEMTO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Picometer', 'Picometers', 'pm', 'Metric (Tiny)', METRIC_PICO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Nanometer', 'Nanometers', 'nm', 'Metric (Tiny)', METRIC_NANO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Micron', 'Microns', 'μm', 'Metric (Tiny)', METRIC_MICRO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Millimeter', 'Millimeters', 'mm', 'Metric', METRIC_MILLI);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Centimeter', 'Centimeter', 'cm', 'Metric', METRIC_CENTI);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Decimeter', 'Decimeters', 'dm', 'Metric', METRIC_DECI);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Meter', 'Meters', 'm', 'Metric', METRIC_BASE).SetAsBase;

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Decameter', 'Decameters', 'dam', 'Metric', METRIC_DECA);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Hectometer', 'Hectometers', 'hm', 'Metric', METRIC_HECTO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Kilometer', 'Kilometers', 'km', 'Metric', METRIC_KILO);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Megameter', 'Megameters', 'Mm', 'Metric (Huge)', METRIC_MEGA);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Gigameter', 'Gigameters', 'Gm', 'Metric (Huge)', METRIC_GIGA);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Terameter', 'Terameters', 'Tm', 'Metric (Huge)', METRIC_TERA);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Petameter', 'Petameters', 'Pm', 'Metric (Huge)', METRIC_PETA);
  //}

  //Imperial / US Customary

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Inch', 'Inches', '"', 'Imperial,US Customary', FACTOR_INCH);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Foot', 'Feet', '''', 'Imperial,US Customary', FACTOR_FOOT);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Yard', 'Yards', 'yd', 'Imperial,US Customary', FACTOR_YARD);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Fathom', 'Fathoms', 'fath', 'Imperial', FACTOR_FATHOM);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Rod', 'Rods', 'rd', 'Imperial', FACTOR_ROD);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Furlong', 'Furlongs', 'fur', 'Imperial,US Customary', FACTOR_FURLONG);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Mile', 'Miles', 'mi', 'Imperial,US Customary', FACTOR_MILE);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Nautical Mile', 'Nautical Miles', 'nmi', 'Imperial,US Customary', FACTOR_NAUTICAL_MILE);

  //Natural

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Light Year', 'Light Years', 'ly', 'Natural', FACTOR_LIGHT_YEAR);

  //Random

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Banana', 'Bananas', 'ban', 'Random', FACTOR_BANANA);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'iPhone 14 Pro Max', 'iPhone 14 Pro Maxes', 'iP14PM', 'Random', FACTOR_IPHONE_14_PRO_MAX);

end;

initialization
  RegisterUOM;
end.
