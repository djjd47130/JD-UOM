unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

//TODO: NUMEROUS more UOMs discovered here:
//https://www.gowebtool.com/unit-conversion/length/convert.php?from=meter&to=gauge&language=en

const
  FACTOR_MIL =                2.54e-5;
  FACTOR_INCH =               0.0254;
  FACTOR_FOOT =               0.3048;
  FACTOR_YARD =               0.9144;
  FACTOR_FATHOM =             1.82879999999;
  FACTOR_ROD =                5.0292;
  FACTOR_FURLONG =            201.168;
  FACTOR_MILE =               1609.344;
  FACTOR_NAUTICAL_MILE =      1852;
  FACTOR_LIGHT_YEAR =         9460730472580800;
  FACTOR_BANANA =             0.254;
  FACTOR_IPHONE_14_PRO_MAX =  0.1607;

implementation

uses
  System.Math;

procedure RegisterUOM;
begin

  //Metric

  TUOMMetricUtils.ProduceUOMs('Distance', 'Meter', 'm', [msYocto, msZepto, msAtto,
    msFemto, msPico, msNano, msMicro, msMilli, msCenti, msDeci,
    msBase, msDeca, msHecto, msKilo, msMega, msGiga, msTera, msPeta,
    msExa, msZeta, msYotta], 'Meter');

  //TODO: Gauge...
  //https://sheetmetal.me/air-bend-force-chart/gauge-to-mm-conversion/
  //TUOMUtils.RegisterSimpleUOM('Distance',
  //  'Gauge', 'Gauge', '', '', FACTOR_GAUGE);

  //Imperial / US Customary



  TUOMUtils.RegisterSimpleUOM('Distance',
    'Mil', 'Mil', 'mil', 'Imperial,US Customary', FACTOR_MIL).AddAlias('thou').AddAlias('Thou').AddAlias('Thousandth');

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Inch', 'Inches', '"', 'Imperial,US Customary', FACTOR_INCH).AddAlias('in').AddAlias('In').AddAlias('IN');

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Foot', 'Feet', '''', 'Imperial,US Customary', FACTOR_FOOT).AddAlias('ft').AddAlias('Ft').AddAlias('FT');

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Yard', 'Yards', 'yd', 'Imperial,US Customary', FACTOR_YARD);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Fathom', 'Fathoms', 'fath', 'Imperial', FACTOR_FATHOM);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Rod', 'Rods', 'rd', 'Imperial', FACTOR_ROD);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Furlong', 'Furlongs', 'fur', 'Imperial,US Customary', FACTOR_FURLONG);

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Mile', 'Miles', 'mi', 'Imperial,US Customary', FACTOR_MILE).AddAlias('Mi').AddAlias('MI');

  TUOMUtils.RegisterSimpleUOM('Distance',
    'Nautical Mile', 'Nautical Miles', 'nmi', 'Imperial,US Customary', FACTOR_NAUTICAL_MILE).AddAlias('NMI');

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
