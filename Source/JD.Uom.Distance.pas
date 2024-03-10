unit JD.Uom.Distance;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

const
  FACTOR_INCH =           0.0254;
  FACTOR_FOOT =           0.3048;
  FACTOR_YARD =           0.9144;
  FACTOR_FATHOM =         1.82879999999;
  FACTOR_ROD =            5.0292;
  FACTOR_FURLONG =        201.168;
  FACTOR_MILE =           1609.344;
  FACTOR_NAUTICAL_MILE =  1852;
  FACTOR_LIGHT_YEAR =     9460730472580800;
  FACTOR_BANANA =         0.254;
  FACTOR_IPHONE_14_PRO_MAX = 0.1607;

implementation

uses
  System.Math;

procedure RegisterUOM;
begin

  //Metric

  TUOMMetricUtils.ProduceUOMs('Distance', 'Meter', 'm', [msFemto, msPico,
    msNano, msMicro, msMilli, msCenti, msDeci, msBase, msDeca, msHecto,
    msKilo, msMega, msGiga, msTera, msPeta]);

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
