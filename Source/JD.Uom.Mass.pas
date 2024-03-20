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
begin

  //Metric

  TUOMMetricUtils.ProduceUOMs('Mass', 'Gram', 'g', [msFemto, msPico,
    msNano, msMicro, msMilli, msCenti, msDeci, msBase, msDeca, msHecto,
    msKilo, msMega, msGiga, msTera, msPeta], 'Gram');

  //Same as Megagram
  TUOMUtils.RegisterSimpleUOM('Mass',
    'Metric Ton', 'Metric Tons', 't', 'Metric (Huge)', METRIC_MEGA);

  //Imperial / US Customary

  TUOMUtils.RegisterSimpleUOM('Mass',
    'Ton (UK)', 'Tons (UK)', 'UK t', 'Imperial (Huge)', FACTOR_UK_TON);

  TUOMUtils.RegisterSimpleUOM('Mass',
    'Ton (US)', 'Tons (US)', 'US t', 'US Customary (Huge)', FACTOR_US_TON);

  TUOMUtils.RegisterSimpleUOM('Mass',
    'Stone', 'Stones', 'st', 'Imperial', FACTOR_STONE);

  TUOMUtils.RegisterSimpleUOM('Mass',
    'Pound', 'Pounds', 'lbs', 'US Customary', FACTOR_POUND);

  TUOMUtils.RegisterSimpleUOM('Mass',
    'Ounce', 'Ounces', 'oz', 'US Customary,Imperial', FACTOR_OUNCE);

end;

initialization
  RegisterUOM;
end.
