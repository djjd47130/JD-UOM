unit JD.Uom.Frequency;

{
Frequency units of measurement such as MHz, GHz...

Ref: https://en.wikipedia.org/wiki/Frequency

GOAL: Make Frequency as abstract as possible, then use it elsewhere as necessary,
such as Speed and Radiation.

}

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

const
  FACTOR_CPS = 1;
  FACTOR_CPM = (1 / 60);
  FACTOR_RPM = (1 / 60);

implementation

procedure RegisterUOMs;
begin
  TUOMMetricUtils.ProduceUOMs('Frequency', 'Hertz', 'Hz', [msFemto, msPico,
    msNano, msMicro, msMilli, msCenti, msDeci, msBase, msDeca, msHecto,
    msKilo, msMega, msGiga, msTera, msPeta], 'Hertz');

  TUOMUtils.RegisterSimpleUOM('Frequency',
    'Cycle per Second', 'Cycles per Second', 'cps', 'Natural', FACTOR_CPS);

  TUOMUtils.RegisterSimpleUOM('Frequency',
    'Cycle per Minute', 'Cycles per Minute', 'cpm', 'Natural', FACTOR_CPM);

  TUOMUtils.RegisterSimpleUOM('Frequency',
    'Revolution per Minute', 'Revolutions per Minute', 'rpm', 'Natural', FACTOR_RPM);

end;

initialization
  RegisterUOMs;
end.
