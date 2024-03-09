unit JD.Uom.Temperature;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
begin

  TUOMUtils.RegisterUOM('Temperature',
    'Celsius', 'Celsius', '', '°C', 'Metric',
    'Value', 'Value'
  );

  TUOMUtils.RegisterUOM('Temperature',
    'Farenheit', 'Farenheit', '', '°F', 'Imperial,US Customary',
    '(Value * 1.8) + 32', '(Value - 32) / 1.8'
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Temperature',
    'Kelvin', 'Kelvin', '', '°K', 'Natural',
    'Value + 272.15', 'Value - 272.15'
  );

end;

initialization
  RegisterUOM;
end.
