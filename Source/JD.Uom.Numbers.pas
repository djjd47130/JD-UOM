unit JD.Uom.Numbers;

interface

uses
  System.Classes, System.SysUtils,
  JD.UOM;

implementation

procedure RegisterUOMs;
begin

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'One', 'Ones', '1', 'Natural', 1).SetAsBase;

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Ten', 'Tens', '10', 'Natural', 10);

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Hundred', 'Hundreds', '100', 'Natural', 100);

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Thousand', 'Thousands', '1000', 'Natural', 1000);

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Ten Thousand', 'Ten Thousands', '10000', 'Natural', 10000);

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Hundred Thousand', 'Hundred Thousands', '100000', 'Natural', 100000);

  TUOMUtils.RegisterSimpleUOM('Numbers',
    'Million', 'Millions', '1000000', 'Natural', 1000000);

end;

initialization
  RegisterUOMs;
end.
