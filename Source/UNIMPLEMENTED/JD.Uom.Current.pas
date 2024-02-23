unit JD.Uom.Current;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMCurrentUnit = (umnAmps);
  TUOMCurrentUnits = set of TUOMCurrentUnit;

  TUOMCurrentUtils = class

  end;

  TUOMCurrent = record

  end;

implementation

var
  DefaultCurrentUnit: TUOMCurrentUnit;

initialization
  DefaultCurrentUnit:= TUOMCurrentUnit.umnAmps;
end.
