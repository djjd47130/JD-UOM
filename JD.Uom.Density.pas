unit JD.Uom.Density;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMDensityUnit = (umyKiloGramsPerCubicMeter);
  TUOMDensityUnits = set of TUOMDensityUnit;

  TUOMDensityUtils = class

  end;

  TUOMDensity = record

  end;

implementation

var
  DefaultDensityUnit: TUOMDensityUnit;

initialization
  DefaultDensityUnit:= TUOMDensityUnit.umyKiloGramsPerCubicMeter;
end.
