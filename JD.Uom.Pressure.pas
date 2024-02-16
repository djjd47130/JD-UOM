unit JD.Uom.Pressure;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMPressureUnit = (umrAtmospheres, umrBars, umrKiloPascals, umrMillimetersOfMercury,
    umrPascals, umrPoundsPerSquareInch);
  TUOMPressureUnits = set of TUOMPressureUnit;

  TUOMPressureUtils = class

  end;

  TUOMPressure = record

  end;

implementation

var
  DefaultPressureUnit: TUOMPressureUnit;

initialization
  DefaultPressureUnit:= TUOMPressureUnit.umrBars;
end.
