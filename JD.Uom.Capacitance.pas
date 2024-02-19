unit JD.Uom.Capacitance;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMCapacitanceUnit = (umcFarad, umcMicroFarad, umcNanoFarad, umcPicoFarad,
    umcFemtoFarad);
  TUOMCapacitanceUnits = set of TUOMCapacitanceUnit;

  TUOMCapacitanceUtils = class

  end;

  TUOMCapacitance = record

  end;

implementation

var
  DefaultCapacitanceUnit: TUOMCapacitanceUnit;

initialization
  DefaultCapacitanceUnit:= TUOMCapacitanceUnit.umcFarad;
end.
