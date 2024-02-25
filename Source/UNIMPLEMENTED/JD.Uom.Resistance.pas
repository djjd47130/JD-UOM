unit JD.Uom.Resistance;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMResistanceUnit = (umcOhms, umcSiemens);
  TUOMResistanceUnits = set of TUOMResistanceUnit;

  TUOMResistanceUtils = class

  end;

  TUOMResistance = record

  end;

implementation

var
  DefaultResistanceUnit: TUOMResistanceUnit;

initialization
  DefaultResistanceUnit:= TUOMResistanceUnit.umcOhms;
end.
