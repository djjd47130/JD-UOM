unit JD.Uom.Voltage;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMVoltageUnit = (umoVolts, umoKiloVolts);
  TUOMVoltageUnits = set of TUOMVoltageUnit;

  TYOMVoltageUtils = class

  end;

  TUOMVoltage = record

  end;

implementation

var
  DefaultVoltageUnit: TUOMVoltageUnit;

initialization
  DefaultVoltageUnit:= TUOMVoltageUnit.umoVolts;
end.
