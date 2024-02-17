unit JD.Uom.Angle;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMAngleUnit = (umgDegrees, umgRadians, umgGradians, umgPercent);
  TUOMAngleUnits = set of TUOMAngleUnit;

  TUOMAngleUtils = class

  end;

  TUOMAngle = record

  end;

implementation

var
  DefaultAngleUnit: TUOMAngleUnit;

initialization
  DefaultAngleUnit:= TUOMAngleUnit.umgDegrees;
end.
