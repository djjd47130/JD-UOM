unit JD.Uom.Speed;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMSpeedUnit = (umsCentimetersPerSecond, umsMetersPerSecond, umsKilometersPerHour,
    umsFeetPerSecond, umsMilesPerHour, umsKnots, umsMach, umsLightspeed);
  TUOMSpeedUnits = set of TUOMSpeedUnit;

  TUOMSpeedUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMSpeedUnit): String; static;
    class function UnitName(const AValue: TUOMSpeedUnit): String; static;
  end;

  TUOMSpeed = record

  end;

implementation

var
  DefaultSpeedUnit: TUOMSpeedUnit;

{ TUOMSpeedUtils }

class procedure TUOMSpeedUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Centimeters per Second');
  AList.Append('Meters per Second');
  AList.Append('Kilometers per Hour');
  AList.Append('Feet per Second');
  AList.Append('Miles per Hour');
  AList.Append('Knots');
  AList.Append('Mach');
  AList.Append('Lightspeed');
end;

class function TUOMSpeedUtils.UnitName(const AValue: TUOMSpeedUnit): String;
begin
  case AValue of
    umsCentimetersPerSecond:  Result:= 'Centimetrs per Second';
    umsMetersPerSecond:       Result:= 'Meters per Second';
    umsKilometersPerHour:     Result:= 'Kilometers per Hour';
    umsFeetPerSecond:         Result:= 'Feet per Second';
    umsMilesPerHour:          Result:= 'Miles per Hour';
    umsKnots:                 Result:= 'Knots';
    umsMach:                  Result:= 'Mach';
    umsLightspeed:            Result:= 'Lightspeed';
  end;
end;

class function TUOMSpeedUtils.UnitSuffix(const AValue: TUOMSpeedUnit): String;
begin
  case AValue of
    umsCentimetersPerSecond:  Result:= 'cm/s';
    umsMetersPerSecond:       Result:= 'm/s';
    umsKilometersPerHour:     Result:= 'km/h';
    umsFeetPerSecond:         Result:= 'ft/s';
    umsMilesPerHour:          Result:= 'mph';
    umsKnots:                 Result:= '';
    umsMach:                  Result:= '';
    umsLightspeed:            Result:= 'c';
  end;
end;

initialization
  DefaultSpeedUnit:= TUOMSpeedUnit.umsMilesPerHour;
end.
