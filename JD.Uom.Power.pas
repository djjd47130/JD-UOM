unit JD.Uom.Power;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMPowerUnit = (umpWatts, umpKiloWatts, umpHorsePower, umpFootPoundsPerMinute,
    umpBTUsPerMinute);
  TUOMPowerUnits = set of TUOMPowerUnit;

  TUOMPowerUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMPowerUnit): String; static;
    class function UnitName(const AValue: TUOMPowerUnit): String; static;
  end;

  TUOMPower = record

  end;

implementation

var
  DefaultPowerUnit: TUOMPowerUnit;

{ TUOMPowerUtils }

class procedure TUOMPowerUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Watts');
  AList.Append('Kilowatts');
  AList.Append('Horsepower');
  AList.Append('Foot Pounds per Minute');
  AList.Append('BTUs per Minute');
end;

class function TUOMPowerUtils.UnitName(const AValue: TUOMPowerUnit): String;
begin
  case AValue of
    umpWatts:               Result:= 'Watts';
    umpKiloWatts:           Result:= 'Kilowatts';
    umpHorsePower:          Result:= 'Horsepower';
    umpFootPoundsPerMinute: Result:= 'Foot Pounds per Minute';
    umpBTUsPerMinute:       Result:= 'BTUs per Minute';
  end;
end;

class function TUOMPowerUtils.UnitSuffix(const AValue: TUOMPowerUnit): String;
begin
  case AValue of
    umpWatts:               Result:= 'W';
    umpKiloWatts:           Result:= 'KW';
    umpHorsePower:          Result:= 'HP';
    umpFootPoundsPerMinute: Result:= 'fp';
    umpBTUsPerMinute:       Result:= 'BTUs';
  end;
end;

initialization
  DefaultPowerUnit:= TUOMPowerUnit.umpWatts;
end.
