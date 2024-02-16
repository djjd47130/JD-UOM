unit JD.Uom.Time;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMTimeUnit = (ummMicroSeconds, ummMilliSeconds, ummSeconds, ummMinutes, ummHours,
    ummDays, ummWeeks, ummYears);
  TUOMTimeUnits = set of TUOMTimeUnit;

  TUOMTimeUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMTimeUnit): String; static;
    class function UnitName(const AValue: TUOMTimeUnit): String; static;
  end;

  TUOMTime = record

  end;

implementation

var
  DefaultTimeUnit: TUOMTimeUnit;

{ TUOMTimeUtils }

class procedure TUOMTimeUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Microseconds');
  AList.Append('Milliseconds');
  AList.Append('Seconds');
  AList.Append('Minutes');
  AList.Append('Hours');
  AList.Append('Days');
  AList.Append('Weeks');
  AList.Append('Years');
end;

class function TUOMTimeUtils.UnitName(const AValue: TUOMTimeUnit): String;
begin
  case AValue of
    ummMicroSeconds:  Result:= 'Microseconds';
    ummMilliSeconds:  Result:= 'Milliseconds';
    ummSeconds:       Result:= 'Seconds';
    ummMinutes:       Result:= 'Minutes';
    ummHours:         Result:= 'Hours';
    ummDays:          Result:= 'Days';
    ummWeeks:         Result:= 'Weeks';
    ummYears:         Result:= 'Years';
  end;
end;

class function TUOMTimeUtils.UnitSuffix(const AValue: TUOMTimeUnit): String;
begin
  case AValue of
    ummMicroSeconds:  Result:= 'μs';
    ummMilliSeconds:  Result:= 'ms';
    ummSeconds:       Result:= 's';
    ummMinutes:       Result:= 'm';
    ummHours:         Result:= 'h';
    ummDays:          Result:= 'd';
    ummWeeks:         Result:= 'w';
    ummYears:         Result:= 'y';
  end;
end;

initialization
  DefaultTimeUnit:= TUOMTimeUnit.ummMinutes;
end.
