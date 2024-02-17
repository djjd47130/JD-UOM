unit JD.Uom.Time;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMTimeUnit = (ummMicroSeconds, ummMilliSeconds, ummSeconds, ummMinutes, ummHours,
    ummDays, ummWeeks, ummMonths, ummYears, ummDecades);
  TUOMTimeUnits = set of TUOMTimeUnit;

  TUOMTimeUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMTimeUnit): String; static;
    class function UnitName(const AValue: TUOMTimeUnit): String; static;
  public
    //Micro Seconds
    class function MilliSecondsToMicroSeconds(const AMilliSeconds: Double): Double;
    class function SecondsToMicroSeconds(const ASeconds: Double): Double;
    class function MinutesToMicroSeconds(const AMinutes: Double): Double;
    class function HoursToMicroSeconds(const AHours: Double): Double;
    class function DaysToMicroSeconds(const ADays: Double): Double;
    class function WeeksToMicroSeconds(const AWeeks: Double): Double;
    class function MonthsToMicroSeconds(const AMonths: Double): Double;
    class function YearsToMicroSeconds(const AYears: Double): Double;
    class function DecadesToMicroSeconds(const ADecades: Double): Double;
    //Milli Seconds
    class function MicroSecondsToMilliSeconds(const AMicroSeconds: Double): Double;
    class function SecondsToMilliSeconds(const ASeconds: Double): Double;
    class function MinutesToMilliSeconds(const AMinutes: Double): Double;
    class function HoursToMilliSeconds(const AHours: Double): Double;
    class function DaysToMilliSeconds(const ADays: Double): Double;
    class function WeeksToMilliSeconds(const AWeeks: Double): Double;
    class function MonthsToMilliSeconds(const AMonths: Double): Double;
    class function YearsToMilliSeconds(const AYears: Double): Double;
    class function DecadesToMilliSeconds(const ADecades: Double): Double;
    //Seconds
    class function MicroSecondsToSeconds(const AMicroSeconds: Double): Double;
    class function MilliSecondsToSeconds(const AMilliSeconds: Double): Double;
    class function MinutesToSeconds(const AMinutes: Double): Double;
    class function HoursToSeconds(const AHours: Double): Double;
    class function DaysToSeconds(const ADays: Double): Double;
    class function WeeksToSeconds(const AWeeks: Double): Double;
    class function MonthsToSeconds(const AMonths: Double): Double;
    class function YearsToSeconds(const AYears: Double): Double;
    class function DecadesToSeconds(const ADecades: Double): Double;
    //Minutes
    class function MicroSecondsToMinutes(const AMicroSeconds: Double): Double;
    class function MilliSecondsToMinutes(const AMilliSeconds: Double): Double;
    class function SecondsToMinutes(const ASeconds: Double): Double;
    class function HoursToMinutes(const AHours: Double): Double;
    class function DaysToMinutes(const ADays: Double): Double;
    class function WeeksToMinutes(const AWeeks: Double): Double;
    class function MonthsToMinutes(const AMonths: Double): Double;
    class function YearsToMinutes(const AYears: Double): Double;
    class function DecadesToMinutes(const ADecades: Double): Double;
    //Hours
    class function MicroSecondsToHours(const AMicroSeconds: Double): Double;
    class function MilliSecondsToHours(const AMilliSeconds: Double): Double;
    class function SecondsToHours(const ASeconds: Double): Double;
    class function MinutesToHours(const AMinutes: Double): Double;
    class function DaysToHours(const ADays: Double): Double;
    class function WeeksToHours(const AWeeks: Double): Double;
    class function MonthsToHours(const AMonths: Double): Double;
    class function YearsToHours(const AYears: Double): Double;
    class function DecadesToHours(const ADecades: Double): Double;
    //Days
    class function MicroSecondsToDays(const AMicroSeconds: Double): Double;
    class function MilliSecondsToDays(const AMilliSeconds: Double): Double;
    class function SecondsToDays(const ASeconds: Double): Double;
    class function MinutesToDays(const AMinutes: Double): Double;
    class function HoursToDays(const AHours: Double): Double;
    class function WeeksToDays(const AWeeks: Double): Double;
    class function MonthsToDays(const AMonths: Double): Double;
    class function YearsToDays(const AYears: Double): Double;
    class function DecadesToDays(const ADecades: Double): Double;
    //Weeks
    class function MicroSecondsToWeeks(const AMicroSeconds: Double): Double;
    class function MilliSecondsToWeeks(const AMilliSeconds: Double): Double;
    class function SecondsToWeeks(const ASeconds: Double): Double;
    class function MinutesToWeeks(const AMinutes: Double): Double;
    class function HoursToWeeks(const AHours: Double): Double;
    class function DaysToWeeks(const ADays: Double): Double;
    class function MonthsToWeeks(const AMonths: Double): Double;
    class function YearsToWeeks(const AYears: Double): Double;
    class function DecadesToWeeks(const ADecades: Double): Double;
    //Months
    class function MicroSecondsToMonths(const AMicroSeconds: Double): Double;
    class function MilliSecondsToMonths(const AMilliSeconds: Double): Double;
    class function SecondsToMonths(const ASeconds: Double): Double;
    class function MinutesToMonths(const AMinutes: Double): Double;
    class function HoursToMonths(const AHours: Double): Double;
    class function DaysToMonths(const ADays: Double): Double;
    class function WeeksToMonths(const AWeeks: Double): Double;
    class function YearsToMonths(const AYears: Double): Double;
    class function DecadesToMonths(const ADecades: Double): Double;
    //Years
    class function MicroSecondsToYears(const AMicroSeconds: Double): Double;
    class function MilliSecondsToYears(const AMilliSeconds: Double): Double;
    class function SecondsToYears(const ASeconds: Double): Double;
    class function MinutesToYears(const AMinutes: Double): Double;
    class function HoursToYears(const AHours: Double): Double;
    class function DaysToYears(const ADays: Double): Double;
    class function WeeksToYears(const AWeeks: Double): Double;
    class function MonthsToYears(const AMonths: Double): Double;
    class function DecadesToYears(const ADecades: Double): Double;
    //Decades
    class function MicroSecondsToDecades(const AMicroSeconds: Double): Double;
    class function MilliSecondsToDecades(const AMilliSeconds: Double): Double;
    class function SecondsToDecades(const ASeconds: Double): Double;
    class function MinutesToDecades(const AMinutes: Double): Double;
    class function HoursToDecades(const AHours: Double): Double;
    class function DaysToDecades(const ADays: Double): Double;
    class function WeeksToDecades(const AWeeks: Double): Double;
    class function MonthsToDecades(const AMonths: Double): Double;
    class function YearsToDecades(const AYears: Double): Double;
  end;

  TUOMTime = record
  private
    FUnit: TUOMTimeUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMTimeUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMTimeUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMTime;
    class operator implicit(const AValue: TUOMTime): Double;
    class operator implicit(const AValue: String): TUOMTime;
    class operator implicit(const AValue: TUOMTime): String;
  public
    function ToMicroSeconds: Double;
    function ToMilliSeconds: Double;
    function ToSeconds: Double;
    function ToMinutes: Double;
    function ToHours: Double;
    function ToDays: Double;
    function ToWeeks: Double;
    function ToMonths: Double;
    function ToYears: Double;
    function ToDecades: Double;
  end;

implementation

var
  DefaultTimeUnit: TUOMTimeUnit;

{ TUOMTimeUtils }

class function TUOMTimeUtils.DaysToDecades(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToHours(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToMicroSeconds(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToMilliSeconds(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToMinutes(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToMonths(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToSeconds(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToWeeks(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DaysToYears(const ADays: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToDays(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToHours(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToMicroSeconds(
  const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToMilliSeconds(
  const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToMinutes(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToMonths(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToSeconds(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToWeeks(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.DecadesToYears(const ADecades: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToDays(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToDecades(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToMicroSeconds(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToMilliSeconds(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToMinutes(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToMonths(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToSeconds(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToWeeks(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.HoursToYears(const AHours: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToDays(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToDecades(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToHours(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToMilliSeconds(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToMinutes(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToMonths(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToSeconds(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToWeeks(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MicroSecondsToYears(
  const AMicroSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToDays(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToDecades(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToHours(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToMicroSeconds(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToMinutes(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToMonths(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToSeconds(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToWeeks(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MilliSecondsToYears(
  const AMilliSeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToDays(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToDecades(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToHours(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToMicroSeconds(
  const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToMilliSeconds(
  const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToMonths(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToSeconds(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToWeeks(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MinutesToYears(const AMinutes: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToDays(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToDecades(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToHours(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToMicroSeconds(
  const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToMilliSeconds(
  const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToMinutes(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToSeconds(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToWeeks(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.MonthsToYears(const AMonths: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToDays(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToDecades(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToHours(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToMicroSeconds(
  const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToMilliSeconds(
  const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToMinutes(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToMonths(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToWeeks(const ASeconds: Double): Double;
begin

end;

class function TUOMTimeUtils.SecondsToYears(const ASeconds: Double): Double;
begin

end;

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
  AList.Append('Months');
  AList.Append('Years');
  AList.Append('Decades');
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
    ummMonths:        Result:= 'Months';
    ummYears:         Result:= 'Years';
    ummDecades:       Result:= 'Decades';
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
    ummMonths:        Result:= 'Mo';
    ummYears:         Result:= 'y';
    ummDecades:       Result:= 'Dec';
  end;
end;

class function TUOMTimeUtils.WeeksToDays(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToDecades(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToHours(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToMicroSeconds(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToMilliSeconds(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToMinutes(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToMonths(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToSeconds(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.WeeksToYears(const AWeeks: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToDays(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToDecades(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToHours(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToMicroSeconds(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToMilliSeconds(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToMinutes(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToMonths(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToSeconds(const AYears: Double): Double;
begin

end;

class function TUOMTimeUtils.YearsToWeeks(const AYears: Double): Double;
begin

end;

{ TUOMTime }

class operator TUOMTime.implicit(const AValue: Double): TUOMTime;
begin

end;

class operator TUOMTime.implicit(const AValue: TUOMTime): Double;
begin

end;

class operator TUOMTime.implicit(const AValue: String): TUOMTime;
begin

end;

class operator TUOMTime.implicit(const AValue: TUOMTime): String;
begin

end;

procedure TUOMTime.SetUnit(const Value: TUOMTimeUnit);
begin

end;

procedure TUOMTime.SetValue(const Value: Double);
begin

end;

function TUOMTime.ToDays: Double;
begin

end;

function TUOMTime.ToDecades: Double;
begin

end;

function TUOMTime.ToHours: Double;
begin

end;

function TUOMTime.ToMicroSeconds: Double;
begin
  case FUnit of
    ummMicroSeconds: Result:= FValue;
    ummMilliSeconds: Result:= TUOMTimeUtils.MilliSecondsToMicroSeconds(FValue);
    ummSeconds: Result:= TUOMTimeUtils.SecondsToMicroSeconds(FValue);
    ummMinutes: Result:= TUOMTimeUtils.MinutesToMicroSeconds(FValue);
    ummHours: Result:= TUOMTimeUtils.HoursToMicroSeconds(FValue);
    ummDays: Result:= TUOMTimeUtils.DaysToMicroSeconds(FValue);
    ummWeeks: Result:= TUOMTimeUtils.WeeksToMicroSeconds(FValue);
    ummMonths: Result:= TUOMTimeUtils.MonthsToMicroSeconds(FValue);
    ummYears: Result:= TUOMTimeUtils.YearsToMicroSeconds(FValue);
    ummDecades: Result:= TUOMTimeUtils.DecadesToMicroSeconds(FValue);
  end;
end;

function TUOMTime.ToMilliSeconds: Double;
begin
  case FUnit of
    ummMicroSeconds: Result:= TUOMTimeUtils.MicroSecondsToMilliSeconds(FValue);
    ummMilliSeconds: Result:= FValue;
    ummSeconds: Result:= TUOMTimeUtils.SecondsToMilliSeconds(FValue);
    ummMinutes: Result:= TUOMTimeUtils.MinutesToMilliSeconds(FValue);
    ummHours: Result:= TUOMTimeUtils.HoursToMilliSeconds(FValue);
    ummDays: Result:= TUOMTimeUtils.DaysToMilliSeconds(FValue);
    ummWeeks: Result:= TUOMTimeUtils.WeeksToMilliSeconds(FValue);
    ummMonths: Result:= TUOMTimeUtils.MonthsToMilliSeconds(FValue);
    ummYears: Result:= TUOMTimeUtils.YearsToMilliSeconds(FValue);
    ummDecades: Result:= TUOMTimeUtils.DecadesToMilliSeconds(FValue);
  end;
end;

function TUOMTime.ToMinutes: Double;
begin

end;

function TUOMTime.ToMonths: Double;
begin

end;

function TUOMTime.ToSeconds: Double;
begin

end;

function TUOMTime.ToWeeks: Double;
begin

end;

function TUOMTime.ToYears: Double;
begin

end;

initialization
  DefaultTimeUnit:= TUOMTimeUnit.ummMinutes;
end.
