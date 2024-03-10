unit JD.Uom.Time;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

const
  HoursPerDay = 24;
  DaysPerYear = 365.25;
  MonthsPerYear = 12;
  DaysPerWeek = 7;
  YearsPerDecade = 10;
  YearsPerCentury = 100;
  YearsPerMillennium = 1000;
  YearsPerHumanLifespan = 77.5;
  MinutesPerHour = 60;
  SecondsPerMinute = 60;
  MillisecondsPerDay = 86400000;
  MicrosecondsPerDay = 86400000000;
  NanosecondsPerDay = 86400000000000;
  SecondsPerHour = SecondsPerMinute * MinutesPerHour;
  DaysPerMonth = DaysPerYear / MonthsPerYear;
  WeeksPerYear = DaysPerYear / DaysPerWeek;
  DaysPerDecade = DaysPerYear * YearsPerDecade;
  DaysPerCentury = DaysPerYear * YearsPerCentury;
  DaysPerMillennium = DaysPerYear * YearsPerMillennium;
  DaysPerHumanLifespan = DaysPerYear * YearsPerHumanLifespan;
  MinutesPerDay = HoursPerDay * MinutesPerHour;
  SecondsPerDay = HoursPerDay * MinutesPerHour;

implementation

procedure RegisterUOM;
  function F(const V: Double): String;
  begin
    Result:= FormatFloat(NumInternalFormat, V);
  end;
  function D(const V: Double): String;
  begin
    Result:= 'Value / '+F(V);
  end;
  function M(const V: Double): String;
  begin
    Result:= 'Value * '+F(V);
  end;
begin

  TUOMUtils.RegisterUOM('Time',
    'Nanosecond', 'Nanoseconds', 'ns', 'Metric (Tiny)',
    M(NanosecondsPerDay), D(NanosecondsPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterUOM('Time',
    'Microsecond', 'Microseconds', 'μs', 'Metric (Tiny)',
    M(MicrosecondsPerDay), D(MicrosecondsPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterUOM('Time',
    'Millisecond', 'Milliseconds', 'ms', 'Metric (Tiny)',
    M(MillisecondsPerDay), D(MillisecondsPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterUOM('Time',
    'Second', 'Seconds', 'sec', 'Metric,Imperial,US Customary,Natural',
    M(SecondsPerDay), D(SecondsPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterUOM('Time',
    'Minute', 'Minutes', 'min', 'Metric,Imperial,US Customary,Natural',
    M(MinutesPerDay), D(MinutesPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterUOM('Time',
    'Hour', 'Hours', 'hr', 'Metric,Imperial,US Customary,Natural',
    M(HoursPerDay), D(HoursPerDay)); //TODO: Reverse values...

  TUOMUtils.RegisterSimpleUOM('Time',
    'Day', 'Days', 'd', 'Metric,Imperial,US Customary,Natural', 1).SetAsBase;

  TUOMUtils.RegisterSimpleUOM('Time',
    'Week', 'Weeks', 'wk', 'Metric,Imperial,US Customary,Natural', DaysPerWeek);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Month', 'Months', 'mo', 'Metric,Imperial,US Customary,Natural', DaysPerMonth);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Year', 'Years', 'yr', 'Metric,Imperial,US Customary,Natural', DaysPerYear);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Decade', 'Decades', 'dec', 'Metric,Imperial,US Customary,Natural', DaysPerDecade);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Century', 'Centuries', 'cen', 'Metric (Huge),Imperial (Huge),US Customary (Huge),Natural', DaysPerCentury);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Millennium', 'Millennia', 'mill', 'Metric (Huge),Imperial (Huge),US Customary (Huge),Natural', DaysPerMillennium);

  TUOMUtils.RegisterSimpleUOM('Time',
    'Human Lifespan', 'Human Lifespans', 'HLs', 'Random', DaysPerHumanLifespan);

end;


initialization
  RegisterUOM;
end.
