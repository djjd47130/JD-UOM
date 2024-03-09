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
    'Nanosecond', 'Nanoseconds', '', 'ns', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    M(NanosecondsPerDay), D(NanosecondsPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Nanoseconds
      Result:= Value * NanosecondsPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Nanoseconds to Days
      Result:= Value / NanosecondsPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Microsecond', 'Microseconds', '', 'μs', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    M(MicrosecondsPerDay), D(MicrosecondsPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Microseconds
      Result:= Value * MicrosecondsPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Microseconds to Days
      Result:= Value / MicrosecondsPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Millisecond', 'Milliseconds', '', 'ms', 'Metric (Tiny)',
  {$IFDEF USE_MATH_EXPR}
    M(MillisecondsPerDay), D(MillisecondsPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Milliseconds
      Result:= Value * MillisecondsPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Milliseconds to Days
      Result:= Value / MillisecondsPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Second', 'Seconds', '', 'sec', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    M(SecondsPerDay), D(SecondsPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Seconds
      Result:= Value * SecondsPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Seconds to Days
      Result:= Value / SecondsPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Minute', 'Minutes', '', 'min', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    M(MinutesPerDay), D(MinutesPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Minutes
      Result:= Value * MinutesPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Minutes to Days
      Result:= Value / MinutesPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Hour', 'Hours', '', 'hr', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    M(HoursPerDay), D(HoursPerDay)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Hours
      Result:= Value * HoursPerDay;
    end,
    function(const Value: Double): Double
    begin
      //Hours to Days
      Result:= Value / HoursPerDay;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Day', 'Days', '', 'd', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    M(1), D(1)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Days
      Result:= Value * 1;
    end,
    function(const Value: Double): Double
    begin
      //Days to Days
      Result:= Value / 1;
    end
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Time',
    'Week', 'Weeks', '', 'wk', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerWeek), M(DaysPerWeek)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Weeks
      Result:= Value / DaysPerWeek;
    end,
    function(const Value: Double): Double
    begin
      //Weeks to Days
      Result:= Value * DaysPerWeek;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Month', 'Months', '', 'mo', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerMonth), M(DaysPerMonth)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Months
      Result:= Value / DaysPerMonth;
    end,
    function(const Value: Double): Double
    begin
      //Months to Days
      Result:= Value * DaysPerMonth;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Year', 'Years', '', 'yr', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerYear), M(DaysPerYear)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Years
      Result:= Value / DaysPerYear;
    end,
    function(const Value: Double): Double
    begin
      //Years to Days
      Result:= Value * DaysPerYear;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Decade', 'Decades', '', 'dec', 'Metric,Imperial,US Customary,Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerDecade), M(DaysPerDecade)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Decades
      Result:= Value / DaysPerDecade;
    end,
    function(const Value: Double): Double
    begin
      //Decades to Days
      Result:= Value * DaysPerDecade;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Century', 'Centuries', '', 'cen', 'Metric (Huge),Imperial (Huge),US Customary (Huge),Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerCentury), M(DaysPerCentury)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Centuries
      Result:= Value / DaysPerCentury;
    end,
    function(const Value: Double): Double
    begin
      //Centuries to Days
      Result:= Value * DaysPerCentury;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Millennium', 'Millennia', '', 'mill', 'Metric (Huge),Imperial (Huge),US Customary (Huge),Natural',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerMillennium), M(DaysPerMillennium)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Millennia
      Result:= Value / DaysPerMillennium;
    end,
    function(const Value: Double): Double
    begin
      //Millennia to Days
      Result:= Value * DaysPerMillennium;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Time',
    'Human Lifespan', 'Human Lifespans', '', 'HLs', 'Random',
  {$IFDEF USE_MATH_EXPR}
    D(DaysPerHumanLifespan), M(DaysPerHumanLifespan)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Days to Human Lifespans
      Result:= Value / DaysPerHumanLifespan;
    end,
    function(const Value: Double): Double
    begin
      //Human Lifespans to Days
      Result:= Value * DaysPerHumanLifespan;
    end
  {$ENDIF}
  );

end;


initialization
  RegisterUOM;
end.
