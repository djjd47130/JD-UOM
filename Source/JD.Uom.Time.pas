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
  DaysPerMonth = DaysPerYear / MonthsPerYear;
  WeeksPerYear = DaysPerYear / DaysPerWeek;
  DaysPerDecade = DaysPerYear * YearsPerDecade;
  DaysPerCentury = DaysPerYear * YearsPerCentury;
  DaysPerMillennium = DaysPerYear * YearsPerMillennium;
  DaysPerHumanLifespan = DaysPerYear * YearsPerHumanLifespan;

implementation

procedure RegisterUOM;
begin

  TUOMUtils.RegisterUOM('Time',
    'Nanosecond', 'Nanoseconds', '', 'ns', 'Metric',
    function(const Value: Double): Double
    begin
      //Days to Nanoseconds
      Result:= Value * 86400000000000;
    end,
    function(const Value: Double): Double
    begin
      //Nanoseconds to Days
      Result:= Value / 86400000000000;
    end
  );

  TUOMUtils.RegisterUOM('Time',
    'Microsecond', 'Microseconds', '', 'μs', 'Metric',
    function(const Value: Double): Double
    begin
      //Days to Microseconds
      Result:= Value * 86400000000;
    end,
    function(const Value: Double): Double
    begin
      //Microseconds to Days
      Result:= Value / 86400000000;
    end
  );

  TUOMUtils.RegisterUOM('Time',
    'Millisecond', 'Milliseconds', '', 'ms', 'Metric',
    function(const Value: Double): Double
    begin
      //Days to Milliseconds
      Result:= Value * 86400000;
    end,
    function(const Value: Double): Double
    begin
      //Milliseconds to Days
      Result:= Value / 86400000;
    end
  );

  TUOMUtils.RegisterUOM('Time',
    'Second', 'Seconds', '', 'sec', 'Metric,Imperial,US Customary,Natural',
    function(const Value: Double): Double
    begin
      //Days to Seconds
      Result:= Value * 86400;
    end,
    function(const Value: Double): Double
    begin
      //Seconds to Days
      Result:= Value / 86400;
    end
  );

  TUOMUtils.RegisterUOM('Time',
    'Minute', 'Minutes', '', 'min', 'Metric,Imperial,US Customary,Natural',
    function(const Value: Double): Double
    begin
      //Days to Minutes
      Result:= Value * 1440;
    end,
    function(const Value: Double): Double
    begin
      //Minutes to Days
      Result:= Value / 1440;
    end
  );

  TUOMUtils.RegisterUOM('Time',
    'Hour', 'Hours', '', 'hr', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Day', 'Days', '', 'd', 'Metric,Imperial,US Customary,Natural',
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
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Time',
    'Week', 'Weeks', '', 'wk', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Month', 'Months', '', 'mo', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Year', 'Years', '', 'yr', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Decade', 'Decades', '', 'dec', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Century', 'Centuries', '', 'cen', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Millennium', 'Millennia', '', 'mill', 'Metric,Imperial,US Customary,Natural',
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
  );

  TUOMUtils.RegisterUOM('Time',
    'Human Lifespan', 'Human Lifespans', '', 'HLs', 'Random',
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
  );

end;

initialization
  RegisterUOM;
end.
