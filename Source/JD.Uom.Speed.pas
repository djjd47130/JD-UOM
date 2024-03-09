unit JD.Uom.Speed;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom,
  JD.Uom.Time,
  JD.Uom.Distance;

const
  FACTOR_MM_PER_SECOND = 277.777777777777777777777778;
  FACTOR_CM_PER_SECOND = 27.777777777777777777777778;
  FACTOR_M_PER_SECOND =  3.6;
  FACTOR_KM_PER_SECOND = 3600;
  FACTOR_MM_PER_HOUR =   1000000;
  FACTOR_CM_PER_HOUR =   100000;
  FACTOR_M_PER_HOUR =    1000;
  FACTOR_KM_PER_HOUR =   1;
  FACTOR_FOOT_PER_SECOND = 0.911344;
  FACTOR_MILE_PER_HOUR = 1.609344;
  FACTOR_KNOT =          1.852;
  FACTOR_MACH =          1235;
  FACTOR_LIGHTSPEED =    1079252848.7999;

implementation

{ $DEFINE DYNAMIC_REG}

procedure RegisterUOM;
{$IFDEF DYNAMIC_REG}
const
  DIST_UNITS: array of String = ['Millimeter', 'Meter', 'Kilometer', 'Inch', 'Foot', 'Mile'];
  TIME_UNITS: array of String = ['Second', 'Minute', 'Hour', 'Day'];
var
  DU, TU: TUOM;
  XDist, XTime: Integer;
  NameSingular, NamePlural, Suffix: String;
  DistFactor, TimeFactor: Double;
{$ENDIF}
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


{$IFDEF DYNAMIC_REG}

  //Dynamically register UOMs based on existing Distance and Time UOMs...

  for XDist := Low(DIST_UNITS) to High(DIST_UNITS) do begin
    DU:= TUOMUtils.GetUOMByName(DIST_UNITS[XDist]);
    for XTime := Low(TIME_UNITS) to High(TIME_UNITS) do begin
      TU:= TUOMUtils.GetUOMByName(TIME_UNITS[XTime]);
      NameSingular:= DU.NameSingular+' per '+TU.NameSingular;
      NamePlural:= DU.NamePlural+' per '+TU.NameSingular;
      Suffix:= DU.Suffix+'/'+TU.Suffix;
      DistFactor:= DU.ConvertToBase(1);
      TimeFactor:= TU.ConvertToBase(1);

      TUOMUtils.RegisterUOM('Speed',
        NameSingular,
        NamePlural,
        '',
        Suffix,
        TU.Systems.DelimitedText,
        {$IFDEF USE_MATH_EXPR}
        //TODO: Clearly this is wrong...
        'Value / (DistFactor / TimeFactor)',
        'Value * (DistFactor / TimeFactor)'
        {$ELSE}
        function(const Value: Double): Double
        begin
          //Base to Unit - TODO: Clearly this is wrong...
          Result:= Value / (DistFactor / TimeFactor);
        end,
        function(const Value: Double): Double
        begin
          //Unit to Base - TODO: Clearly this is wrong...
          Result:= Value * (DistFactor / TimeFactor);
        end
        {$ENDIF}
      );
    end;
  end;

  TUOMUtils.RegisterBaseUOM('Speed', TUOMUtils.GetUOMByName('Kilometer per Hour'));

{$ELSE}

  //Metric

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Second', 'Millimeters per Second', '', 'mm/s', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_MM_PER_SECOND), D(FACTOR_MM_PER_SECOND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Millimeters per Second
      Result:= Value * FACTOR_MM_PER_SECOND;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters per Second to Base
      Result:= Value / FACTOR_MM_PER_SECOND;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Second', 'Centimeters per Second', '', 'cm/s', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_CM_PER_SECOND), D(FACTOR_CM_PER_SECOND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Centimeters per Second
      Result:= Value * FACTOR_CM_PER_SECOND;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters per Second to Base
      Result:= Value / FACTOR_CM_PER_SECOND;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Meter per Second', 'Meters per Second', '', 'm/s', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_M_PER_SECOND), M(FACTOR_M_PER_SECOND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Meters per Second
      Result:= Value / FACTOR_M_PER_SECOND;
    end,
    function(const Value: Double): Double
    begin
      //Meters per Second to Base
      Result:= Value * FACTOR_M_PER_SECOND;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Kilometer per Second', 'Kilometers per Second', '', 'km/s', 'Metric (Huge)',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_KM_PER_SECOND), M(FACTOR_KM_PER_SECOND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Kilometers per Second
      Result:= Value / FACTOR_KM_PER_SECOND;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers per Second to Base
      Result:= Value * FACTOR_KM_PER_SECOND;
    end
  {$ENDIF}
  );

  //TODO: Add Per Minute for each...

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Hour', 'Millimeters per Hour', '', 'mm/h', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_MM_PER_HOUR), D(FACTOR_MM_PER_HOUR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Millimeters per Hour
      Result:= Value * FACTOR_MM_PER_HOUR;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters per Hour to Base
      Result:= Value / FACTOR_MM_PER_HOUR;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Hour', 'Centimeters per Hour', '', 'cm/h', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_CM_PER_HOUR), D(FACTOR_CM_PER_HOUR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Centimeters per Hour
      Result:= Value * FACTOR_CM_PER_HOUR;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters per Hour to Base
      Result:= Value / FACTOR_CM_PER_HOUR;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Meter per Hour', 'Meters per Hour', '', 'm/h', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_M_PER_HOUR), D(FACTOR_M_PER_HOUR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Meters per Hour
      Result:= Value * FACTOR_M_PER_HOUR;
    end,
    function(const Value: Double): Double
    begin
      //Meters per Hour to Base
      Result:= Value / FACTOR_M_PER_HOUR;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Kilometer per Hour', 'Kilometers per Hour', '', 'km/h', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_KM_PER_HOUR), M(FACTOR_KM_PER_HOUR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Kilometers per Hour
      Result:= Value / FACTOR_KM_PER_HOUR;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers per Hour to Base
      Result:= Value * FACTOR_KM_PER_HOUR;
    end
  {$ENDIF}
  ).SetAsBase;

  //Imperial / US Customary

  TUOMUtils.RegisterUOM('Speed',
    'Foot per Second', 'Feet per Second', '', 'ft/s', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(FACTOR_FOOT_PER_SECOND), D(FACTOR_FOOT_PER_SECOND)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Feet per Second
      Result:= Value * FACTOR_FOOT_PER_SECOND;
    end,
    function(const Value: Double): Double
    begin
      //Feet per Second to Base
      Result:= Value / FACTOR_FOOT_PER_SECOND;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Mile per Hour', 'Miles per Hour', '', 'mph', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_MILE_PER_HOUR), M(FACTOR_MILE_PER_HOUR)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Miles per Hour
      Result:= Value / FACTOR_MILE_PER_HOUR;
    end,
    function(const Value: Double): Double
    begin
      //Miles per Hour to Base
      Result:= Value * FACTOR_MILE_PER_HOUR;
    end
  {$ENDIF}
  );

{$ENDIF}

  TUOMUtils.RegisterUOM('Speed',
    'Knot', 'Knots', '', 'kt', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_KNOT), M(FACTOR_KNOT)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Knots
      Result:= Value / FACTOR_KNOT;
    end,
    function(const Value: Double): Double
    begin
      //Knots to Base
      Result:= Value * FACTOR_KNOT;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Mach', 'Mach', '', 'M', 'Natural',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_MACH), M(FACTOR_MACH)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Mach
      Result:= Value / FACTOR_MACH;
    end,
    function(const Value: Double): Double
    begin
      //Mach to Base
      Result:= Value * FACTOR_MACH;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Speed',
    'Lightspeed', 'Lightspeed', '', 'c', 'Natural',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_LIGHTSPEED), M(FACTOR_LIGHTSPEED)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Base to Lightspeed
      Result:= Value / FACTOR_LIGHTSPEED;
    end,
    function(const Value: Double): Double
    begin
      //Lightspeed to Base
      Result:= Value * FACTOR_LIGHTSPEED;
    end
  {$ENDIF}
  );

end;

initialization
  RegisterUOM;
end.
