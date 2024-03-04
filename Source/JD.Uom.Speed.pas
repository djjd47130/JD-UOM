unit JD.Uom.Speed;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom,
  JD.Uom.Time,
  JD.Uom.Distance;

type
  TUOMSpeedUnit = (umsCentimetersPerSecond, umsMetersPerSecond, umsKilometersPerHour,
    umsFeetPerSecond, umsMilesPerHour, umsKnots, umsMach, umsLightspeed);

implementation

{$DEFINE DYNAMIC_REG}

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
      );
    end;
  end;


  TUOMUtils.RegisterBaseUOM('Speed', TUOMUtils.GetUOMByName('Kilometer per Hour'));

{$ELSE}

  //Metric

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Second', 'Millimeters per Second', '', 'mm/s', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Millimeters per Second
      Result:= Value * 277.777777777777777777777778;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters per Second to Base
      Result:= Value / 277.777777777777777777777778;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Second', 'Centimeters per Second', '', 'cm/s', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Centimeters per Second
      Result:= Value * 27.777777777777777777777778;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters per Second to Base
      Result:= Value / 27.777777777777777777777778;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Meter per Second', 'Meters per Second', '', 'm/s', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Meters per Second
      Result:= Value / 3.6;
    end,
    function(const Value: Double): Double
    begin
      //Meters per Second to Base
      Result:= Value * 3.6;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Kilometer per Second', 'Kilometers per Second', '', 'km/s', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Kilometers per Second
      Result:= Value / 3600;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers per Second to Base
      Result:= Value * 3600;
    end
  );

  //TODO: Add Per Minute for each...

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Hour', 'Millimeters per Hour', '', 'mm/h', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Millimeters per Hour
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Millimeters per Hour to Base
      Result:= Value / 1000000;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Hour', 'Centimeters per Hour', '', 'cm/h', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Centimeters per Hour
      Result:= Value * 100000;
    end,
    function(const Value: Double): Double
    begin
      //Centimeters per Hour to Base
      Result:= Value / 100000;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Meter per Hour', 'Meters per Hour', '', 'm/h', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Meters per Hour
      Result:= Value * 1000;
    end,
    function(const Value: Double): Double
    begin
      //Meters per Hour to Base
      Result:= Value / 1000;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Kilometer per Hour', 'Kilometers per Hour', '', 'km/h', 'Metric',
    function(const Value: Double): Double
    begin
      //Base to Kilometers per Hour
      Result:= Value / 1;
    end,
    function(const Value: Double): Double
    begin
      //Kilometers per Hour to Base
      Result:= Value * 1;
    end
  ).SetAsBase;

  //Imperial / US Customary

  TUOMUtils.RegisterUOM('Speed',
    'Foot per Second', 'Feet per Second', '', 'ft/s', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Base to Feet per Second
      Result:= Value * 0.911344;
    end,
    function(const Value: Double): Double
    begin
      //Feet per Second to Base
      Result:= Value / 0.911344;
    end
  );

  TUOMUtils.RegisterUOM('Speed',
    'Mile per Hour', 'Miles per Hour', '', 'mph', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Base to Miles per Hour
      Result:= Value / 1.609344;
    end,
    function(const Value: Double): Double
    begin
      //Miles per Hour to Base
      Result:= Value * 1.609344;
    end
  );

{$ENDIF}

//  AList.Append('Knots');
//  AList.Append('Mach');
//  AList.Append('Lightspeed');

//    umsKnots:                 Result:= '';
//    umsMach:                  Result:= '';
//    umsLightspeed:            Result:= 'c';

end;

initialization
  RegisterUOM;
end.
