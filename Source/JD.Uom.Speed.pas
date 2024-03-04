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

{ $DEFINE DYNAMIC_REG}

procedure RegisterUOM;
{$IFDEF DYNAMIC_REG}
var
  XDist, XTime: Integer;
  sDistSingular, sDistPlural, sDistSuf: String;
  DistFactor: Double;
  sTime, sTimeSuf: String;
  TimeFactor: Double;
{$ENDIF}
begin
{$IFDEF DYNAMIC_REG}
  //TODO: Change to dynamically create in a loop...
  //Millimeter
  //Centimeter
  //Meter
  //Kilometer

  //Second
  //Minute
  //Hour

  //Metric
  for XDist := 1 to 4 do begin
    //TODO: Instead, iterate values already registered "Distance" UOMs...
    case XDist of
      1: begin
        sDistSingular:= 'Millimeter';
        sDistSuf:= 'mm';
        DistFactor:= 1000000;
      end;
      2: begin
        sDistSingular:= 'Centimeter';
        sDistSuf:= 'cm';
        DistFactor:= 100000;
      end;
      3: begin
        sDistSingular:= 'Meter';
        sDistSuf:= 'm';
        DistFactor:= 1000;
      end;
      4: begin
        sDistSingular:= 'Kilometer';
        sDistSuf:= 'km';
        DistFactor:= 1;
      end;
    end;
    sDistPlural:= sDistSingular + 's';
    for XTime := 1 to 3 do begin
      case XTime of
        1: begin
          sTime:= 'Second';
          sTimeSuf:= 's';
          TimeFactor:= 3600;
        end;
        2: begin
          sTime:= 'Minute';
          sTimeSuf:= 'm';
          TimeFactor:= 60;
        end;
        3: begin
          sTime:= 'Hour';
          sTimeSuf:= 'h';
          TimeFactor:= 1;
        end;
      end;

      //ACTUAL UOM REGISTRATION
      TUOMUtils.RegisterUOM('Speed',
        sDistSingular+' per '+sTime,
        sDistPlural+' per '+sTime,
        '',
        sDistSuf+'/'+sTimeSuf,
        'Metric',
        function(const Value: Double): Double
        begin
          //Base to Unit - TODO
          Result:= Value / (DistFactor / TimeFactor);
        end,
        function(const Value: Double): Double
        begin
          //Unit to Base - TODO
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

{$ENDIF}


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
