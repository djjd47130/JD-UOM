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
      TUOMUtils.RegisterUOM('Speed', NameSingular, NamePlural, Suffix, TU.Systems.DelimitedText,
        //TODO: Clearly this is wrong...
        'Value / (DistFactor / TimeFactor)',
        'Value * (DistFactor / TimeFactor)'
      );
    end;
  end;

  TUOMUtils.RegisterBaseUOM('Speed', TUOMUtils.GetUOMByName('Kilometer per Hour'));

{$ELSE}

  //Metric

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Second', 'Millimeters per Second', 'mm/s', 'Metric',
    M(FACTOR_MM_PER_SECOND), D(FACTOR_MM_PER_SECOND)  ); //TODO: Reverse...

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Second', 'Centimeters per Second', 'cm/s', 'Metric',
    M(FACTOR_CM_PER_SECOND), D(FACTOR_CM_PER_SECOND) ); //TODO: Reverse...

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Meter per Second', 'Meters per Second', 'm/s', 'Metric', FACTOR_M_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Kilometer per Second', 'Kilometers per Second', 'km/s', 'Metric (Huge)', FACTOR_KM_PER_SECOND);

  //TODO: Add Per Minute for each...

  TUOMUtils.RegisterUOM('Speed',
    'Millimeter per Hour', 'Millimeters per Hour', 'mm/h', 'Metric',
    M(FACTOR_MM_PER_HOUR), D(FACTOR_MM_PER_HOUR) ); //TODO: Reverse...

  TUOMUtils.RegisterUOM('Speed',
    'Centimeter per Hour', 'Centimeters per Hour', 'cm/h', 'Metric',
    M(FACTOR_CM_PER_HOUR), D(FACTOR_CM_PER_HOUR) ); //TODO: Reverse...

  TUOMUtils.RegisterUOM('Speed',
    'Meter per Hour', 'Meters per Hour', 'm/h', 'Metric',
    M(FACTOR_M_PER_HOUR), D(FACTOR_M_PER_HOUR) ); //TODO: Reverse...

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Kilometer per Hour', 'Kilometers per Hour', 'km/h', 'Metric', FACTOR_KM_PER_HOUR).SetAsBase;

  //Imperial / US Customary

  TUOMUtils.RegisterUOM('Speed',
    'Foot per Second', 'Feet per Second', 'ft/s', 'Imperial,US Customary',
    M(FACTOR_FOOT_PER_SECOND), D(FACTOR_FOOT_PER_SECOND)  ); //TODO: Reverse...

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Mile per Hour', 'Miles per Hour', 'mph', 'Imperial,US Customary', FACTOR_MILE_PER_HOUR);

{$ENDIF}

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Knot', 'Knots', 'kt', 'Imperial,US Customary', FACTOR_KNOT);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Mach', 'Mach', 'M', 'Natural', FACTOR_MACH);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Lightspeed', 'Lightspeed', 'c', 'Natural', FACTOR_LIGHTSPEED);

end;

initialization
  RegisterUOM;
end.
