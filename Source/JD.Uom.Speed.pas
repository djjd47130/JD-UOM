unit JD.Uom.Speed;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom,
  JD.Uom.Time,
  JD.Uom.Distance;

const
  //Metric per Second
  FACTOR_MM_PER_SECOND = 0.0036;
  FACTOR_CM_PER_SECOND = 0.036;
  FACTOR_M_PER_SECOND =  3.6;
  FACTOR_KM_PER_SECOND = 3600;
  //Imperial per Second
  FACTOR_INCH_PER_SECOND = 0.09144;
  FACTOR_FOOT_PER_SECOND = 1.097;
  FACTOR_MILE_PER_SECOND = 5793.64;
  //Metric per Minute
  FACTOR_MM_PER_MINUTE = 6e-5;
  FACTOR_CM_PER_MINUTE = 0.0006;
  FACTOR_M_PER_MINUTE = 16.6667;
  FACTOR_KM_PER_MINUTE = 60.00012;
  //Imperial per Minute
  FACTOR_INCH_PER_MINUTE = 0.001524;
  FACTOR_FOOT_PER_MINUTE = 0.018288;
  FACTOR_MILE_PER_MINUTE = 96.5606;
  //Metric per Hour
  FACTOR_MM_PER_HOUR =   1e-6;
  FACTOR_CM_PER_HOUR =   1e-5;
  FACTOR_M_PER_HOUR =    0.001;
  FACTOR_KM_PER_HOUR =   1;
  //Imperial per Hour
  FACTOR_INCH_PER_HOUR = 2.54e-5;
  FACTOR_FOOT_PER_HOUR = 0.0003048;
  FACTOR_MILE_PER_HOUR = 1.609344;
  //Natural
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

  //Metric per Second

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Millimeter per Second', 'Millimeters per Second', 'mm/s', 'Metric', FACTOR_MM_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Centimeter per Second', 'Centimeters per Second', 'cm/s', 'Metric', FACTOR_CM_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Meter per Second', 'Meters per Second', 'm/s', 'Metric', FACTOR_M_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Kilometer per Second', 'Kilometers per Second', 'km/s', 'Metric (Huge)', FACTOR_KM_PER_SECOND);

  //Metric per Minute

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Millimeter per Minute', 'Millimeters per Minute', 'mm/m', 'Metric', FACTOR_MM_PER_MINUTE);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Centimeter per Minute', 'Centimeters per Minute', 'cm/m', 'Metric', FACTOR_CM_PER_MINUTE);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Meter per Minute', 'Meters per Minute', 'm/m', 'Metric', FACTOR_M_PER_MINUTE);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Kilometer per Minute', 'Kilometers per Minute', 'km/m', 'Metric (Huge)', FACTOR_KM_PER_MINUTE);

  //Metric per Hour

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Millimeter per Hour', 'Millimeters per Hour', 'mm/h', 'Metric', FACTOR_MM_PER_HOUR);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Centimeter per Hour', 'Centimeters per Hour', 'cm/h', 'Metric', FACTOR_CM_PER_HOUR);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Meter per Hour', 'Meters per Hour', 'm/h', 'Metric', FACTOR_M_PER_HOUR);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Kilometer per Hour', 'Kilometers per Hour', 'km/h', 'Metric', FACTOR_KM_PER_HOUR).SetAsBase;

  //Imperial per Second

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Inch per Second', 'Inches per Second', 'in/s', 'Imperial,US Customary', FACTOR_INCH_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Foot per Second', 'Feet per Second', 'ft/s', 'Imperial,US Customary', FACTOR_FOOT_PER_SECOND);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Mile per Second', 'Miles per Second', 'mps', 'Imperial (Huge),US Customary (Huge)', FACTOR_MILE_PER_SECOND);

  //Imperial per Minute

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Inch per Minute', 'Inches per Minute', 'in/m', 'Imperial,US Customary', FACTOR_INCH_PER_MINUTE);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Foot per Minute', 'Feet per Minute', 'ft/m', 'Imperial,US Customary', FACTOR_FOOT_PER_MINUTE);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Mile per Minute', 'Miles per Minute', 'mpm', 'Imperial (Huge),US Customary (Huge)', FACTOR_MILE_PER_MINUTE);

  //Imperial per Hour

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Inch per Hour', 'Inches per Hour', 'in/h', 'Imperial,US Customary', FACTOR_INCH_PER_HOUR);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Foot per Hour', 'Feet per Hour', 'ft/h', 'Imperial,US Customary', FACTOR_FOOT_PER_HOUR);

  TUOMUtils.RegisterSimpleUOM('Speed',
    'Mile per Hour', 'Miles per Hour', 'mph', 'Imperial,US Customary', FACTOR_MILE_PER_HOUR);

{$ENDIF}

  //Natural

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
