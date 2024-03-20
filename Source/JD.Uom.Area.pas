unit JD.Uom.Area;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance;

const
  FACTOR_ACRE = 4046.86;

type

  /// <summary>
  /// (NOT READY)
  /// A record that allows area to be specified using two linear dimensions
  /// </summary>
  TUOMAreaRect = record
  private
    FWidth: TUOMValue;
    FLength: TUOMValue;
    procedure SetLength(const Value: TUOMValue);
    procedure SetWidth(const Value: TUOMValue);
    function GetArea: UOMNum;
  public
    property Width: TUOMValue read FWidth write SetWidth;
    property Length: TUOMValue read FLength write SetLength;
    property Area: UOMNum read GetArea;
    class operator implicit(const AValue: UOMNum): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): UOMNum;
    class operator implicit(const AValue: String): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): String;
    class operator implicit(const AValue: TUOMValue): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): TUOMValue;
  end;

implementation

procedure RegisterUOM;
begin

  //Metric

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Millimeter', 'Square Millimeters', 'mm²', 'Metric', Sqr(METRIC_MILLI));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Centimeter', 'Square Centimeters', 'cm²', 'Metric', Sqr(METRIC_CENTI));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Meter', 'Square Meters', 'm²', 'Metric', Sqr(METRIC_BASE)).SetAsBase;

  TUOMUtils.RegisterSimpleUOM('Area',
    'Hectare', 'Hectares', 'ha', 'Metric', Sqr(METRIC_HECTO));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Kilometer', 'Square Kilometers', 'km²', 'Metric', Sqr(METRIC_KILO));

  //Imperial / US Customary

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Inch', 'Square Inches', '"²', 'Imperial,US Customary', Sqr(FACTOR_INCH));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Foot', 'Square Feet', '''²', 'Imperial,US Customary', Sqr(FACTOR_FOOT));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Yard', 'Square Yards', 'yd²', 'Imperial,US Customary', Sqr(FACTOR_YARD));

  TUOMUtils.RegisterSimpleUOM('Area',
    'Acre', 'Acres', 'ac', 'Imperial,US Customary', FACTOR_ACRE);

  TUOMUtils.RegisterSimpleUOM('Area',
    'Square Mile', 'Square Miles', 'mi²', 'Imperial,US Customary', Sqr(FACTOR_MILE));

end;

{ TUOMAreaRect }

procedure TUOMAreaRect.SetLength(const Value: TUOMValue);
begin
  FLength := Value;
end;

procedure TUOMAreaRect.SetWidth(const Value: TUOMValue);
begin
  FWidth := Value;
end;

function TUOMAreaRect.GetArea: UOMNum;
begin
  //TODO: Convert combined UOMs...
  Result:= FWidth * FLength;

end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): UOMNum;
begin
  Result:= 0; //TODO
end;

class operator TUOMAreaRect.implicit(
  const AValue: UOMNum): TUOMAreaRect;
begin
  //TODO: Kinda hard to convert from area to width/length dimensions...
  //  Perhaps just get the square root?
end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): String;
begin
  Result:= AValue.Width + ' x ' + AValue.Length;
end;

class operator TUOMAreaRect.implicit(
  const AValue: String): TUOMAreaRect;
begin
  //TODO: Parse string...

end;

class operator TUOMAreaRect.implicit(const AValue: TUOMAreaRect): TUOMValue;
begin
  //TODO: Multiply width + height and return new value...

end;

class operator TUOMAreaRect.implicit(const AValue: TUOMValue): TUOMAreaRect;
begin
  //TODO: Get square root of value and return new rect value...

end;

initialization
  RegisterUOM;
end.
