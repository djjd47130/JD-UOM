unit JD.Uom.Area;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance;

type

  //TODO: A record that allows area to be specified using two linear dimensions
  TUOMAreaRect = record
  private
    FWidth: Double;
    FLength: Double;
    procedure SetLength(const Value: Double);
    procedure SetWidth(const Value: Double);
    function GetArea: Double;
  public
    property Width: Double read FWidth write SetWidth;
    property Length: Double read FLength write SetLength;
    property Area: Double read GetArea;
    class operator implicit(const AValue: Double): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): Double;
    class operator implicit(const AValue: String): TUOMAreaRect;
    class operator implicit(const AValue: TUOMAreaRect): String;
  end;

implementation

procedure RegisterUOM;
var
  Base: TUOM;
begin

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Millimeter', 'Square Millimeters', '', 'mm²', 'Metric',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Millimeters
      Result:= Value * 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Square Millimeters to Square Meters
      Result:= Value / 1000000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Centimeter', 'Square Centimeters', '', 'cm²', 'Metric',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Centimeters
      Result:= Value * 10000;
    end,
    function(const Value: Double): Double
    begin
      //Square Centimeters to Square Meters
      Result:= Value / 10000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Meter', 'Square Meters', '', 'm²', 'Metric',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Meters
      Result:= Value * 1;
    end,
    function(const Value: Double): Double
    begin
      //Square Meters to Square Meters
      Result:= Value / 1;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Hectare', 'Hectares', '', 'ha', 'Metric',
    function(const Value: Double): Double
    begin
      //Square Meters to Hectares
      Result:= Value / 10000;
    end,
    function(const Value: Double): Double
    begin
      //Hectares to Square Meters
      Result:= Value * 10000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Kilometer', 'Square Kilometers', '', 'km²', 'Metric',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Kilometers
      Result:= Value / 1000000;
    end,
    function(const Value: Double): Double
    begin
      //Square Kilometers to Square Meters
      Result:= Value * 1000000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Inch', 'Square Inches', '', '"²', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Inches
      Result:= Value * 1550;
    end,
    function(const Value: Double): Double
    begin
      //Square Inches to Square Meters
      Result:= Value / 1550;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Foot', 'Square Feet', '', '''²', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Feet
      Result:= Value * 10.7639;
    end,
    function(const Value: Double): Double
    begin
      //Square Feet to Square Meters
      Result:= Value / 10.7639;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Yard', 'Square Yards', '', 'yd²', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Yards
      Result:= Value * 1.19599;
    end,
    function(const Value: Double): Double
    begin
      //Square Yards to Square Meters
      Result:= Value / 1.19599;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Acre', 'Acres', '', 'ac', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Square Meters to Acres
      Result:= Value / 4046.86;
    end,
    function(const Value: Double): Double
    begin
      //Acres to Square Meters
      Result:= Value * 4046.86;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Area',
    'Square Mile', 'Square Miles', '', 'mi²', 'Imperial,US Customary',
    function(const Value: Double): Double
    begin
      //Square Meters to Square Miles
      Result:= Value / 2589988.110336;
    end,
    function(const Value: Double): Double
    begin
      //Square Miles to Square Meters
      Result:= Value * 2589988.110336;
    end
  ));

  Base:= TUOMUtils.GetUOMByName('Square Meter');
  TUOMUtils.RegisterBaseUOM(Base.Category, Base);

end;

{ TUOMAreaRect }

procedure TUOMAreaRect.SetLength(const Value: Double);
begin
  FLength := Value;
end;

procedure TUOMAreaRect.SetWidth(const Value: Double);
begin
  FWidth := Value;
end;

function TUOMAreaRect.GetArea: Double;
begin
  //TODO: Convert combined UOMs...
  Result:= FWidth * FLength;

end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): Double;
begin
  Result:= 0; //TODO
end;

class operator TUOMAreaRect.implicit(
  const AValue: Double): TUOMAreaRect;
begin
  //TODO: Kinda hard to convert from area to width/length dimensions...
  //  Perhaps just get the square root?
end;

class operator TUOMAreaRect.implicit(
  const AValue: TUOMAreaRect): String;
begin
  //Result:= AValue.Width + ' x ' + AValue.Length;
end;

class operator TUOMAreaRect.implicit(
  const AValue: String): TUOMAreaRect;
begin
  //TODO: Parse string...

end;

initialization
  RegisterUOM;
end.
