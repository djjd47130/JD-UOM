unit JD.Uom.Area;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom,
  JD.Uom.Distance;

const
  FACTOR_ACRE = 4046.86;

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

  TUOMUtils.RegisterUOM('Area',
    'Square Millimeter', 'Square Millimeters', '', 'mm²', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(METRIC_MILLI)), M(Sqr(METRIC_MILLI))
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Centimeter', 'Square Centimeters', '', 'cm²', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(METRIC_CENTI)), M(Sqr(METRIC_CENTI))
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Meter', 'Square Meters', '', 'm²', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(METRIC_BASE)), M(Sqr(METRIC_BASE))
  {$ELSE}
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
  {$ENDIF}
  ).SetAsBase;

  TUOMUtils.RegisterUOM('Area',
    'Hectare', 'Hectares', '', 'ha', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(METRIC_HECTO)), M(Sqr(METRIC_HECTO))
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Kilometer', 'Square Kilometers', '', 'km²', 'Metric',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(METRIC_KILO)), M(Sqr(METRIC_KILO))
  {$ELSE}
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
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Inch', 'Square Inches', '', '"²', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(Sqr(FACTOR_INCH)), D(Sqr(FACTOR_INCH))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Square Meters to Square Inches
      Result:= Value * Sqr(FACTOR_INCH);
    end,
    function(const Value: Double): Double
    begin
      //Square Inches to Square Meters
      Result:= Value / Sqr(FACTOR_INCH);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Foot', 'Square Feet', '', '''²', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(Sqr(FACTOR_FOOT)), D(Sqr(FACTOR_FOOT))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Square Meters to Square Feet
      Result:= Value * Sqr(FACTOR_FOOT);
    end,
    function(const Value: Double): Double
    begin
      //Square Feet to Square Meters
      Result:= Value / Sqr(FACTOR_FOOT);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Yard', 'Square Yards', '', 'yd²', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    M(Sqr(FACTOR_YARD)), D(Sqr(FACTOR_YARD))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Square Meters to Square Yards
      Result:= Value * Sqr(FACTOR_YARD);
    end,
    function(const Value: Double): Double
    begin
      //Square Yards to Square Meters
      Result:= Value / Sqr(FACTOR_YARD);
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Acre', 'Acres', '', 'ac', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(FACTOR_ACRE), M(FACTOR_ACRE)
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Square Meters to Acres
      Result:= Value / FACTOR_ACRE;
    end,
    function(const Value: Double): Double
    begin
      //Acres to Square Meters
      Result:= Value * FACTOR_ACRE;
    end
  {$ENDIF}
  );

  TUOMUtils.RegisterUOM('Area',
    'Square Mile', 'Square Miles', '', 'mi²', 'Imperial,US Customary',
  {$IFDEF USE_MATH_EXPR}
    D(Sqr(FACTOR_MILE)), M(Sqr(FACTOR_MILE))
  {$ELSE}
    function(const Value: Double): Double
    begin
      //Square Meters to Square Miles
      Result:= Value / Sqr(FACTOR_MILE);
    end,
    function(const Value: Double): Double
    begin
      //Square Miles to Square Meters
      Result:= Value * Sqr(FACTOR_MILE);
    end
  {$ENDIF}
  );

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
