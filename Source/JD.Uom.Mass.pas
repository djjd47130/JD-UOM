unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
var
  Base: TUOM;
begin
  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Milligram', 'Milligrams', '', 'mg', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Milligrams
      Result:= Value * 1000;
    end,
    function(const Value: Double): Double
    begin
      //Milligrams to Grams
      Result:= Value / 1000;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Gram', 'Grams', '', 'g', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value * 1;
    end,
    function(const Value: Double): Double
    begin
      //Grams to Grams
      Result:= Value / 1;
    end
  ));

  TUOMUtils.RegisterUOM(TUOM.Create('Mass',
    'Kilogram', 'Kilograms', '', 'kg', 'Metric',
    function(const Value: Double): Double
    begin
      //Grams to Kilograms
      Result:= Value / 1000;
    end,
    function(const Value: Double): Double
    begin
      //Kilograms to Grams
      Result:= Value * 1000;
    end
  ));

  Base:= TUOMUtils.GetUOMByName('Gram');
  TUOMUtils.RegisterBaseUOM(Base.Category, Base);

end;

initialization
  RegisterUOM;
end.
