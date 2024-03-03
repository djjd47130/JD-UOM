unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

implementation

procedure RegisterUOM;
var
  Base: TUOMLookupUnit;
begin
  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Mass',
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

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Mass',
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

  TUOMLookupTable.RegisterUnit(TUOMLookupUnit.Create('Mass',
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

  Base:= TUOMLookupTable.GetUnitByName('Gram');
  TUOMLookupTable.RegisterBaseUnit(Base.UOM, Base);

end;

initialization
  RegisterUOM;
end.
