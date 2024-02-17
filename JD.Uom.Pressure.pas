unit JD.Uom.Pressure;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMPressureUnit = (umrAtmospheres, umrBars, umrKiloPascals, umrMillimetersOfMercury,
    umrPascals, umrPoundsPerSquareInch);
  TUOMPressureUnits = set of TUOMPressureUnit;

  TUOMPressureUtils = class
  private
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMPressureUnit): String; static;
    class function UnitName(const AValue: TUOMPressureUnit): String; static;
  end;

  TUOMPressure = record

  end;

implementation

var
  DefaultPressureUnit: TUOMPressureUnit;

class procedure TUOMPressureUtils.UnitList(AList: TStrings);
var
  V: TUOMPressureUnit;
begin
  AList.Clear;
  for V:= Low(TUOMPressureUnit) to High(TUOMPressureUnit) do begin
    AList.Append(TUOMPressureUtils.UnitName(V));
  end;
end;

class function TUOMPressureUtils.UnitName(const AValue: TUOMPressureUnit): String;
begin
  case AValue of
    umrAtmospheres: ;
    umrBars: ;
    umrKiloPascals: ;
    umrMillimetersOfMercury: ;
    umrPascals: ;
    umrPoundsPerSquareInch: ;
  end;
end;

class function TUOMPressureUtils.UnitSuffix(const AValue: TUOMPressureUnit): String;
begin
  case AValue of
    umrAtmospheres: ;
    umrBars: ;
    umrKiloPascals: ;
    umrMillimetersOfMercury: ;
    umrPascals: ;
    umrPoundsPerSquareInch: ;
  end;
end;

initialization
  DefaultPressureUnit:= TUOMPressureUnit.umrBars;
end.
