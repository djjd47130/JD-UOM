unit JD.Uom.Weight;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMWeightUnit = (umwCarats, umwMilliGrams, umwCentiGrams, umwDeciGrams, umwGrams,
    umwDekaGrams, umwHectoGrams, umwKiloGrams, umwMetricTonnes, umwOunces,
    umwPounds, umwStone, umwShortTons, umwLongTons);
  TUOMWeightUnits = set of TUOMWeightUnit;

  TUOMWeightUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMWeightUnit): String; static;
    class function UnitName(const AValue: TUOMWeightUnit): String; static;

    { Metric System }

    //TODO: Weight is technically relative to gravity - MASS is the main factor...

    { US Customary System }



  end;

  TUOMWeight = record
  private
    FUnit: TUOMWeightUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMWeightUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMWeightUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMWeight;
    class operator implicit(const AValue: TUOMWeight): Double;
    class operator implicit(const AValue: String): TUOMWeight;
    class operator implicit(const AValue: TUOMWeight): String;
    function ToCarats: Double;
    function ToMilligrams: Double;
    function ToCentigrams: Double;
    function ToDecigrams: Double;
    function ToGrams: Double;
    function ToDekagrams: Double;
    function ToHectograms: Double;
    function ToKilograms: Double;
    function ToMetricTonnes: Double;
    function ToOunces: Double;
    function ToPounds: Double;
    function ToStones: Double;
    function ToShortTons: Double;
    function ToLongTons: Double;
  end;

implementation

var
  DefaultWeightUnit: TUOMWeightUnit;

{ TUOMWeightUtils }

class function TUOMWeightUtils.UnitName(const AValue: TUOMWeightUnit): String;
begin
  case AValue of
    umwCarats:        Result:= 'Carats';
    umwMilliGrams:    Result:= 'Milligrams';
    umwCentiGrams:    Result:= 'Centigrams';
    umwDeciGrams:     Result:= 'Decigrams';
    umwGrams:         Result:= 'Grams';
    umwDekaGrams:     Result:= 'Dekagrams';
    umwHectoGrams:    Result:= 'Hectograms';
    umwKiloGrams:     Result:= 'Kilograms';
    umwMetricTonnes:  Result:= 'Metric Tonnes';
    umwOunces:        Result:= 'Ounces';
    umwPounds:        Result:= 'Pounds';
    umwStone:         Result:= 'Stone';
    umwShortTons:     Result:= 'Short Tons';
    umwLongTons:      Result:= 'Long Tons';
  end;
end;

class function TUOMWeightUtils.UnitSuffix(const AValue:  TUOMWeightUnit): String;
begin
  case AValue of
    umwCarats:        Result:= 'ct';
    umwMilliGrams:    Result:= 'mg';
    umwCentiGrams:    Result:= 'cg';
    umwDeciGrams:     Result:= 'dg';
    umwGrams:         Result:= 'g';
    umwDekaGrams:     Result:= 'dag';
    umwHectoGrams:    Result:= 'hg';
    umwKiloGrams:     Result:= 'kg';
    umwMetricTonnes:  Result:= 't';
    umwOunces:        Result:= 'oz';
    umwPounds:        Result:= 'lbs';
    umwStone:         Result:= 'st';
    umwShortTons:     Result:= 'st';
    umwLongTons:      Result:= 'lt';
  end;
end;

class procedure TUOMWeightUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Carats');
  AList.Append('Milligrams');
  AList.Append('Centigrams');
  AList.Append('Decigrams');
  AList.Append('Grams');
  AList.Append('Dekagrams');
  AList.Append('Hectograms');
  AList.Append('Kilograms');
  AList.Append('Metric Tonnes');
  AList.Append('Ounces');
  AList.Append('Pounds');
  AList.Append('Stone');
  AList.Append('Short Tons');
  AList.Append('Long Tons');
end;

{ TUOMWeight }

class operator TUOMWeight.implicit(const AValue: TUOMWeight): Double;
begin
  case DefaultWeightUnit of
    umwCarats:        Result:= AValue.ToCarats;
    umwMilliGrams:    Result:= AValue.ToMilligrams;
    umwCentiGrams:    Result:= AValue.ToCentigrams;
    umwDeciGrams:     Result:= AValue.ToDecigrams;
    umwGrams:         Result:= AValue.ToGrams;
    umwDekaGrams:     Result:= AValue.ToDekagrams;
    umwHectoGrams:    Result:= AValue.ToHectograms;
    umwKiloGrams:     Result:= AValue.ToKilograms;
    umwMetricTonnes:  Result:= AValue.ToMetricTonnes;
    umwOunces:        Result:= AValue.ToOunces;
    umwPounds:        Result:= AValue.ToPounds;
    umwStone:         Result:= AValue.ToStones;
    umwShortTons:     Result:= AValue.ToShortTons;
    umwLongTons:      Result:= AValue.ToLongTons;
  end;
end;

class operator TUOMWeight.implicit(const AValue: Double): TUOMWeight;
begin
  Result.FUnit:= DefaultWeightUnit;
  Result.FValue:= AValue;
end;

class operator TUOMWeight.implicit(const AValue: String): TUOMWeight;
begin
  //TODO: Parse string...

end;

class operator TUOMWeight.implicit(const AValue: TUOMWeight): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  Result:= Result + TUOMWeightUtils.UnitSuffix(AValue.&Unit);
end;

procedure TUOMWeight.SetUnit(const Value: TUOMWeightUnit);
begin
  case DefaultWeightUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
  FUnit:= Value;
end;

procedure TUOMWeight.SetValue(const Value: Double);
begin
  FValue:= Value;
end;

function TUOMWeight.ToCarats: Double;
begin
  case FUnit of
    umwCarats:        Result:= FValue; // Same
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToCentigrams: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    Result:= FValue; // Same
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToDecigrams: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     Result:= FValue; // Same
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToDekagrams: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     Result:= FValue; // Same
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToGrams: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         Result:= FValue; // Same
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToHectograms: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    Result:= FValue; // Same
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToKilograms: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     Result:= FValue; // Same
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToLongTons: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      Result:= FValue; // Same
  end;
end;

function TUOMWeight.ToMetricTonnes: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  Result:= FValue; // Same
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToMilligrams: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    Result:= FValue; // Same
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToOunces: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        Result:= FValue; // Same
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToPounds: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        Result:= FValue; // Same
    umwStone:         ;
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToShortTons: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         ;
    umwShortTons:     Result:= FValue; // Same
    umwLongTons:      ;
  end;
end;

function TUOMWeight.ToStones: Double;
begin
  case FUnit of
    umwCarats:        ;
    umwMilliGrams:    ;
    umwCentiGrams:    ;
    umwDeciGrams:     ;
    umwGrams:         ;
    umwDekaGrams:     ;
    umwHectoGrams:    ;
    umwKiloGrams:     ;
    umwMetricTonnes:  ;
    umwOunces:        ;
    umwPounds:        ;
    umwStone:         Result:= FValue; // Same
    umwShortTons:     ;
    umwLongTons:      ;
  end;
end;


initialization
  DefaultWeightUnit:= TUOMWeightUnit.umwPounds;
end.
