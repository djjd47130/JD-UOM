unit JD.Uom.Mass;

interface

uses
  System.Classes, System.SysUtils, System.Types, System.Generics.Collections,
  JD.Uom;

type
  //TODO: Identify all required mass units
  //NOTE: DO NOT confuse mass with weight - weight is relative to gravity.
  TUOMMassUnit = (ummMilliGrams, ummGrams, ummKiloGrams);
  TUOMMassUnits = set of TUOMMassUnit;

  TUOMMassUnitBase = class;
  TUOMMassUtils = class;

  TUOMMassUnitBaseClass = class of TUOMMassUnitBase;

  TUOMMassUnitBase = class(TUOMUnitBase)
    class function UOM: TUOMBaseClass; override;
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function UnitDescription: String; override;
    class function Systems: TUOMSystems; override;
    class function Prefix: String; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMMassUnit; virtual; abstract;
  end;

  //Specific Mass Units

  TUOMMassMilliGrams = class(TUOMMassUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMMassUnit; override;
  end;

  TUOMMassGrams = class(TUOMMassUnitBase)
    class function UnitID: String; override;
    class function NameSingular: String; override;
    class function Systems: TUOMSystems; override;
    class function Suffix: String; override;
    class function ConvertToBase(const AValue: Double): Double; override;
    class function ConvertFromBase(const AValue: Double): Double; override;
    class function UnitEnum: TUOMMassUnit; override;
  end;




  TUOMMassUtils = class(TUOMBase)
  private
    class var FUnits: TList<TUOMUnitClass>;
    class procedure RegisterUOM;
    class procedure RegisterUnits;
    class procedure RegisterUnit(AUnitClass: TUOMUnitClass);
  public
    class constructor Create;
    class destructor Destroy;
    class function UOMID: String; override;
    class function UOMName: String; override;
    class function UnitCount: Integer; override;
    class function GetUnit(const Index: Integer): TUOMUnitClass; override;
    class function BaseUnit: TUOMUnitClass; override;

    class function UnitByEnum(const AUnit: TUOMMassUnit): TUOMMassUnitBaseClass;
    class function Convert(const AValue: Double; const AFromUnit,
      AToUnit: TUOMMassUnit): Double;

  end;

  TUOMMass = record
  private
    FUnit: TUOMMassUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMMassUnit);
    procedure SetValue(const Value: Double);

  end;

implementation

var
  _: TUOMMassUtils;

{ TUOMMassUtils }

class constructor TUOMMassUtils.Create;
begin
  FUnits:= TList<TUOMUnitClass>.Create;
  RegisterUOM;
  RegisterUnits;
end;

class destructor TUOMMassUtils.Destroy;
begin
  FreeAndNil(FUnits);
end;

class function TUOMMassUtils.BaseUnit: TUOMUnitClass;
begin
  Result:= TUOMMassGrams;
end;

class function TUOMMassUtils.Convert(const AValue: Double; const AFromUnit,
  AToUnit: TUOMMassUnit): Double;
var
  F, T: TUOMMassUnitBaseClass;
begin
  F:= UnitByEnum(AFromUnit);
  T:= UnitByEnum(AToUnit);
  Result:= F.ConvertToBase(AValue);
  Result:= T.ConvertFromBase(Result);
end;

class function TUOMMassUtils.GetUnit(const Index: Integer): TUOMUnitClass;
begin
  Result:= FUnits[Index];
end;

class procedure TUOMMassUtils.RegisterUnit(AUnitClass: TUOMUnitClass);
begin
  FUnits.Add(AUnitClass);
end;

class procedure TUOMMassUtils.RegisterUnits;
begin
  RegisterUnit(TUOMMassMilliGrams);
  RegisterUnit(TUOMMassGrams);
  //TODO
end;

class procedure TUOMMassUtils.RegisterUOM;
begin
  TUOMUtils.RegisterUOM(TUOMMassUtils);
end;

class function TUOMMassUtils.UnitByEnum(
  const AUnit: TUOMMassUnit): TUOMMassUnitBaseClass;
var
  X: Integer;
  U: TUOMMassUnitBaseClass;
begin
  Result:= nil;
  for X := 0 to FUnits.Count-1 do begin
    U:= TUOMMassUnitBaseClass(FUnits[X]);
    if U.UnitEnum = AUnit then begin
      Result:= U;
      Break;
    end;
  end;
end;

class function TUOMMassUtils.UnitCount: Integer;
begin
  Result:= FUnits.Count;
end;

class function TUOMMassUtils.UOMID: String;
begin
  Result:= '{500A5163-FDBF-4161-8654-7DE773801509}';
end;

class function TUOMMassUtils.UOMName: String;
begin
  Result:= 'Mass';
end;

{ TUOMMass }

procedure TUOMMass.SetUnit(const Value: TUOMMassUnit);
begin

end;

procedure TUOMMass.SetValue(const Value: Double);
begin

end;

{ TUOMMassUnitBase }

class function TUOMMassUnitBase.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMMassUnitBase.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMMassUnitBase.NameSingular: String;
begin
  Result:= '';
end;

class function TUOMMassUnitBase.Prefix: String;
begin
  Result:= '';
end;

class function TUOMMassUnitBase.Suffix: String;
begin
  Result:= '';
end;

class function TUOMMassUnitBase.Systems: TUOMSystems;
begin
  Result:= [];
end;

class function TUOMMassUnitBase.UnitDescription: String;
begin
  Result:= '';
end;

class function TUOMMassUnitBase.UnitID: String;
begin
  Result:= '';
end;

class function TUOMMassUnitBase.UOM: TUOMBaseClass;
begin
  Result:= TUOMMassUtils;
end;

{ TUOMMassMilliGrams }

class function TUOMMassMilliGrams.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue * 1000;
end;

class function TUOMMassMilliGrams.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue / 1000;
end;

class function TUOMMassMilliGrams.NameSingular: String;
begin
  Result:= 'Milligrams';
end;

class function TUOMMassMilliGrams.Suffix: String;
begin
  Result:= 'mg';
end;

class function TUOMMassMilliGrams.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMMassMilliGrams.UnitEnum: TUOMMassUnit;
begin
  Result:= ummMilliGrams;
end;

class function TUOMMassMilliGrams.UnitID: String;
begin
  Result:= '{BDB1443D-1D32-4615-8048-9318B2EDBF2A}';
end;

{ TUOMMassGrams }

class function TUOMMassGrams.ConvertFromBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMMassGrams.ConvertToBase(const AValue: Double): Double;
begin
  Result:= AValue;
end;

class function TUOMMassGrams.NameSingular: String;
begin
  Result:= 'Gram';
end;

class function TUOMMassGrams.Suffix: String;
begin
  Result:= 'g';
end;

class function TUOMMassGrams.Systems: TUOMSystems;
begin
  Result:= [ustMetric];
end;

class function TUOMMassGrams.UnitEnum: TUOMMassUnit;
begin
  Result:= ummGrams;
end;

class function TUOMMassGrams.UnitID: String;
begin
  Result:= '{322B3585-2B37-4E76-AFB8-2406F0561C71}';
end;

initialization
  _:= nil;
end.
