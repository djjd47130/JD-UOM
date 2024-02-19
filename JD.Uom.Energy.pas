unit JD.Uom.Energy;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMEnergyUnit = (umeElectronVolts, umeJoules, umeKiloJoules, umeThermalCalories,
    umeFoodCalories, umeFootPounds, umeBritishThermalUnits);
  TUOMEnergyUnits = set of TUOMEnergyUnit;

  TUOMEnergyUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMEnergyUnit): String; static;
    class function UnitName(const AValue: TUOMEnergyUnit): String; static;



  end;

  TUOMEnergy = record

  end;

implementation

var
  DefaultEnergyUnit: TUOMEnergyUnit;

{ TUOMEnergyUtils }

class procedure TUOMEnergyUtils.UnitList(AList: TStrings);
begin
  AList.Clear;
  AList.Append('Electron Volts');
  AList.Append('Joules');
  AList.Append('Kilojoules');
  AList.Append('Thermal Calories');
  AList.Append('Food Calories');
  AList.Append('Foot Pounds');
  AList.Append('BTUs');
end;

class function TUOMEnergyUtils.UnitName(
  const AValue: TUOMEnergyUnit): String;
begin
  case AValue of
    umeElectronVolts:       Result:= 'Electron Volts';
    umeJoules:              Result:= 'Joules';
    umeKiloJoules:          Result:= 'Kilojoules';
    umeThermalCalories:     Result:= 'Thermal Calories';
    umeFoodCalories:        Result:= 'Food Calories';
    umeFootPounds:          Result:= 'Foot Pounds';
    umeBritishThermalUnits: Result:= 'BTUs';
  end;
end;

class function TUOMEnergyUtils.UnitSuffix(
  const AValue: TUOMEnergyUnit): String;
begin
  case AValue of
    umeElectronVolts:       Result:= '';
    umeJoules:              Result:= '';
    umeKiloJoules:          Result:= '';
    umeThermalCalories:     Result:= '';
    umeFoodCalories:        Result:= '';
    umeFootPounds:          Result:= '';
    umeBritishThermalUnits: Result:= 'BTUs';
  end;
end;

initialization
  DefaultEnergyUnit:= TUOMEnergyUnit.umeJoules;
end.
