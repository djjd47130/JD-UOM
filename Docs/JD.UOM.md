# JD.UomUtils.pas

Central unit to access all possible unit of measure conversions.

### NOTE: This documentation is out of date, and will be updated soon.

## Constants

- `PartOfNumber = ['0'..'9', '.', ','];`
- `NumFormat = '#,###,###,###,##0.#############';`

## Types

- `TUOMSystem = (ustAny, ustMetric, ustUSCustomary, ustImperial);`
- `TUOMSystems = set of TUOMSystem;`
- `TUOM = (umLength, umArea, umVolume, umWeight, umTemperature,
    umEnergy, umSpeed, umTime, umPower, umData, umPressure, umAngle,
    umResistance, umCapacitance, umVoltage, umCurrent, umDensity, umGravity,
    umRadiation);`
-  `TUOMs = set of TUOM;`

## `TUOMUtils`

- `LisUOMSystems(AList: TStrings); static;`
- `ListUOMs(AList: TStrings); static;`
- `ListUOMUnits(const AUOM: TUOM; AList: TStrings; const ASystem: TUOMSystem = TUOMSystem.ustAny);`
- `UOMName(const AUOM: TUOM): String;`
- `ParseSuffix(AValue: String; var ANumber: Double; var ASuffix: String);`

## `TUnitOfMeasurement`


