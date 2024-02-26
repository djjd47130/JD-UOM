# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and is not ready for use at this time. 

### NOTE: Do not get confused with the term `unit` - it is used to reference a unit-of-measurement, and does not mean a "delphi unit".

### Features
- `TUOMList` - Class encapsulating all possible unit conversions.

### Units and Types

- Unit [**JD.Uom.pas**](https://github.com/djjd47130/JD-UOM/blob/main/Docs/JD.UOMUtils.md) - Central access to all possible units of measurement and conversions.
  - **TUOMSystem** - Enum to identify different systems of unit measurements.
  - **TUOMSystems** - Set of **TUOMSystem** enums.
  - **TUOMList** - Main class to access all possible unit conversions and information.
- Unit [**JD.Uom.Length.pas**](https://github.com/djjd47130/JD-UOM/blob/main/Docs/JD.UOM.Length.md) - Conversion between lengths.
  - **TUOMLengthUnit** - Enum to identify different length units.
  - **TUOMLengthUnits** - Set of **TUOMLengthUnit** enums.
  - **TUOMLengthUtils** - Class object to access length conversion methods.
  - **TUOMLength** - Record to encapsulate any given length value.
- Unit [**JD.Uom.Area.pas**](https://github.com/djjd47130/JD-UOM/blob/main/Docs/JD.UOM.Area.md) - Conversion between areas.
  - **TUOMAreaUnit** - Enum to identify different area units.
  - **TUOMAreaUnits** - Set of **TUOMAreaUnit** enums.
  - **TUOMAreaUtils** - Class object to access area conversion methods.
  - **TUOMArea** - Record to encapsulate any given area value.
  - **TUOMTwoDimensions** - Record to encapsulate area value based on two length values.
- Unit [**JD.Uom.Volume.pas**](JD.Uom.Volume.pas) - Conversion between volumes.
- Unit [**JD.Uom.Temperature.pas**](JD.Uom.Temperature.pas) - Conversion between temperatures.
  - **TUOMTemperatureUnit** - Enum to identify different temperature units.
  - **TUOMTemperatureUnits** - Set of **TUOMTemperatureUnit** enums.
  - **TUOMTemperatureUtils** - Class object to access temperature conversion methods.
  - **TUOMTemperature** - Record to encapsulate any given temperature value.

**And many more to come...**

