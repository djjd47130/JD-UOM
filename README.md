# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and is not ready for use at this time.

- Unit [**JD.UomUtils.pas**](https://github.com/djjd47130/JD-UOM/blob/main/Docs/JD.UOMUtils.md) - Central access to all possible units of measurement and conversions.
  - **TUOMSystem** - Enum to identify different systems of unit measurements.
  - **TUOMSystems** - Set of **TUOMSystem** enums.
- Unit [**JD.UomCtrls.pas**](https://github.com/djjd47130/JD-UOM/blob/main/Docs/JD.UOMCtrls.md) - VCL controls related to UOM conversion.
  - **TUOMEdit** - Custom control to control formatted text values for units of measurement.
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

### Application

An application is provided to not just test this collection of code, but to serve as a single central application to perform any and all possible conversions between different units of measurement.

Application requires:
- Raize Controls
