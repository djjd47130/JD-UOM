# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and is not ready for use at this time.

- Unit [**JD.Uom.Common.pas**](JD.Uom.Common.pas) - Common unit for any UOM conversions.
  - **TUOMSystem** - Enum to identify different systems of unit measurements.
  - **TUOMSystems** - Set of **TUOMSystem** enums.
- Unit [**JD.Uom.Length.pas**](JD.Uom.Length.pas) - Conversion between lengths.
  - **TUOMLengthUnit** - Enum to identify different length units.
  - **TUOMLengthUnits** - Set of **TUOMLengthUnit** enums.
  - **TUOMLengthUtils** - Class object to access length conversion methods.
  - **TUOMLength** - Record to encapsulate any given length value.
- Unit [**JD.Uom.Area.pas**](JD.Uom.Area.pas) - Conversion between areas.
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

An application is provided to not only test this collection of code, but to serve as a single central application to perform any and all possible conversions between different units of measurement.
