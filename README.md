# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and is not ready for use at this time. 

### Help Wanted
Visit [Open Issues](https://github.com/djjd47130/JD-UOM/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22) to see what I need help with.

### NOTE: Do not get confused with the term `unit` - it is used to reference a unit-of-measurement, and does not mean a "delphi unit". Except for in documentation, such as below.

## Features
- Generic central class `TUOMUtils` to encapsulate entire UOM conversion capabilities.
- Base object `TUOM` to encapsulate all possible details of a specific unit of measurement.
  - Registered within `TUOMUtils` class.
- Endless common units-of-measurement pre-registered, including `Distance`, `Area`, `Volume`, `Mass`, `Temperature`, etc.
- Ability to manually add your own units of measurement.
- Statistic capabilities for any given unit of measurement.
- Application `JDConvert.exe` to demonstrate all possible UOM capabilities.

## Units

- [**JD.Uom.pas**](Docs/JD.Uom.md) - Central access to all possible units of measurement and conversions.
  - **TUOMUtils** - Main class to encapsulate entire conversion library and UOM details.
  - **TUOM** - Base object referencing a specific unit of measurement and all its details.
  - **TUOMValue** **(NOT READY)** - Implicit record type to contain a single value of any given UOM.
- [**JD.Uom.Distance.pas**](/Docs/JD.Uom.Distance.md) - Registration of Distance related UOMs.
- [**JD.Uom.Area.pas**](/Docs/JD.Uom.Area.md) - Registration of all Area related UOMs.
- [**JD.Uom.Volume.pas**](/Docs/JD.Uom.Volume.md) - Registration of all Volume related UOMs.
- [**JD.Uom.Temperature.pas**](/Docs/JD.Uom.Temperature.md) - Registration of all Temperature related UOMs.
- [**JD.Uom.Mass.pas**](/Docs/JD.Uom.Mass.md) - Registration of all Mass related UOMs.

**And much more to come...**
