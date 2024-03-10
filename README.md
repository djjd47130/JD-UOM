# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and is not ready for use at this time. 

### Help Wanted
Visit [Open Issues](https://github.com/djjd47130/JD-UOM/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22) to see what I need help with.

### NOTE: This library requires DWScript, which is required for string-based expression conversion formulas.

### NOTE: Do not get confused with the term `unit` - it is used to reference a unit-of-measurement, and does not mean a "delphi unit". Except for in documentation, such as below.

## Features
- Main central class `TUOMUtils` to encapsulate entire UOM conversion capabilities.
- Base object `TUOM` to encapsulate all possible details of a specific unit of measurement.
  - Registered within `TUOMUtils` class.
- Base UOM registered within `TUOMUtils` for each given Category.
  - For example, the base of "Distance" is "Meters, and the base of "Time" is "Days".
  - All other UOMs in the same Category calculate to and from this base.
- Endless common units-of-measurement pre-registered, including `Distance`, `Area`, `Volume`, `Mass`, `Temperature`, etc.
- Ability to manually add your own units of measurement.
- String-based expressions to allow flexible and dynamic UOM registration in run-time.
  - For example, "Temperature" is one Category which requires more than just a single factor to divide / multiply.
  - Also allows for UI display of formulas to the user.
- Statistic capabilities for any given unit of measurement.
  - For example, list all UOM Categories, Get UOM Object by Name, Conversion Examples.
- Add a given Category unit to your `uses` clause to add support to your own project.
  - For example, `uses JD.Uom.Distance, JD.Uom.Area, JD.Uom.Time` - and all UOMs of those categories will be automatically registered.
- Application `JDConvert.exe` to demonstrate all possible UOM capabilities.

## Application
There is an application project aimed at both demonstration the capabilities of the library and actually converting
data in the most flexible manner possible.

![image](https://github.com/djjd47130/JD-UOM/assets/8213266/16918eae-241b-4f6d-a0b9-e42c91b69641)

## Units

- [**JD.Uom.pas**](Docs/JD.Uom.md) - Central access to all possible units of measurement and conversions.
  - **TUOMUtils** - Main class to encapsulate entire conversion library and UOM details.
  - **TUOM** - Base object referencing a specific unit of measurement and all its details.
  - **TUOMValue** **(NOT READY)** - Implicit record type to contain a single value of any given UOM.
- [**JD.Uom.Distance.pas**](/Docs/JD.Uom.Distance.md) - Registration of Distance related UOMs.
- [**JD.Uom.Area.pas**](/Docs/JD.Uom.Area.md) - Registration of all Area related UOMs.
  - **TUOMAreaRect** - **(NOT READY)** - Implicit record type to contain 2 linear dimensions.
- [**JD.Uom.Volume.pas**](/Docs/JD.Uom.Volume.md) - Registration of all Volume related UOMs.
  - **TUOMVolumeCube** - **(NOT READY)** - Implicit record type to contain 3 linear dimensions.
- [**JD.Uom.Temperature.pas**](/Docs/JD.Uom.Temperature.md) - Registration of all Temperature related UOMs.
- [**JD.Uom.Mass.pas**](/Docs/JD.Uom.Mass.md) - Registration of all Mass related UOMs.
- [**JD.Uom.Time.pas**](/Docs/JD.Uom.Time.md) - Registration of all Time related UOMs.

**And much more to come...**

## Usage

Currently, this library is in active development. Eventually, `TUOMValue` will be the grand class of everything, encapsulating any possible value of any given UOM. However, it's currently still a work-in-progress to implement an infrastructure.

That being said, everything currently starts with `TUOMUtils` in [JD.Uom.pas](/Docs/JD.Uom.md). Specific units are registered via `TUOMUtils.RegisterUOM`. This registration is deliberately done from **outside** this main unit, as to be as abstract as possible. You can access information about all registered UOMs through `TUOMUtils`. For example, `GetUOMByName` or `ListCategories`. 

