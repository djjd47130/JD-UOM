# JD-UOM - Delphi Library to Convert Unit of Measure

### NOTE: This library is in active development, and has no guarantees at this time. Use at your own risk.

### Help Wanted
Visit [Open Issues](https://github.com/djjd47130/JD-UOM/issues?q=is%3Aopen+is%3Aissue+label%3A%22help+wanted%22) to see what I need help with.

### NOTE: This library requires DWScript, which is required for string-based expression conversion formulas.

### NOTE: Do not get confused with the term `unit` - it is used to reference a unit-of-measurement, and does not mean a "delphi unit". Except for in documentation, such as below.

## Features
- Main central class [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils) to encapsulate entire UOM conversion library and its capabilities.
- Main object [`TUOM`](/Docs/JD.Uom.md#tuom) to encapsulate all possible details of a specific unit of measurement.
  - Registered within [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils) class.
- Base UOM registered within [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils) for each given Category.
  - For example, the base of "Distance" is "Meters, and the base of "Time" is "Days".
  - All other UOMs in the same Category calculate to and from this base.
- Endless common units-of-measurement pre-registered, including `Distance`, `Area`, `Volume`, `Mass`, `Temperature`, etc.
- Ability to manually add your own units of measurement.
- String-based expressions to allow flexible and dynamic UOM registration in run-time.
  - For example, "Temperature" is one Category which requires more than just a single factor to divide / multiply.
  - Also allows for UI display of formulas to the user.
- Statistic capabilities for any given unit of measurement.
  - For example, list all UOM Categories, Get UOM Object by Name, Find UOM, etc.
- Add a given Category unit to your `uses` clause to add support to your own project.
  - For example, `uses JD.Uom.Distance, JD.Uom.Area, JD.Uom.Time` - and all UOMs of those categories will be automatically registered.
- Application `JDConvert.exe` to demonstrate all possible UOM capabilities, including custom user-level UOMs.

## Units

- [**JD.Uom.pas**](Docs/JD.Uom.md) - Central access to all possible units of measurement and conversions.
  - [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils) - Main class to encapsulate entire conversion library and UOM details.
  - [`TUOM`](/Docs/JD.Uom.md#tuom) - Base object referencing a specific unit of measurement and all its details.
  - [`TUOMValue`](/Docs/JD.Uom.md#tuomvalue) **(NOT READY)** - Implicit record type to contain a single value of any given UOM.
- **JD.Uom.Expr.pas** - Encapsulates string-based expression evaluation on an abstract level.
- **JD.Uom.Files.pas** - Encapsulates the ability to save/load UOMs via INI files.
- [**JD.Uom.Distance.pas**](/Docs/JD.Uom.Distance.md) - Registration of Distance related UOMs.
- [**JD.Uom.Area.pas**](/Docs/JD.Uom.Area.md) - Registration of all Area related UOMs.
  - **TUOMAreaRect** - **(NOT READY)** - Implicit record type to contain 2 linear dimensions.
- [**JD.Uom.Volume.pas**](/Docs/JD.Uom.Volume.md) - Registration of all Volume related UOMs.
  - **TUOMVolumeCube** - **(NOT READY)** - Implicit record type to contain 3 linear dimensions.
- [**JD.Uom.Temperature.pas**](/Docs/JD.Uom.Temperature.md) - Registration of all Temperature related UOMs.
- [**JD.Uom.Mass.pas**](/Docs/JD.Uom.Mass.md) - Registration of all Mass related UOMs.
- [**JD.Uom.Frequency.pas**](/Docs/JD.Uom.Frequency.md) - Registration of all Frequency related UOMs.
- [**JD.Uom.Time.pas**](/Docs/JD.Uom.Time.md) - Registration of all Time related UOMs.

**And much more to come...**

## Usage

Currently, this library is in active development. Eventually, [`TUOMValue`](/Docs/JD.Uom.md#tuomvalue) will be the grand master of everything, encapsulating any possible value of any given UOM. However, it's currently still a work-in-progress to implement an infrastructure.

That aside, everything currently starts with [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils) in [`JD.Uom.pas`](/Docs/JD.Uom.md). Specific units are registered via `TUOMUtils.RegisterUOM`. This registration is deliberately done from **outside** this main unit, as to be as abstract as possible. You can access information about all registered UOMs through [`TUOMUtils`](/Docs/JD.Uom.md#tuomutils). For example, `GetUOMByName` or `ListCategories`. 

## Application

There is an application project aimed at both demonstration the capabilities of the library and actually converting
data in the most flexible manner possible.

### NOTE: 
This application uses the `Raize` controls, specifically `TRzSpinEdit`.

### NOTE: 
This application uses the [`JDLib`](https://github.com/djjd47130/JDLib) controls, specifically [`TJDFontButton`](https://github.com/djjd47130/JDLib/blob/master/Docs/TJDFontButton.md), also written by Jerry Dodge.

![image](https://github.com/djjd47130/JD-UOM/assets/8213266/964dae59-235f-4559-acc4-ba10065dc4af)

![image](https://github.com/djjd47130/JD-UOM/assets/8213266/cb21182d-1d2c-46a0-b36f-207e5519dfe6)

![image](https://github.com/djjd47130/JD-UOM/assets/8213266/77591b8c-fc8e-4fe6-bf91-80c1c2e9d234)



