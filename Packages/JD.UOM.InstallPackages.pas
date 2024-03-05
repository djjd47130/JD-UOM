unit JD.UOM.InstallPackages;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  JD.Uom;

procedure Register;

implementation

procedure Register;
begin
  //TODO: Create property editor to drop-down and select a specific UOM Category.
  //TODO: Create property editor to drop-down and check specific UOM Systems.
  //TODO: Create property editor to drop-down and select a specific UOM as filtered by Category and Systems.
  //TODO: Create thorough property editor for a single TUOMValue based on Double using above capabilities...
  //  - Internally stores BASE value, with ability to convert to/from others of same category.
  //  - Must be able to edit Base value, Converted value, and UOM, as well as
  //    filter by Category and Systems.


end;

end.
