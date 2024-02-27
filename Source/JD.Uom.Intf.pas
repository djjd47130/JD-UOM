unit JD.Uom.Intf;

interface

uses
  System.Classes, System.SysUtils, System.Types,
  JD.Uom;

type
  IUOMBase = interface
    ['{008FD2FE-5FF6-4B77-8C09-2C8E58E018BC}']
    function UOMID: String;
    function UOMName: String;
    function UnitCount: Integer;
    function GetUnit(const Index: Integer): TUOMUnitClass;
    procedure UnitList(AList: TStrings; ASystem: TUOMSystem = ustAny);
    function UnitName(const Index: Integer): String;
    function UnitPrefix(const Index: Integer): String;
    function UnitSuffix(const Index: Integer): String;
  end;

  IUOMUnitBase = interface
    ['{5BA7C745-E30F-409D-9C58-0A2AD8676D3C}']

  end;

  IUOMUtils = interface
    ['{C3C58143-6F3B-4978-848D-51CB5230465C}']
    procedure RegisterUOM(AClass: TUOMBaseClass);
    function Count: Integer;
    function UOM(const Index: Integer): TUOMBaseClass;
    function IndexOf(AClass: TUOMBaseClass): Integer; overload;
    function IndexOf(AClass: String): Integer; overload;
  end;

implementation

end.
