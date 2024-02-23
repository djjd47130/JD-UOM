unit JD.Uom.Frequency;

{
TODO: Frequency units of measurement such as MHz, GHz...

Ref: https://en.wikipedia.org/wiki/Frequency
- I'm surprised Wikipedia says literally nothing about computer processors...

GOAL: Make Frequency as abstract as possible, then use it elsewhere as necessary,
such as Speed and Radiation.

}

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMFrequencyUnit = (umfTeraHertz, umfGigaHertz, umfMegaHertz, umfKiloHertz,
    umfHertz);
  TUOMFrequencyUnits = set of TUOMFrequencyUnit;

  TUOMFrequencyUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMFrequencyUnit): String; static;
    class function UnitName(const AValue: TUOMFrequencyUnit): String; static;


  end;

  TUOMFrequency = record
  private
    //FValue: Double;
    //FUOM: TUOM;
  public
    //TODO: Need a way to filter to ONLY UOMs which are compatible...
  end;

implementation

{ TUOMFrequencyUtils }

class procedure TUOMFrequencyUtils.UnitList(AList: TStrings);
begin

end;

class function TUOMFrequencyUtils.UnitName(
  const AValue: TUOMFrequencyUnit): String;
begin

end;

class function TUOMFrequencyUtils.UnitSuffix(
  const AValue: TUOMFrequencyUnit): String;
begin

end;

end.
