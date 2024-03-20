unit JD.Uom.Data;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom;

type
  TUOMDataUnit = (umdBits, umdBytes, umdKiloBits, umbKibiBits, umdKiloBytes, umdKibiBytes,
    umdMegaBits, umdMebiBits, umdMegaBytes, umdMebiBytes, umdGigaBits, umdGibiBits,
    umdGigaBytes, umdGibiBytes, umdTeraBits, umdTebiBits, umdTeraBytes, umdTebiBytes,
    umdPetaBits, umdPebiBits, umdPetaBytes, umdPebiBytes, umdExaBits, umdExbiBits,
    umdExaBytes, umdExbiBytes, umdZetaBits, umdZebiBits, umdZetaBytes, umdZebiBytes,
    umdYottaBits, umdYobiBits, umdYottaBytes, umdYobiBytes);
  TUOMDataUnits = set of TUOMDataUnit;

implementation

procedure RegisterUOMs;
begin
  //TODO...

  TUOMMetricUtils.ProduceUOMs('Data', 'Bit', 'b', [msBase,
    msKilo, msMega, msGiga, msTera, msPeta], '', 'Metric', 'Value / 8');

  TUOMMetricUtils.ProduceUOMs('Data', 'Byte', 'B', [msBase,
    msKilo, msMega, msGiga, msTera, msPeta], 'Byte');

end;

{ TUOMDataUtils }

{

class function TUOMDataUtils.UnitName(const AValue: TUOMDataUnit): String;
begin
  case AValue of
    umdBits:      Result:= 'Bits';
    umdBytes:     Result:= 'Bytes';
    umdKiloBits:  Result:= 'Kilobits';
    umbKibiBits:  Result:= 'Kibibits';
    umdKiloBytes: Result:= 'Kilobytes';
    umdKibiBytes: Result:= 'Kibibytes';
    umdMegaBits:  Result:= 'Megabits';
    umdMebiBits:  Result:= 'Mebibits';
    umdMegaBytes: Result:= 'Megabytes';
    umdMebiBytes: Result:= 'Mebibytes';
    umdGigaBits:  Result:= 'Gigabits';
    umdGibiBits:  Result:= 'Gibibits';
    umdGigaBytes: Result:= 'Gigabytes';
    umdGibiBytes: Result:= 'Gibibytes';
    umdTeraBits:  Result:= 'Terabits';
    umdTebiBits:  Result:= 'Tebibits';
    umdTeraBytes: Result:= 'Terabytes';
    umdTebiBytes: Result:= 'Tebibytes';
    umdPetaBits:  Result:= 'Petabits';
    umdPebiBits:  Result:= 'Pebibits';
    umdPetaBytes: Result:= 'Petabytes';
    umdPebiBytes: Result:= 'Pebibytes';
    umdExaBits:   Result:= 'Exabits';
    umdExbiBits:  Result:= 'Exbibits';
    umdExaBytes:  Result:= 'Exabytes';
    umdExbiBytes: Result:= 'Exbibytes';
    umdZetaBits:  Result:= 'Zetabits';
    umdZebiBits:  Result:= 'Zebibits';
    umdZetaBytes: Result:= 'Zetabytes';
    umdZebiBytes: Result:= 'Zebibytes';
    umdYottaBits: Result:= 'Yottabits';
    umdYobiBits:  Result:= 'Yobibits';
    umdYottaBytes: Result:= 'Yottabytes';
    umdYobiBytes: Result:= 'Yobibytes';
  end;
end;
}

initialization
  RegisterUOMs;
end.
