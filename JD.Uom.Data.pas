unit JD.Uom.Data;

interface

uses
  System.Classes, System.SysUtils,
  JD.Uom.Common;

type
  TUOMDataUnit = (umdBits, umdBytes, umdKiloBits, umbKibiBits, umdKiloBytes, umdKibiBytes,
    umdMegaBits, umdMebiBits, umdMegaBytes, umdMebiBytes, umdGigaBits, umdGibiBits,
    umdGigaBytes, umdGibiBytes, umdTeraBits, umdTebiBits, umdTeraBytes, umdTebiBytes,
    umdPetaBits, umdPebiBits, umdPetaBytes, umdPebiBytes, umdExaBits, umdExbiBits,
    umdExaBytes, umdExbiBytes, umdZetaBits, umdZebiBits, umdZetaBytes, umdZebiBytes,
    umdYottaBits, umdYobiBits, umdYottaBytes, umdYobiBytes);
  TUOMDataUnits = set of TUOMDataUnit;

  TUOMDataUtils = class
  public
    class procedure UnitList(AList: TStrings); static;
    class function UnitSuffix(const AValue: TUOMDataUnit): String; static;
    class function UnitName(const AValue: TUOMDataUnit): String; static;

    //TODO: All possible conversions (THIS ONE WILL BE MASSIVE!)
    class function BytesToBits(const ABytes: Double): Double; static;
  end;

  TUOMData = record
  private
    FUnit: TUOMDataUnit;
    FValue: Double;
    procedure SetUnit(const Value: TUOMDataUnit);
    procedure SetValue(const Value: Double);
  public
    property &Unit: TUOMDataUnit read FUnit write SetUnit;
    property Value: Double read FValue write SetValue;
    class operator implicit(const AValue: Double): TUOMData;
    class operator implicit(const AValue: TUOMData): Double;
    class operator implicit(const AValue: String): TUOMData;
    class operator implicit(const AValue: TUOMData): String;
  public
    function ToBits: Double;
    //TODO
  end;

implementation

var
  DefaultDataUnit: TUOMDataUnit;

{ TUOMDataUtils }

class procedure TUOMDataUtils.UnitList(AList: TStrings);
var
  V: TUOMDataUnit;
begin
  AList.Clear;
  for V:= Low(TUOMDataUnit) to High(TUOMDataUnit) do begin
    AList.Append(TUOMDataUtils.UnitName(V));
  end;
end;

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

class function TUOMDataUtils.UnitSuffix(const AValue: TUOMDataUnit): String;
begin
  case AValue of
    umdBits:      Result:= '';
    umdBytes:     Result:= '';
    umdKiloBits:  Result:= '';
    umbKibiBits:  Result:= '';
    umdKiloBytes: Result:= '';
    umdKibiBytes: Result:= '';
    umdMegaBits:  Result:= '';
    umdMebiBits:  Result:= '';
    umdMegaBytes: Result:= '';
    umdMebiBytes: Result:= '';
    umdGigaBits:  Result:= '';
    umdGibiBits:  Result:= '';
    umdGigaBytes: Result:= '';
    umdGibiBytes: Result:= '';
    umdTeraBits:  Result:= '';
    umdTebiBits:  Result:= '';
    umdTeraBytes: Result:= '';
    umdTebiBytes: Result:= '';
    umdPetaBits:  Result:= '';
    umdPebiBits:  Result:= '';
    umdPetaBytes: Result:= '';
    umdPebiBytes: Result:= '';
    umdExaBits:   Result:= '';
    umdExbiBits:  Result:= '';
    umdExaBytes:  Result:= '';
    umdExbiBytes: Result:= '';
    umdZetaBits:  Result:= '';
    umdZebiBits:  Result:= '';
    umdZetaBytes: Result:= '';
    umdZebiBytes: Result:= '';
    umdYottaBits: Result:= '';
    umdYobiBits:  Result:= '';
    umdYottaBytes: Result:= '';
    umdYobiBytes: Result:= '';
  end;
end;

class function TUOMDataUtils.BytesToBits(const ABytes: Double): Double;
begin
  //TODO
  Result:= 0;
end;

{ TUOMData }

class operator TUOMData.implicit(const AValue: TUOMData): Double;
begin
  Result:= 0;
  case DefaultDataUnit of
    umdBits: Result:= AValue.ToBits;
    //TODO
  end;
end;

class operator TUOMData.implicit(const AValue: Double): TUOMData;
begin
  Result.FUnit:= DefaultDataUnit;
  Result.FValue:= AValue;
end;

class operator TUOMData.implicit(const AValue: String): TUOMData;
begin
  //TODO: Parse string...

end;

class operator TUOMData.implicit(const AValue: TUOMData): String;
begin
  Result:= FormatFloat(NumFormat, AValue.FValue);
  Result:= Result + TUOMDataUtils.UnitSuffix(AValue.&Unit);
end;

procedure TUOMData.SetUnit(const Value: TUOMDataUnit);
begin
  case FUnit of
    umdBits:   FValue:= Self.ToBits;
    //TODO
  end;
  FUnit:= Value;
end;

procedure TUOMData.SetValue(const Value: Double);
begin
  FValue:= Value;
end;

function TUOMData.ToBits: Double;
begin
  Result:= 0;
  case FUnit of
    umdBits:   Result:= FValue; //Same
    umdBytes: Result:= TUOMDataUtils.BytesToBits(FValue);
    //TODO
  end;
end;

initialization
  DefaultDataUnit:= TUOMDataUnit.umdMegaBytes;
end.
