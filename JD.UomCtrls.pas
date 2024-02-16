unit JD.UomCtrls;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.Classes, System.SysUtils, System.Generics.Collections,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls,
  Vcl.Graphics,

  JD.UomUtils,
  JD.Uom.Common,
  JD.Uom.Length,
  JD.Uom.Area,
  JD.Uom.Volume,
  JD.Uom.Weight,
  JD.Uom.Temperature,
  JD.Uom.Angle,
  JD.Uom.Capacitance,
  JD.Uom.Current,
  JD.Uom.Data,
  JD.Uom.Density,
  JD.Uom.Energy,
  JD.Uom.Gravity,
  JD.Uom.Power,
  JD.Uom.Pressure,
  JD.Uom.Radiation,
  JD.Uom.Resistance,
  JD.Uom.Speed,
  JD.Uom.Time,
  JD.Uom.Voltage;

type
  TUOMEdit = class(TCustomComboBox)
  private
    FUOM: TUOM;
    FValue: Double;
    FUnits: TStringList;
    procedure SetUOM(const Value: TUOM);
    procedure SetValue(const Value: Double);
    procedure SetUnitIndex(const Value: Integer);
    function GetUnitIndex: Integer;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
  protected
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure SetItemIndex(const Value: Integer); override;
    procedure CloseUp; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshList;
  published
    property Align;
    property AlignWithMargins;
    property Anchors;
    property Font;
    property Margins;
    property UOM: TUOM read FUOM write SetUOM;
    property Value: Double read FValue write SetValue;
    property UnitIndex: Integer read GetUnitIndex write SetUnitIndex;
    property Text: TCaption read GetText write SetText;
    property Height;
    property Width;
    property Left;
    property Top;
  end;


implementation

uses
  Vcl.Themes, Vcl.Styles;

{ TUOMEdit }

procedure TUOMEdit.CloseUp;
begin
  inherited;
  Text:= GetText;
end;

constructor TUOMEdit.Create(AOwner: TComponent);
begin
  inherited;
  FUnits:= TStringList.Create;
  RefreshList;
end;

procedure TUOMEdit.CreateWnd;
begin
  inherited;
  RefreshList;
end;

destructor TUOMEdit.Destroy;
begin
  FreeAndNil(FUnits);
  inherited;
end;

procedure TUOMEdit.DestroyWnd;
begin
  inherited;

end;

function TUOMEdit.GetUnitIndex: Integer;
begin
  Result:= ItemIndex;
end;

procedure TUOMEdit.RefreshList;
begin
  if Self.WindowHandle <> 0 then begin
    TUomUtils.ListUOMUnits(FUOM, Items);
    UnitIndex:= 0;
  end;
end;

procedure TUOMEdit.SetUnitIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < FUnits.Count) then
    ItemIndex := Value
  else
    ItemIndex:= -1;
  SetItemIndex(Value);
  Text:= GetText;
end;

procedure TUOMEdit.SetUOM(const Value: TUOM);
begin
  FUOM := Value;
  RefreshList;
end;

procedure TUOMEdit.SetValue(const Value: Double);
begin
  FValue := Value;
end;

function TUOMEdit.GetText: TCaption;
var
  Suffix: String;
begin
  Result:= inherited Text;
  case FUOM of
    umLength:       Suffix:= TUOMLengthUtils.UnitSuffix(TUOMLengthUnit(UnitIndex));
    umArea:         Suffix:= TUOMAreaUtils.UnitSuffix(TUOMAreaUnit(UnitIndex));
    umVolume:       Suffix:= TUOMVolumeUtils.UnitSuffix(TUOMVolumeUnit(UnitIndex));
    umWeight:       Suffix:= TUOMWeightUtils.UnitSuffix(TUOMWeightUnit(UnitIndex));
    umTemperature:  Suffix:= TUOMTemperatureUtils.UnitSuffix(TUOMTemperatureUnit(UnitIndex));
    umEnergy:       ;
    umSpeed:        ;
    umTime:         ;
    umPower:        ;
    umData:         ;
    umPressure:     ;
    umAngle:        ;
    umResistance:   ;
    umCapacitance:  ;
    umVoltage:      ;
    umCurrent:      ;
    umDensity:      ;
  end;
  Result:= FormatFloat(NumFormat, FValue);
end;

procedure TUOMEdit.SetItemIndex(const Value: Integer);
begin
  inherited;
  Text:= GetText;
end;

procedure TUOMEdit.SetText(const Value: TCaption);
var
  Num: Double;
  Suf: String;
begin
  inherited Text:= Value;
  TUOMUtils.ParseSuffix(Value, Num, Suf);
  case FUOM of
    umLength:       UnitIndex:= Integer(TUOMLengthUtils.StrToUnit(Suf));
    umArea:         ;
    umVolume:       ;
    umWeight:       ;
    umTemperature:  ;
    umEnergy:       ;
    umSpeed:        ;
    umTime:         ;
    umPower:        ;
    umData:         ;
    umPressure:     ;
    umAngle:        ;
    umResistance:   ;
    umCapacitance:  ;
    umVoltage:      ;
    umCurrent:      ;
    umDensity:      ;
  end;
end;

end.
