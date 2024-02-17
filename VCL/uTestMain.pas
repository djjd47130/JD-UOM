unit uTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes, System.Win.Registry,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.Mask, Vcl.Buttons, Vcl.ComCtrls, Vcl.Clipbrd,
  //JvExMask, JvSpin,
  JD.UomUtils,
  JD.Uom.Common,
  JD.Uom.Length,
  JD.Uom.Area,
  JD.Uom.Volume,
  JD.Uom.Weight,
  JD.Uom.Temperature, RzEdit, RzSpnEdt;

type

  TTest = class(TCustomTransparentControl);

  TfrmMain = class(TForm)
    cboUOM: TComboBox;
    cboConvertFrom: TComboBox;
    cboConvertTo: TComboBox;
    lblUOM: TLabel;
    lblConvertFrom: TLabel;
    lblConvertTo: TLabel;
    cboSystem: TComboBox;
    lblUOMSystem: TLabel;
    Bevel1: TBevel;
    lblUnitFrom: TLabel;
    lblUnitTo: TLabel;
    Bevel2: TBevel;
    lblSuffixFrom: TLabel;
    lblSuffixTo: TLabel;
    Stat: TStatusBar;
    seConvertFrom: TRzSpinEdit;
    seConvertTo: TRzSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure seConvertFromChange(Sender: TObject);
    procedure cboUOMClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure seConvertToChange(Sender: TObject);
    procedure cboSystemEnter(Sender: TObject);
    procedure cboUOMEnter(Sender: TObject);
    procedure cboConvertFromEnter(Sender: TObject);
    procedure cboConvertToEnter(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    FCalculating: Boolean;
    FCalcBackward: Boolean;
    procedure LoadUOM;
    procedure LoadUOMSystems;
    procedure LoadUOMUnits;
    procedure CalcLength;
    procedure CalcArea;
    procedure CalcVolume;
    procedure CalcWeight;
    procedure CalcTemperature;
    procedure CalcEnergy;
    procedure CalcSpeed;
    procedure CalcTime;
    procedure CalcPower;
    procedure CalcData;
    procedure CalcPressure;
    procedure CalcAngle;
    procedure CalcResistance;
    procedure CalcCapacitance;
    procedure CalcVoltage;
    procedure CalcCurrent;
    procedure CalcDensity;
    procedure CalcGravity;
    procedure CalcRadiation;
  public
    //FPicker: TUOMEdit;
    //FList: TStringList;
    function CurrentUOM: TUOM;
    function CurrentSystem: TUOMSystem;
    function CurrentFromIndex: Integer;
    function CurrentToIndex: Integer;
    procedure DoCalc;
    procedure LoadPrefs;
    procedure SavePrefs;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormClick(Sender: TObject);
begin

  Self.ActiveControl:= nil;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {
  FPicker:= TUOMEdit.Create(Self);
  FPicker.Name:= 'upTest';
  FPicker.Parent:= Self;
  FPicker.Left:= Self.seConvertTo.Left;
  FPicker.Top:= Self.seConvertTo.Top + Self.seConvertTo.Height + 16;
  FPicker.Width:= Self.cboUOM.Width;
  FPicker.Height:= Self.seConvertTo.Height;
  FPicker.ShowHint:= True;
  FPicker.Font.Assign(Self.cboUOM.Font);
  FPicker.Value:= 1;
  FPicker.Visible:= False;
  }

  LoadUOMSystems;
  LoadUOM;
  Self.DoCalc;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin

  Self.ActiveControl:= nil;
end;

procedure TfrmMain.FormResize(Sender: TObject);
var
  H, HH, OH: Integer;
begin
  //Resize and Reposition Controls...
  H:= (ClientHeight - Stat.Height); //Available Height
  HH:= H div 3; //Split Height
  OH:= (cboUOM.Height + lblUOM.Height + 4) div 2;


  Bevel1.Top:= HH;
  Bevel2.Top:= HH * 2;


  cboUOM.Top:= (HH div 2) - (cboUOM.Height div 2) + OH;
  lblUOM.Top:= cboUOM.Top - lblUOM.Height - 4;

  cboSystem.Top:= cboUOM.Top;
  lblUOMSystem.Top:= lblUOM.Top;


  cboConvertFrom.Top:= HH + (HH div 2) - (cboConvertFrom.Height div 2) + OH;
  lblConvertFrom.Top:= cboConvertFrom.Top - lblConvertFrom.Height - 4;

  seConvertFrom.Top:= cboConvertFrom.Top;
  lblUnitFrom.Top:= lblConvertFrom.Top;


  cboConvertTo.Top:= HH + HH + (HH div 2) - (cboConvertTo.Height div 2) + OH;
  lblConvertTo.Top:= cboConvertTo.Top - lblConvertTo.Height - 4;

  seConvertTo.Top:= cboConvertTo.Top;
  lblUnitTo.Top:= lblConvertTo.Top;


  lblSuffixFrom.Top:= (seConvertFrom.Top + 3);
  lblSuffixTo.Top:= (seConvertTo.Top + 3);


  //{
  SetWindowRgn(cboUOM.Handle, CreateRectRgn(2,2,cboUOM.Width - 2,
               cboUOM.Height - 2), True);

  SetWindowRgn(cboSystem.Handle, CreateRectRgn(2,2,cboSystem.Width - 2,
               cboSystem.Height - 2), True);

  SetWindowRgn(cboConvertFrom.Handle, CreateRectRgn(2,2,cboConvertFrom.Width - 2,
               cboConvertFrom.Height - 2), True);

  SetWindowRgn(cboConvertTo.Handle, CreateRectRgn(2,2,cboConvertTo.Width - 2,
               cboConvertTo.Height - 2), True);

  SetWindowRgn(seConvertFrom.Handle, CreateRectRgn(2,2,seConvertFrom.Width - 2,
               seConvertFrom.Height - 2), True);

  SetWindowRgn(seConvertTo.Handle, CreateRectRgn(2,2,seConvertTo.Width - 2,
               seConvertTo.Height - 2), True);

  //}
end;

procedure TfrmMain.cboUOMClick(Sender: TObject);
begin
  LoadUOMUnits;
  //Temporarily Color Based on Completion
  if CurrentUom in [umLength, umArea, umTemperature] then
    cboUOM.Font.Color:= clWhite
  else
    cboUOM.Font.Color:= clRed;

  try
    if cboUOM.Focused then
      if seConvertFrom.CanFocus then
        seConvertFrom.SetFocus;
  except
    on E: Exception do begin
      //Nothing
    end;
  end;
end;

procedure TfrmMain.cboUOMEnter(Sender: TObject);
begin
  seConvertFrom.SetFocus;
end;

procedure TfrmMain.cboConvertFromEnter(Sender: TObject);
begin
  seConvertFrom.SetFocus;
end;

procedure TfrmMain.cboConvertToEnter(Sender: TObject);
begin
  seConvertTo.SetFocus;
end;

procedure TfrmMain.cboSystemClick(Sender: TObject);
begin
  LoadUOMUnits;

  try
    if cboSystem.Focused then
      if seConvertFrom.CanFocus then
        seConvertFrom.SetFocus;
  except
    on E: Exception do begin
      //Nothing
    end;
  end;
end;

procedure TfrmMain.cboSystemEnter(Sender: TObject);
begin
  seConvertFrom.SetFocus;
end;

function TfrmMain.CurrentFromIndex: Integer;
begin
  Result:= cboConvertFrom.ItemIndex;
end;

function TfrmMain.CurrentSystem: TUOMSystem;
begin
  Result:= TUOMSystem(cboSystem.ItemIndex);
end;

function TfrmMain.CurrentToIndex: Integer;
begin
  Result:= cboConvertTo.ItemIndex;
end;

function TfrmMain.CurrentUOM: TUOM;
begin
  Result:= TUOM(Self.cboUOM.ItemIndex);
end;

procedure TfrmMain.LoadUOM;
begin
  TUOMUtils.ListUOMs(cboUOM.Items);
  cboUOM.ItemIndex:= 0;
  cboUOMClick(nil);
end;

procedure TfrmMain.LoadUOMSystems;
begin
  TUOMUtils.ListUOMSystems(cboSystem.Items);
  cboSystem.ItemIndex:= 0;
  cboSystemClick(nil);
end;

procedure TfrmMain.LoadUOMUnits;
begin
  TUOMUtils.ListUOMUnits(CurrentUOM, cboConvertFrom.Items, CurrentSystem);
  TUOMUtils.ListUOMUnits(CurrentUOM, cboConvertTo.Items, CurrentSystem);
  cboConvertFrom.ItemIndex:= 0;
  cboConvertTo.ItemIndex:= 1;
  FCalculating:= True;
  try
    DoCalc;
  finally
    FCalculating:= False;
  end;
end;

procedure TfrmMain.seConvertFromChange(Sender: TObject);
begin
  if not FCalculating then begin
    FCalculating:= True;
    try
      FCalcBackward:= False;
      DoCalc;
    finally
      FCalculating:= False;
    end;
  end;

  try
    if cboConvertFrom.Focused then
      if seConvertFrom.CanFocus then
        seConvertFrom.SetFocus;
  except
    on E: Exception do begin
      //Nothing
    end;
  end;
end;

procedure TfrmMain.seConvertToChange(Sender: TObject);
begin
  if not FCalculating then begin
    FCalculating:= True;
    try
      FCalcBackward:= False; // True;
      DoCalc;
    finally
      FCalculating:= False;
    end;
  end;
  {
  try
    if cboConvertTo.Focused then
      if seConvertTo.CanFocus then
        seConvertTo.SetFocus;
  except
    on E: Exception do begin
      //Nothing
    end;
  end;
  }
end;

procedure TfrmMain.DoCalc;
begin
  case CurrentUOM of
    umLength:       CalcLength;
    umArea:         CalcArea;
    umVolume:       CalcVolume;
    umWeight:       CalcWeight;
    umTemperature:  CalcTemperature;
    umEnergy:       CalcEnergy;
    umSpeed:        CalcSpeed;
    umTime:         CalcTime;
    umPower:        CalcPower;
    umData:         CalcData;
    umPressure:     CalcPressure;
    umAngle:        CalcAngle;
    umResistance:   CalcResistance;
    umCapacitance:  CalcCapacitance;
    umVoltage:      CalcVoltage;
    umCurrent:      CalcCurrent;
    umDensity:      CalcDensity;
    umGravity:      CalcGravity;
    umRadiation:    CalcRadiation;
  end;
end;

procedure TfrmMain.CalcLength;
var
  ValFrom, ValResult: TUOMLength;
begin
  ValFrom.&Unit:= TUOMLengthUnit(cboConvertFrom.ItemIndex);
  ValResult.&Unit:= TUOMLengthUnit(cboConvertTo.ItemIndex);

  lblSuffixFrom.Caption:= TUOMLengthUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Caption:= TUOMLengthUtils.UnitSuffix(ValResult.&Unit);

  if FCalcBackward then begin
    ValFrom.Value:= seConvertTo.Value;
  end else begin
    ValFrom.Value:= seConvertFrom.Value;
  end;

  case ValResult.&Unit of
    umlNanometers:    ValResult.Value:= ValFrom.ToNanometers;
    umlMicrons:       ValResult.Value:= ValFrom.ToMicrons;
    umlMillimeters:   ValResult.Value:= ValFrom.ToMillimeters;
    umlCentimeters:   ValResult.Value:= ValFrom.ToCentimeters;
    umlMeters:        ValResult.Value:= ValFrom.ToMeters;
    umlKilometers:    ValResult.Value:= ValFrom.ToKilometers;
    umlInches:        ValResult.Value:= ValFrom.ToInches;
    umlFeet:          ValResult.Value:= ValFrom.ToFeet;
    umlYards:         ValResult.Value:= ValFrom.ToYards;
    umlMiles:         ValResult.Value:= ValFrom.ToMiles;
    umlNauticalMiles: ValResult.Value:= ValFrom.ToNauticalMiles;
  end;

  //TODO: I really need a proper spin edit control that handles big values...

  if FCalcBackward then begin
    seConvertFrom.Value:= ValResult.Value;
  end else begin
    seConvertTo.Value:= ValResult.Value;
  end;

end;

procedure TfrmMain.CalcArea;
var
  ValFrom, ValResult: TUOMArea;
begin
  ValFrom.&Unit:= TUOMAreaUnit(cboConvertFrom.ItemIndex);
  ValFrom.Value:= seConvertFrom.Value;

  lblSuffixFrom.Caption:= TUOMAreaUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Caption:= TUOMAreaUtils.UnitSuffix(ValResult.&Unit);

  ValResult.&Unit:= TUOMAreaUnit(cboConvertTo.ItemIndex);
  case ValResult.&Unit of
    umaSquareMillimeters: ValResult.Value:= ValFrom.ToSquareMillimeters;
    umaSquareCentimeters: ValResult.Value:= ValFrom.ToSquareCentimeters;
    umaSquareMeters:      ValResult.Value:= ValFrom.ToSquareMeters;
    umaHectares:          ValResult.Value:= ValFrom.ToHectares;
    umaSquareKilometers:  ValResult.Value:= ValFrom.ToSquareKilometers;
    umaSquareInches:      ValResult.Value:= ValFrom.ToSquareInches;
    umaSquareFeet:        ValResult.Value:= ValFrom.ToSquareFeet;
    umaSquareYards:       ValResult.Value:= ValFrom.ToSquareYards;
    umaAcres:             ValResult.Value:= ValFrom.ToAcres;
    umaSquareMiles:       ValResult.Value:= ValFrom.ToSquareMiles;
  end;

  seConvertTo.Value:= ValResult.Value;

end;

procedure TfrmMain.CalcVolume;
var
  ValFrom, ValResult: TUOMVolume;
begin
  ValFrom.&Unit:= TUOMVolumeUnit(cboConvertFrom.ItemIndex);
  ValFrom.Value:= seConvertFrom.Value;

  lblSuffixFrom.Caption:= TUOMVolumeUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Caption:= TUOMVolumeUtils.UnitSuffix(ValResult.&Unit);

  ValResult.&Unit:= TUOMVolumeUnit(cboConvertTo.ItemIndex);
  case ValResult.&Unit of
    umvMilliliters:       ValResult.Value:= ValFrom.ToMilliliters;
    umvCubicCentimeters:  ValResult.Value:= ValFrom.ToCubicCentimeters;
    umvLiters:            ValResult.Value:= ValFrom.ToLiters;
    umvCubicMeters:       ValResult.Value:= ValFrom.ToCubicMeters;
    umvTeaSpoonsUS:       ValResult.Value:= ValFrom.ToUSTeaspoons;
    umvTableSpoonsUS:     ValResult.Value:= ValFrom.ToUSTablespoons;
    umvFluidOuncesUS:     ValResult.Value:= ValFrom.ToUSFluidOunces;
    umvTeaSpoonsUK:       ValResult.Value:= ValFrom.ToUKTeaspoons;
    umvTableSpoonsUK:     ValResult.Value:= ValFrom.ToUKTablespoons;
    umvFluidOuncesUK:     ValResult.Value:= ValFrom.ToUKFluidOunces;
    umvCups:              ValResult.Value:= ValFrom.ToCups;
    umvPints:             ValResult.Value:= ValFrom.ToPints;
    umvQuarts:            ValResult.Value:= ValFrom.ToQuarts;
    umvGallons:           ValResult.Value:= ValFrom.ToGallons;
    umvCubicInches:       ValResult.Value:= ValFrom.ToCubicInches;
    umvCubicFeet:         ValResult.Value:= ValFrom.ToCubicFeet;
    umvCubicYards:        ValResult.Value:= ValFrom.ToCubicYards;
  end;

  seConvertTo.Value:= ValResult.Value;

end;

procedure TfrmMain.CalcWeight;
var
  ValFrom, ValResult: TUOMWeight;
begin
  ValFrom.&Unit:= TUOMWeightUnit(cboConvertFrom.ItemIndex);
  ValFrom.Value:= seConvertFrom.Value;

  lblSuffixFrom.Caption:= TUOMWeightUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Caption:= TUOMWeightUtils.UnitSuffix(ValResult.&Unit);

  ValResult.&Unit:= TUOMWeightUnit(cboConvertTo.ItemIndex);
  case ValResult.&Unit of
    umwCarats:        ValResult.Value:= ValFrom.ToCarats;
    umwMilliGrams:    ValResult.Value:= ValFrom.ToMilligrams;
    umwCentiGrams:    ValResult.Value:= ValFrom.ToCentigrams;
    umwDeciGrams:     ValResult.Value:= ValFrom.ToDecigrams;
    umwGrams:         ValResult.Value:= ValFrom.ToGrams;
    umwDekaGrams:     ValResult.Value:= ValFrom.ToDekagrams;
    umwHectoGrams:    ValResult.Value:= ValFrom.ToHectograms;
    umwKiloGrams:     ValResult.Value:= ValFrom.ToKilograms;
    umwMetricTonnes:  ValResult.Value:= ValFrom.ToMetricTonnes;
    umwOunces:        ValResult.Value:= ValFrom.ToOunces;
    umwPounds:        ValResult.Value:= ValFrom.ToPounds;
    umwStone:         ValResult.Value:= ValFrom.ToStones;
    umwShortTons:     ValResult.Value:= ValFrom.ToShortTons;
    umwLongTons:      ValResult.Value:= ValFrom.ToLongTons;
  end;

  seConvertTo.Value:= ValResult.Value;

end;

procedure TfrmMain.CalcTemperature;
var
  ValFrom, ValResult: TUOMTemperature;
begin
  ValFrom.&Unit:= TUOMTemperatureUnit(cboConvertFrom.ItemIndex);
  ValFrom.Value:= seConvertFrom.Value;

  lblSuffixFrom.Caption:= TUOMTemperatureUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Caption:= TUOMTemperatureUtils.UnitSuffix(ValResult.&Unit);

  ValResult.&Unit:= TUOMTemperatureUnit(cboConvertTo.ItemIndex);
  case ValResult.&Unit of
    umtCelsius:   ValResult.Value:= ValFrom.ToCelcius;
    umtFarenheit: ValResult.Value:= ValFrom.ToFarenheit;
    umtKelvin:    ValResult.Value:= ValFrom.ToKelvin;
  end;

  seConvertTo.Value:= ValResult.Value;

end;

procedure TfrmMain.CalcPower;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcPressure;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcSpeed;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcTime;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcData;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcEnergy;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcAngle;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcResistance;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcCapacitance;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcVoltage;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcCurrent;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcDensity;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcGravity;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcRadiation;
begin
  lblSuffixFrom.Caption:= '';
  lblSuffixTo.Caption:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.LoadPrefs;
begin

end;

procedure TfrmMain.SavePrefs;
begin

end;

end.
