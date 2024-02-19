unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.ListBox, FMX.Edit, FMX.EditBox,
  FMX.SpinBox, FMX.NumberBox,
  JD.Uom,
  JD.Uom.Length,
  JD.Uom.Area,
  JD.Uom.Volume,
  JD.Uom.Weight,
  JD.Uom.Temperature;

type
  TfrmMain = class(TForm)
    G: TGridPanelLayout;
    Styles: TStyleBook;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel6: TPanel;
    Panel7: TPanel;
    Panel8: TPanel;
    Panel9: TPanel;
    Panel10: TPanel;
    Panel11: TPanel;
    Panel12: TPanel;
    Panel13: TPanel;
    Panel14: TPanel;
    Panel15: TPanel;
    Panel16: TPanel;
    Panel17: TPanel;
    Panel18: TPanel;
    Panel19: TPanel;
    Panel20: TPanel;
    Panel21: TPanel;
    Panel22: TPanel;
    Panel23: TPanel;
    Panel24: TPanel;
    Panel25: TPanel;
    Panel26: TPanel;
    Panel27: TPanel;
    Panel28: TPanel;
    Panel29: TPanel;
    Panel30: TPanel;
    Panel31: TPanel;
    Panel32: TPanel;
    Panel33: TPanel;
    Panel34: TPanel;
    Panel35: TPanel;
    cboUOM: TComboBox;
    Label1: TLabel;
    cboConvertFrom: TComboBox;
    Label2: TLabel;
    cboConvertTo: TComboBox;
    Label3: TLabel;
    seConvertTo: TNumberBox;
    Label4: TLabel;
    seConvertFrom: TNumberBox;
    Label5: TLabel;
    cboSystem: TComboBox;
    Label6: TLabel;
    Label7: TLabel;
    lblSuffixFrom: TLabel;
    lblSuffixTo: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure seConvertFromChange(Sender: TObject);
    procedure cboUOMClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure seConvertToChange(Sender: TObject);
    procedure cboSystemEnter(Sender: TObject);
    procedure cboUOMEnter(Sender: TObject);
    procedure cboConvertFromEnter(Sender: TObject);
    procedure cboConvertToEnter(Sender: TObject);
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

{$R *.fmx}

procedure TfrmMain.FormClick(Sender: TObject);
begin

  Self.ActiveControl:= nil;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  G.Align:= TAlignLayout.Client;
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

procedure TfrmMain.cboUOMClick(Sender: TObject);
var
  S: String;
begin
  S:= '';
  LoadUOMUnits;
  //Temporarily Color Based on Completion
  //if CurrentUom in [umLength, umArea, umTemperature] then
    //cboUOM.Font.Color:= clWhite
  //else
    //cboUOM.Font.Color:= clRed;

  try
    //if cboUOM.Focused then
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
    //if cboSystem.Focused then
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
    //if cboConvertFrom.Focused then
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

  lblSuffixFrom.Text:= TUOMLengthUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Text:= TUOMLengthUtils.UnitSuffix(ValResult.&Unit);

  if FCalcBackward then begin
    ValFrom.Value:= seConvertTo.Value;
  end else begin
    ValFrom.Value:= seConvertFrom.Value;
  end;

  case ValResult.&Unit of
    umlNanometers:    ValResult.Value:= ValFrom.AsNanometers;
    umlMicrons:       ValResult.Value:= ValFrom.AsMicrons;
    umlMillimeters:   ValResult.Value:= ValFrom.AsMillimeters;
    umlCentimeters:   ValResult.Value:= ValFrom.AsCentimeters;
    umlMeters:        ValResult.Value:= ValFrom.AsMeters;
    umlKilometers:    ValResult.Value:= ValFrom.AsKilometers;
    umlInches:        ValResult.Value:= ValFrom.AsInches;
    umlFeet:          ValResult.Value:= ValFrom.AsFeet;
    umlYards:         ValResult.Value:= ValFrom.AsYards;
    umlMiles:         ValResult.Value:= ValFrom.AsMiles;
    umlNauticalMiles: ValResult.Value:= ValFrom.AsNauticalMiles;
  end;

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

  lblSuffixFrom.Text:= TUOMAreaUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Text:= TUOMAreaUtils.UnitSuffix(ValResult.&Unit);

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

  lblSuffixFrom.Text:= TUOMVolumeUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Text:= TUOMVolumeUtils.UnitSuffix(ValResult.&Unit);

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

  lblSuffixFrom.Text:= TUOMWeightUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Text:= TUOMWeightUtils.UnitSuffix(ValResult.&Unit);

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

  lblSuffixFrom.Text:= TUOMTemperatureUtils.UnitSuffix(ValFrom.&Unit);
  lblSuffixTo.Text:= TUOMTemperatureUtils.UnitSuffix(ValResult.&Unit);

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
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcPressure;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcSpeed;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcTime;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcData;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcEnergy;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcAngle;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcResistance;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcCapacitance;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcVoltage;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcCurrent;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcDensity;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcGravity;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.CalcRadiation;
begin
  lblSuffixFrom.Text:= '';
  lblSuffixTo.Text:= '';

  seConvertTo.Value:= 0;


end;

procedure TfrmMain.LoadPrefs;
begin

end;

procedure TfrmMain.SavePrefs;
begin

end;

end.
