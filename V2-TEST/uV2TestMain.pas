unit uV2TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.StdConvs,
  JD.Uom,
  JD.Uom.Length,
  JD.Uom.Area,
  JD.Uom.Temperature,
  JD.Uom.Volume,
  JD.Uom.Mass,
  Vcl.Mask, RzEdit, RzSpnEdt, VclTee.TeeGDIPlus,
  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart;

const
  WIDTH_SMALL = 1;
  WIDTH_LARGE = 5;

type
  TfrmMain = class(TForm)
    pMain: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    lstUOMs: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Label2: TLabel;
    cboSystem: TComboBox;
    lstUnits: TListBox;
    Panel5: TPanel;
    Panel6: TPanel;
    lblUnitHeader: TLabel;
    txtValue: TRzSpinEdit;
    pUnitDetail: TPanel;
    Label3: TLabel;
    lblUnitName: TLabel;
    Label5: TLabel;
    lblUnitID: TLabel;
    Label7: TLabel;
    lblUnitSystems: TLabel;
    Label9: TLabel;
    lblUnitPrefix: TLabel;
    Label11: TLabel;
    lblUnitSuffix: TLabel;
    Label4: TLabel;
    lblUnitBaseFrom: TLabel;
    Label8: TLabel;
    lblUnitBaseTo: TLabel;
    Label6: TLabel;
    lblUnitNamePlural: TLabel;
    Panel1: TPanel;
    Chart: TChart;
    Series1: TFastLineSeries;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure lstUnitsClick(Sender: TObject);
    procedure txtValueChange(Sender: TObject);
    procedure ChartAfterDraw(Sender: TObject);
  private
    FUOM: TUOMBaseClass;
  public
    procedure RefreshUOMs;
    procedure RefreshUnits;
    procedure RefreshUnitDetails;
    procedure RefreshChart;
    procedure UpdateChart;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.ChartAfterDraw(Sender: TObject);
begin
  //TODO: Draw crosshairs for test value conversion with selected unit...
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  WindowState:= wsMaximized;
  Chart.Align:= alClient;
  RefreshUOMs;
end;

procedure TfrmMain.lstUOMsClick(Sender: TObject);
begin
  RefreshUnits;
  RefreshChart;
end;

procedure TfrmMain.cboSystemClick(Sender: TObject);
begin
  RefreshUnits;
  RefreshChart;
end;

procedure TfrmMain.lstUnitsClick(Sender: TObject);
begin
  RefreshUnitDetails;
end;

procedure TfrmMain.RefreshUOMs;
var
  X: Integer;
  U: TUOMBaseClass;
begin
  lstUOMs.Items.Clear;
  for X := 0 to TUOMUtils.Count-1 do begin
    U:= TUOMUtils.UOM(X);
    lstUOMs.Items.Add(U.UOMName);
  end;
  if lstUOMs.Items.Count > 0 then begin
    lstUOMs.ItemIndex:= 0;
    lstUOMsClick(nil);
  end;
end;

procedure TfrmMain.txtValueChange(Sender: TObject);
begin
  RefreshUnitDetails;
end;

procedure TfrmMain.RefreshUnits;
var
  I: Integer;
  U: TUOMUnitClass;
  X: Integer;
  S: TUOMSystem;
begin
  lstUnits.Items.Clear;
  I:= lstUOMs.ItemIndex;
  if I < 0 then Exit;
  FUOM:= TUOMUtils.UOM(I);
  S:= TUOMSystem(cboSystem.ItemIndex);
  for X := 0 to FUOM.UnitCount-1 do begin
    U:= FUOM.GetUnit(X);
    if (S = ustAny) or (S in U.Systems) then
      lstUnits.Items.AddObject(U.NamePlural, Pointer(X));
  end;
  if lstUnits.Items.Count > 0 then begin
    lstUnits.ItemIndex:= 0;
    lstUnitsClick(nil);
  end;
end;

function UOMSystemsStr(const ASystems: TUOMSystems): String;
  procedure A(const System: TUOMSystem; const S: String);
  begin
    if System in ASystems then begin
      if Result <> '' then
        Result:= Result + ', ';
      Result:= Result + S;
    end;
  end;
begin
  Result:= '';
  A(ustMetric, 'Metric');
  A(ustImperial, 'Imperial');
  A(ustUSCustomary, 'US Customary');
  A(ustNatural, 'Natural');
end;

procedure TfrmMain.RefreshUnitDetails;
var
  U: TUOMUnitClass;
  I: Integer;
begin
  I:= Integer(lstUnits.Items.Objects[lstUnits.ItemIndex]);
  U:= FUOM.GetUnit(I);
  lblUnitName.Caption:= U.NameSingular;
  lblUnitNamePlural.Caption:= U.NamePlural;
  lblUnitID.Caption:= U.UnitID;
  lblUnitSystems.Caption:= UOMSystemsStr(U.Systems);
  lblUnitPrefix.Caption:= U.Prefix;
  lblUnitSuffix.Caption:= U.Suffix;
  lblUnitBaseFrom.Caption:= FormatFloat(NumFormat, U.ConvertFromBase(txtValue.Value))+' '+U.Suffix;
  lblUnitBaseTo.Caption:= FormatFloat(NumFormat, U.ConvertToBase(txtValue.Value))+' '+FUOM.BaseUnit.Suffix;
  UpdateChart;
end;

procedure TfrmMain.RefreshChart;
const
  XAXIS_COUNT = 500;
var
  X: Integer;
  S: TLineSeries;
  U: TUOMUnitClass;
  Y: Integer;
  V: Double;
  Sys: TUOMSystem;
  I: Integer;
begin
  Chart.SeriesList.Clear;
  Sys:= TUOMSystem(cboSystem.ItemIndex);
  if lstUnits.Items.Count > 0 then begin
    I:= Integer(lstUnits.Items.Objects[lstUnits.ItemIndex]);
    Chart.Title.Text.Text:= FUOM.UOMName+' Comparison';
    Chart.BottomAxis.Title.Text:= 'Base Unit - '+FUOM.BaseUnit.NameSingular;
    for X := 0 to FUOM.UnitCount-1 do begin
      U:= FUOM.GetUnit(X);
      if (Sys = ustAny) or (Sys in U.Systems) then begin
        S:= TLineSeries.Create(Chart);
        try
          S.Tag:= X;
          S.ParentChart:= Chart;
          S.Title:= U.NamePlural;
          if I = X then
            S.LinePen.Width:= WIDTH_LARGE
          else
            S.LinePen.Width:= WIDTH_SMALL;
          for Y := -XAXIS_COUNT to XAXIS_COUNT do begin
            V:= U.ConvertToBase(Y);
            S.Add(V, IntToStr(Y));
          end;
        finally
          Chart.AddSeries(S);
        end;
      end;
    end;
  end;
  Chart.Invalidate;
end;

procedure TfrmMain.UpdateChart;
var
  S: TLineSeries;
  X: Integer;
  I: Integer;
begin
  I:= Integer(lstUnits.Items.Objects[lstUnits.ItemIndex]);
  for X := 0 to Chart.SeriesCount-1 do begin
    S:= TLineSeries(Chart.Series[X]);
    if I = S.Tag then
      S.LinePen.Width:= WIDTH_LARGE
    else
      S.LinePen.Width:= WIDTH_SMALL;
  end;
  Chart.Invalidate;
end;

end.
