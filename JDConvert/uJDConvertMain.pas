unit uJDConvertMain;

interface

{$DEFINE TABLE_BASED}

uses
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Mask,
  RzEdit, RzSpnEdt,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  JD.Uom,
  JD.Uom.Distance,
  JD.Uom.Area,
  JD.Uom.Temperature,
  JD.Uom.Volume,
  JD.Uom.Mass;

const
  WIDTH_SMALL = 1;
  WIDTH_LARGE = 5;
  WIDTH_CROSSHAIR = 2;

type
  TfrmJDConvertMain = class(TForm)
    pMain: TPanel;
    Panel2: TPanel;
    Label1: TLabel;
    lstUOMs: TListBox;
    Panel3: TPanel;
    Panel4: TPanel;
    Label2: TLabel;
    lstUnits: TListBox;
    Panel5: TPanel;
    Panel6: TPanel;
    lblUnitHeader: TLabel;
    txtValue: TRzSpinEdit;
    pUnitDetail: TPanel;
    Label3: TLabel;
    lblUnitName: TLabel;
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
    pSystems: TPanel;
    Label12: TLabel;
    lstSystems: TCheckListBox;
    txtChartScale: TRzSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure lstUnitsClick(Sender: TObject);
    procedure txtValueChange(Sender: TObject);
    procedure ChartAfterDraw(Sender: TObject);
    procedure txtChartScaleChange(Sender: TObject);
    procedure lstSystemsClickCheck(Sender: TObject);
  private
    FSelSystems: String;
    FSelUOM: String;
    FSelUnit: String;
  public
    procedure RefreshUOMs;
    procedure RefreshUnits;
    procedure RefreshUnitDetails;
    procedure RefreshChart;
    procedure UpdateChart;
  end;

var
  frmJDConvertMain: TfrmJDConvertMain;

implementation

{$R *.dfm}

procedure TfrmJDConvertMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  WindowState:= wsMaximized;
  Chart.Align:= alClient;
  TUOMLookupTable.ListSystems(lstSystems.Items);
  lstSystems.CheckAll(TCheckBoxState.cbChecked);
  RefreshUOMs;
end;

procedure TfrmJDConvertMain.lstUOMsClick(Sender: TObject);
begin
  RefreshUnits;
  RefreshChart;
end;

procedure TfrmJDConvertMain.cboSystemClick(Sender: TObject);
begin
  RefreshUnits;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstSystemsClickCheck(Sender: TObject);
var
  X: Integer;
begin
  FSelSystems:= '';
  for X := 0 to lstSystems.Items.Count-1 do begin
    if lstSystems.Checked[X] then begin
      if FSelSystems <> '' then
        FSelSystems:= FSelSystems + ',';
      FSelSystems:= FSelSystems + lstSystems.Items[X];
    end;
  end;
  RefreshUnits;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstUnitsClick(Sender: TObject);
begin
  FSelUnit:= lstUnits.Items[lstUnits.ItemIndex];
  RefreshUnitDetails;
  UpdateChart;
end;

procedure TfrmJDConvertMain.RefreshUOMs;
begin
  TUOMLookupTable.ListUOMs(lstUOMs.Items);
  if lstUOMs.Items.Count > 0 then begin
    lstUOMs.ItemIndex:= 0;
    lstUOMsClick(nil);
  end;
end;

procedure TfrmJDConvertMain.txtChartScaleChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrmJDConvertMain.txtValueChange(Sender: TObject);
begin
  RefreshUnitDetails;
end;

procedure TfrmJDConvertMain.RefreshUnits;
var
  FU: String;
  FS: String;
begin
  FSelUOM:= lstUOMs.Items[lstUOMs.ItemIndex];
  FU:= FSelUOM;
  FS:= FSelSystems;
  TUOMLookupTable.ListUnits(lstUnits.Items, FU, FS);
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

procedure TfrmJDConvertMain.RefreshUnitDetails;
var
  U: TUOMLookupUnit;
  BU: TUOMLookupUnit;
  BaseSuffix: String;
begin
  if lstUnits.ItemIndex < 0 then Exit;
  U:= TUOMLookupTable.GetUnitByName(FSelUnit);
  BU:= TUOMLookupTable.GetBaseUnit(lstUOMs.Items[lstUOMs.ItemIndex]);
  lblUnitName.Caption:= U.NameSingular;
  lblUnitNamePlural.Caption:= U.NamePlural;
  U.Systems.Delimiter:= ',';
  U.Systems.StrictDelimiter:= True;
  lblUnitSystems.Caption:= U.Systems.DelimitedText;
  lblUnitPrefix.Caption:= U.Prefix;
  lblUnitSuffix.Caption:= U.Suffix;
  BaseSuffix:= BU.Suffix;
  lblUnitBaseFrom.Caption:=
    FormatFloat(NumFormat, U.ConvertFromBase(txtValue.Value))+' '+U.Suffix;
  lblUnitBaseTo.Caption:=
    FormatFloat(NumFormat, U.ConvertToBase(txtValue.Value))+' '+BaseSuffix;
  UpdateChart;
end;

procedure TfrmJDConvertMain.RefreshChart;
var
  X: Integer;
  S: TLineSeries;
  U: TUOMLookupUnit;
  BU: TUOMLookupUnit;
  Y: Integer;
  V: Double;
  Amt: Integer;
begin
  Chart.SeriesList.Clear;
  Chart.Invalidate;
  if lstUnits.Items.Count <= 0 then Exit;
  Amt:= Round(txtChartScale.Value);

  BU:= TUOMLookupTable.GetBaseUnit(lstUOMs.Items[lstUOMs.ItemIndex]);
  Chart.Title.Text.Text:= BU.UOM+' Comparison';
  Chart.BottomAxis.Title.Text:= 'Base Unit - '+BU.NameSingular;
  for X := 0 to lstUnits.Count-1 do begin
    U:= TUOMLookupTable.GetUnitByName(lstUnits.Items[X]);

    S:= TLineSeries.Create(Chart);
    try
      S.Tag:= X;
      S.ParentChart:= Chart;
      S.Title:= U.NameSingular;
      if U.NameSingular = FSelUnit then
        S.LinePen.Width:= WIDTH_LARGE
      else
        S.LinePen.Width:= WIDTH_SMALL;
      for Y := -Amt to Amt do begin
        V:= U.ConvertToBase(Y);
        S.Add(V, IntToStr(Y));
      end;
    finally
      Chart.AddSeries(S);
    end;

  end;
  UpdateChart;
  Chart.Invalidate;
end;

procedure TfrmJDConvertMain.UpdateChart;
var
  S: TLineSeries;
  X: Integer;
begin
  for X := 0 to Chart.SeriesCount-1 do begin
    S:= TLineSeries(Chart.Series[X]);
    if S.Title = FSelUnit then
      S.LinePen.Width:= WIDTH_LARGE
    else
      S.LinePen.Width:= WIDTH_SMALL;
  end;
  Chart.Invalidate;
end;

procedure TfrmJDConvertMain.ChartAfterDraw(Sender: TObject);
var
  P1, P2: TPoint;
  U: TUOMLookupUnit;
begin
  if lstUOMs.ItemIndex <= 0 then Exit;
  if lstUnits.ItemIndex <= 0 then Exit;

  U:= TUOMLookupTable.GetUnitByName(FSelUnit);

  //TODO: Draw crosshair for test value conversion with selected unit...
  //Custom drawing on chart: http://www.teechart.net/docs/teechart/vclfmx/tutorials/UserGuide/html/manu390n.htm
  Chart.Canvas.Brush.Style:= bsClear;
  Chart.Canvas.Pen.Style:= psSolid;
  Chart.Canvas.Pen.Width:= WIDTH_CROSSHAIR;
  Chart.Canvas.Pen.Color:= clYellow;
  Chart.Canvas.Pen.Style:= TPenStyle.psDash;

  //Horizontal, based on test value
  P1.X:= Chart.ChartRect.Left;
  P2.X:= Chart.ChartRect.Right;
  P1.Y:= Chart.LeftAxis.CalcPosValue(txtValue.Value);
  P2.Y:= P1.Y;
  //Chart.Canvas.Line(P1, P2);

  //Vertical, based on conversion of test value to selected unit
  P1.Y:= Chart.ChartRect.Top;
  P2.Y:= Chart.ChartRect.Bottom;
  P1.X:= Chart.BottomAxis.CalcPosValue(U.ConvertToBase(txtValue.Value)); //TODO: Fix...
  P2.X:= P1.X;
  //Chart.Canvas.Line(P1, P2);

end;

end.
