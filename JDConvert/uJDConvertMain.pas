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
  JD.Uom.Mass,
  JD.Uom.Time;

const
  WIDTH_SMALL = 1;
  WIDTH_LARGE = 5;
  WIDTH_CROSSHAIR = 2;

type
  TfrmJDConvertMain = class(TForm)
    pMain: TPanel;
    pCategories: TPanel;
    Label1: TLabel;
    lstCategories: TListBox;
    pUOMs: TPanel;
    lstUOMs: TListBox;
    pInfo: TPanel;
    pTestVal: TPanel;
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
    pChart: TPanel;
    Chart: TChart;
    Series1: TFastLineSeries;
    Label10: TLabel;
    pSystems: TPanel;
    Label12: TLabel;
    lstSystems: TCheckListBox;
    txtChartScale: TRzSpinEdit;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lstCategoriesClick(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure txtValueChange(Sender: TObject);
    procedure ChartAfterDraw(Sender: TObject);
    procedure txtChartScaleChange(Sender: TObject);
    procedure lstSystemsClickCheck(Sender: TObject);
  private
    FSelSystems: String;
    FSelCategory: String;
    FSelUOM: String;
    procedure RefreshSystems;
  public
    procedure RefreshCategories;
    procedure RefreshUOMs;
    procedure RefreshUOMDetails;
    procedure RefreshChart;
    procedure UpdateChart;
  end;

var
  frmJDConvertMain: TfrmJDConvertMain;

implementation

{$R *.dfm}

{ TfrmJDConvertMain }

procedure TfrmJDConvertMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  WindowState:= wsMaximized;
  Chart.Align:= alClient;
  RefreshSystems;
  RefreshCategories;
end;

procedure TfrmJDConvertMain.RefreshSystems;
var
  X: Integer;
  S: String;
begin
  TUOMUtils.ListSystems(lstSystems.Items);
  lstSystems.CheckAll(TCheckBoxState.cbChecked);
  for X := 0 to lstSystems.Items.Count-1 do begin
    S:= lstSystems.Items[X];
    if (S = 'Natural') or (S = 'Random') then
      lstSystems.Checked[X]:= False;
  end;
  lstSystemsClickCheck(nil);
end;

procedure TfrmJDConvertMain.lstCategoriesClick(Sender: TObject);
begin
  RefreshUOMs;
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
  RefreshUOMs;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstUOMsClick(Sender: TObject);
begin
  FSelUOM:= lstUOMs.Items[lstUOMs.ItemIndex];
  RefreshUOMDetails;
  UpdateChart;
end;

procedure TfrmJDConvertMain.RefreshCategories;
begin
  TUOMUtils.ListCategories(lstCategories.Items);
  if lstCategories.Items.Count > 0 then begin
    lstCategories.ItemIndex:= 0;
    lstCategoriesClick(nil);
  end;
end;

procedure TfrmJDConvertMain.txtChartScaleChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrmJDConvertMain.txtValueChange(Sender: TObject);
begin
  RefreshUOMDetails;
end;

procedure TfrmJDConvertMain.RefreshUOMs;
var
  FS: String;
begin
  if lstCategories.ItemIndex < 0 then Exit;
  FSelCategory:= lstCategories.Items[lstCategories.ItemIndex];
  FS:= FSelSystems;
  TUOMUtils.ListUOMs(lstUOMs.Items, FSelCategory, FS);
  if lstUOMs.Items.Count > 0 then begin
    lstUOMs.ItemIndex:= 0;
    lstUOMsClick(nil);
  end;
end;

procedure TfrmJDConvertMain.RefreshUOMDetails;
var
  U: TUOM;
  BU: TUOM;
  BaseSuffix: String;
begin
  if lstUOMs.ItemIndex < 0 then Exit;
  U:= TUOMUtils.GetUOMByName(FSelUOM);
  BU:= TUOMUtils.GetBaseUOM(lstCategories.Items[lstCategories.ItemIndex]);
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
  U: TUOM;
  BU: TUOM;
  Y: Integer;
  V: Double;
  Amt: Integer;
begin
  Chart.SeriesList.Clear;
  Chart.Invalidate;
  if lstUOMs.Items.Count <= 0 then Exit;
  Amt:= Round(txtChartScale.Value);

  BU:= TUOMUtils.GetBaseUOM(FSelCategory);
  Chart.Title.Text.Text:= BU.Category+' Comparison';
  Chart.BottomAxis.Title.Text:= 'Base UOM - '+BU.NameSingular;
  for X := 0 to lstUOMs.Count-1 do begin
    U:= TUOMUtils.GetUOMByName(lstUOMs.Items[X]);
    S:= TLineSeries.Create(Chart);
    try
      S.Tag:= X;
      S.ParentChart:= Chart;
      S.Title:= U.NameSingular;
      if U.NameSingular = FSelUOM then
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
    if S.Title = FSelUOM then
      S.LinePen.Width:= WIDTH_LARGE
    else
      S.LinePen.Width:= WIDTH_SMALL;
  end;
  Chart.Invalidate;
end;

procedure TfrmJDConvertMain.ChartAfterDraw(Sender: TObject);
var
  P1, P2: TPoint;
  U: TUOM;
begin
  if lstCategories.ItemIndex <= 0 then Exit;
  if lstUOMs.ItemIndex <= 0 then Exit;

  U:= TUOMUtils.GetUOMByName(FSelUOM);

  //TODO: Draw crosshair for test value conversion with selected UOM...
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
