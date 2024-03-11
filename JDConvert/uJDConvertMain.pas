unit uJDConvertMain;

interface

{$DEFINE TABLE_BASED}

uses
  Winapi.Windows, Winapi.Messages,
  System.Generics.Collections, System.Generics.Defaults,
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
  JD.Uom.Time,
  JD.Uom.Frequency,
  JD.Uom.Speed;

const
  WIDTH_SMALL = 2;
  WIDTH_LARGE = 6;
  WIDTH_CROSSHAIR = 1;

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
    Label11: TLabel;
    lblUnitSuffix: TLabel;
    Label4: TLabel;
    lblUnitBaseFrom: TLabel;
    Label8: TLabel;
    lblUnitBaseTo: TLabel;
    Label6: TLabel;
    lblUnitNamePlural: TLabel;
    pBottom: TPanel;
    Chart: TChart;
    Label10: TLabel;
    pSystems: TPanel;
    Label12: TLabel;
    lstSystems: TCheckListBox;
    txtChartScale: TRzSpinEdit;
    Label2: TLabel;
    chkNegative: TCheckBox;
    Series1: TLineSeries;
    pConvert: TPanel;
    lblConvertTitle: TLabel;
    Label5: TLabel;
    txtConvertFromValue: TRzSpinEdit;
    cboConvertFromUnit: TComboBox;
    btnConvert: TButton;
    lstEquivalents: TListBox;
    lblEquivalentsTitle: TLabel;
    Label9: TLabel;
    Splitter1: TSplitter;
    procedure FormCreate(Sender: TObject);
    procedure lstCategoriesClick(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure txtValueChange(Sender: TObject);
    procedure ChartAfterDraw(Sender: TObject);
    procedure txtChartScaleChange(Sender: TObject);
    procedure lstSystemsClickCheck(Sender: TObject);
    procedure chkNegativeClick(Sender: TObject);
    procedure cboConvertFromUnitClick(Sender: TObject);
    procedure txtConvertFromValueChange(Sender: TObject);
    procedure lstEquivalentsDblClick(Sender: TObject);
    procedure lstEquivalentsDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
  private
    FSelSystems: String;
    FSelCategory: String;
    FSelUOM: String;
  public
    procedure RefreshSystems;
    procedure RefreshCategories;
    procedure RefreshUOMs;
    procedure RefreshUOMDetails;
    procedure RefreshChart;
    procedure UpdateChart;
    procedure RefreshConvert;
    procedure RefreshEquivalents;
  end;

var
  frmJDConvertMain: TfrmJDConvertMain;

implementation

{$R *.dfm}

uses
  ClipBrd;

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
  lstSystems.CheckAll(TCheckBoxState.cbUnchecked);
  for X := 0 to lstSystems.Items.Count-1 do begin
    S:= lstSystems.Items[X];
    if (S = 'Metric') or (S = 'US Customary') or (S = 'Random') then
      lstSystems.Checked[X]:= True;
  end;
  lstSystemsClickCheck(nil);
end;

procedure TfrmJDConvertMain.chkNegativeClick(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstCategoriesClick(Sender: TObject);
begin
  RefreshUOMs;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstEquivalentsDblClick(Sender: TObject);
begin
  //TODO: Copy to clipboard...
  Clipboard.AsText:= lstEquivalents.Items[lstEquivalents.ItemIndex];
end;

procedure TfrmJDConvertMain.lstEquivalentsDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  U: TUOM;
  C: TCanvas;
  S: String;
begin
  //TODO: Why is none of this working?
  C:= lstEquivalents.Canvas;
  U:= TUOM(lstEquivalents.Items.Objects[Index]);
  S:= lstEquivalents.Items[Index];
  if U.NameSingular = cboConvertFromUnit.Text then begin
    //Selected "FROM" unit...
    C.Font.Color:= clSkyBlue;
  end else begin
    //Anything else...
   C.Font.Color:= clWhite;
  end;
  C.Brush.Style:= bsSolid;
  if odSelected in State then
    C.Brush.Color:= clNavy
  else
    C.Brush.Color:= clBlack;
  C.Pen.Style:= psClear;
  C.Rectangle(Rect);
  InflateRect(Rect, -2, -2);
  DrawText(C.Handle, PChar(S), Length(S), Rect, DT_SINGLELINE);
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

procedure TfrmJDConvertMain.txtConvertFromValueChange(Sender: TObject);
begin
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.txtValueChange(Sender: TObject);
begin
  RefreshUOMDetails;
end;

function CompareUOMVal(const A, B: TUOMValue): Integer;
var
  UA, UB: TUOM;
  VA, VB: Double;
begin
  UA:= TUOMUtils.GetUOMByName(A.UOM);
  UB:= TUOMUtils.GetUOMByName(B.UOM);
  VA:= UA.ConvertToBase(1);
  VB:= UB.ConvertToBase(1);
  if VA = VB then Result:= 0 else begin
    if VB > VA then
      Result:= 1
    else
      Result:= -1;
  end;
end;

procedure TfrmJDConvertMain.RefreshUOMs;
var
  L: TStringList;
  FS: String;
  X: Integer;
  //Comp: TComparison<TUOMValue>;
begin
  lstUOMs.Items.Clear;
  if lstCategories.ItemIndex < 0 then Exit;
  FSelCategory:= lstCategories.Items[lstCategories.ItemIndex];
  FS:= FSelSystems;
  L:= TStringList.Create;
  try
    TUOMUtils.ListUOMs(L, FSelCategory, FS);
    //TODO: Sort by size...?
    //L.CustomSort(CompareUOMVal);
    for X := 0 to L.Count-1 do begin
      lstUOMs.Items.AddObject(L[X], L.Objects[X]);
    end;
    if lstUOMs.Items.Count > 0 then begin
      lstUOMs.ItemIndex:= 0;
      lstUOMsClick(nil);
    end;
  finally
    L.Free;
  end;
  RefreshConvert;
end;

procedure TfrmJDConvertMain.RefreshUOMDetails;
var
  U: TUOM;
  BU: TUOM;
begin
  if lstUOMs.ItemIndex < 0 then Exit;
  U:= TUOMUtils.GetUOMByName(FSelUOM);
  BU:= TUOMUtils.GetBaseUOM(lstCategories.Items[lstCategories.ItemIndex]);
  lblUnitName.Caption:= U.NameSingular;
  lblUnitNamePlural.Caption:= U.NamePlural;
  lblUnitSystems.Caption:= U.Systems.DelimitedText;
  lblUnitSuffix.Caption:= U.Suffix;
  lblUnitBaseFrom.Caption:=
    FormatFloat(NumFormat, U.ConvertFromBase(txtValue.Value))+' '+U.Suffix;
  lblUnitBaseTo.Caption:=
    FormatFloat(NumFormat, U.ConvertToBase(txtValue.Value))+' '+BU.Suffix;
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
  Start: Integer;
begin
  Chart.SeriesList.Clear;
  Chart.Invalidate;
  Screen.Cursor:= crHourglass;
  try
    Application.ProcessMessages;

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
        if chkNegative.Checked then
          Start:= -Amt
        else
          Start:= 0;
        for Y := Start to Amt do begin
          V:= U.ConvertToBase(Y);
          S.Add(V, IntToStr(Y));
        end;
      finally
        Chart.AddSeries(S);
      end;

    end;
    UpdateChart;
    Chart.Invalidate;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TfrmJDConvertMain.RefreshConvert;
var
  Base: TUOM;
begin
  lblConvertTitle.Caption:= 'Convert '+FSelCategory;
  Base:= TUOMUtils.GetBaseUOM(FSelCategory);
  cboConvertFromUnit.Items.Assign(lstUOMs.Items);
  cboConvertFromUnit.ItemIndex:= cboConvertFromUnit.Items.IndexOf(Base.NameSingular);
  RefreshEquivalents;
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

procedure TfrmJDConvertMain.RefreshEquivalents;
var
  L: TStringList;
  X: Integer;
  FU, TU: TUOM;
  FV, TV: Double;
  Str: String;
  UN: String;
begin
  lstEquivalents.Items.Clear;
  FU:= TUOMUtils.GetUOMByName(cboConvertFromUnit.Text);
  FV:= txtConvertFromValue.Value;
  L:= TStringList.Create;
  try
    TUOMUtils.ListUOMs(L, FSelCategory);
    for X := 0 to L.Count-1 do begin
      TU:= TUOMUtils.GetUOMByName(L[X]);
      TV:= TUOMUtils.Convert(FV, FU.NameSingular, TU.NameSingular);
      if TV = 1 then UN:= TU.NameSingular else UN:= TU.NamePlural;
      Str:= FormatFloat(NumFormat, TV)+' '+UN+' ('+TU.Suffix+')';
      lstEquivalents.Items.AddObject(Str, TU);
    end;
  finally
    L.Free;
  end;
end;

procedure TfrmJDConvertMain.cboConvertFromUnitClick(Sender: TObject);
begin
  RefreshEquivalents;
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
