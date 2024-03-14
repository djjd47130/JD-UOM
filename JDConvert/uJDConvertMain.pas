unit uJDConvertMain;

interface

{$DEFINE TABLE_BASED}

uses
  Winapi.Windows, Winapi.Messages,
  System.Generics.Collections, System.Generics.Defaults,
  System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Mask,
  ShellAPI,
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
  JD.Uom.Speed,
  JD.Uom.Numbers, Vcl.ComCtrls, System.ImageList, Vcl.ImgList, JD.Common,
  JD.Graphics,
  JD.FontGlyphs, JD.Ctrls, JD.Ctrls.FontButton, Vcl.WinXCtrls;

const
  WIDTH_SMALL = 2;
  WIDTH_LARGE = 6;
  WIDTH_CROSSHAIR = 1;

type
  TfrmJDConvertMain = class(TForm)
    Pages: TPageControl;
    tabConvert: TTabSheet;
    tabDetails: TTabSheet;
    pBottom: TPanel;
    Chart: TChart;
    txtChartScale: TRzSpinEdit;
    chkNegative: TCheckBox;
    Series1: TLineSeries;
    pTop: TPanel;
    pCategories: TPanel;
    Label1: TLabel;
    lstCategories: TListView;
    pUOMs: TPanel;
    Label2: TLabel;
    lstUOMs: TListView;
    pInfo: TPanel;
    pTestVal: TPanel;
    lblUnitHeader: TLabel;
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
    pSystems: TPanel;
    Label12: TLabel;
    lstSystems: TListView;
    pConvert: TPanel;
    lblEquivalentsTitle: TLabel;
    Label9: TLabel;
    lstEquivalents: TListView;
    Panel2: TPanel;
    btnConvertNormal: TJDFontButton;
    btnConvertSearch: TJDFontButton;
    btnDetails: TJDFontButton;
    pConvertNormal: TPanel;
    Panel1: TPanel;
    lblConvertTitle: TLabel;
    cboConvertCategory: TComboBox;
    pConvertSearch: TPanel;
    txtSearch: TSearchBox;
    Panel3: TPanel;
    Label10: TLabel;
    txtConvertFromValue: TRzSpinEdit;
    Stat: TStatusBar;
    Label13: TLabel;
    cboConvertFromUnit: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure txtValueChange(Sender: TObject);
    procedure txtChartScaleChange(Sender: TObject);
    procedure chkNegativeClick(Sender: TObject);
    procedure cboConvertFromUnitClick(Sender: TObject);
    procedure txtConvertFromValueChange(Sender: TObject);
    procedure lstEquivalentsDblClick(Sender: TObject);
    procedure lstSystemsItemChecked(Sender: TObject; Item: TListItem);
    procedure cboConvertCategoryClick(Sender: TObject);
    procedure lstCategoriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstUOMsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnConvertNormalClick(Sender: TObject);
    procedure btnConvertSearchClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
    procedure lstSystemsClick(Sender: TObject);
    procedure StatDblClick(Sender: TObject);
  private
    FSelSystems: String;
    FSelCategory: String;
    FSelUOM: String;
    procedure ResetButtonColors;
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
var
  X: Integer;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  //WindowState:= wsMaximized;
  ColorManager.BaseColor:= clBlack;
  pConvert.Align:= alClient;
  Chart.Align:= alClient;
  RefreshSystems;
  RefreshCategories;
  TUOMUtils.ListCategories(cboConvertCategory.Items);
  if cboConvertCategory.Items.Count > 0 then
    cboConvertCategory.ItemIndex:= 0;
  RefreshConvert;
  for X := 0 to Pages.PageCount-1 do
    Pages.Pages[X].TabVisible:= False;
  btnConvertNormalClick(nil);
  Stat.Panels[0].Text:= IntToStr(TUOMUtils.UOMCount) + ' UOMs Registered';
end;

procedure TfrmJDConvertMain.btnConvertNormalClick(Sender: TObject);
begin
  Pages.ActivePage:= tabConvert;
  ResetButtonColors;
  btnConvertNormal.Image.StandardColor:= fcOrange;

  pConvertNormal.Visible:= True;
  pConvertSearch.Visible:= False;
end;

procedure TfrmJDConvertMain.btnConvertSearchClick(Sender: TObject);
begin
  Pages.ActivePage:= tabConvert;
  ResetButtonColors;
  btnConvertSearch.Image.StandardColor:= fcOrange;

  pConvertSearch.Visible:= True;
  pConvertNormal.Visible:= False;
  txtSearch.SetFocus;
end;

procedure TfrmJDConvertMain.btnDetailsClick(Sender: TObject);
begin
  ResetButtonColors;
  btnDetails.Image.StandardColor:= fcOrange;

  Pages.ActivePage:= tabDetails;
end;

procedure TfrmJDConvertMain.ResetButtonColors;
begin
  btnConvertNormal.Image.StandardColor:= fcBlue;
  btnConvertSearch.Image.StandardColor:= fcBlue;
  btnDetails.Image.StandardColor:= fcBlue;
end;

procedure OpenWebPageInDefaultBrowser(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmJDConvertMain.StatDblClick(Sender: TObject);
begin
  OpenWebPageInDefaultBrowser('https://github.com/djjd47130/JD-UOM');
end;

procedure TfrmJDConvertMain.RefreshSystems;
var
  L: TStringList;
  X: Integer;
  S: String;
  I: TListItem;
begin
  lstSystems.Items.Clear;
  L:= TStringList.Create;
  try
    TUOMUtils.ListSystems(L);
    for X := 0 to L.Count-1 do begin
      S:= L[X];
      I:= lstSystems.Items.Add;
      I.Caption:= S;
      I.Checked:= (S = 'Metric') or (S = 'US Customary') or (S = 'Random');
    end;
    lstSystemsItemChecked(nil, nil);
  finally
    L.Free;
  end;
end;

procedure TfrmJDConvertMain.chkNegativeClick(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstCategoriesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  RefreshUOMs;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstEquivalentsDblClick(Sender: TObject);
begin
  Clipboard.AsText:= lstEquivalents.Selected.Caption;
end;

procedure TfrmJDConvertMain.lstSystemsClick(Sender: TObject);
begin
  if lstSystems.Selected <> nil then
    lstSystems.Selected.Checked:= not lstSystems.Selected.Checked;
end;

procedure TfrmJDConvertMain.lstSystemsItemChecked(Sender: TObject;
  Item: TListItem);
var
  X: Integer;
begin
  FSelSystems:= '';
  for X := 0 to lstSystems.Items.Count-1 do begin
    if lstSystems.Items[X].Checked then begin
      if FSelSystems <> '' then
        FSelSystems:= FSelSystems + ',';
      FSelSystems:= FSelSystems + lstSystems.Items[X].Caption;
    end;
  end;
  RefreshUOMs;
  RefreshChart;
end;

procedure TfrmJDConvertMain.lstUOMsSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if lstUOMs.Selected <> nil then
    FSelUOM:= lstUOMs.Selected.Caption
  else
    FSelUOM:= '';
  RefreshUOMDetails;
  UpdateChart;
end;

procedure TfrmJDConvertMain.RefreshCategories;
var
  L: TStringList;
  X: Integer;
  I: TListItem;
begin
  lstCategories.Items.Clear;
  L:= TStringList.Create;
  try
    TUOMUtils.ListCategories(L);
    for X := 0 to L.Count-1 do begin
      I:= lstCategories.Items.Add;
      I.Caption:= L[X];
    end;
  finally
    L.Free;
  end;

  if lstCategories.Items.Count > 0 then begin
    lstCategories.ItemIndex:= 0;
    lstCategoriesSelectItem(nil, nil, False);
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
  I: TListItem;
begin
  lstUOMs.Items.Clear;
  if lstCategories.ItemIndex < 0 then Exit;
  FSelCategory:= lstCategories.Selected.Caption;
  FS:= FSelSystems;
  L:= TStringList.Create;
  try
    TUOMUtils.ListUOMs(L, FSelCategory, FS);
    for X := 0 to L.Count-1 do begin
      I:= lstUOMs.Items.Add;
      I.Caption:= L[X];
      I.Data:= L.Objects[X];
    end;
    //TODO: Sort by size...?
    //lstUOMs.CustomSort(CompareUOMVal);
    if lstUOMs.Items.Count > 0 then begin
      lstUOMs.ItemIndex:= 0;
      lstUOMsSelectItem(nil, nil, False);
    end;
  finally
    L.Free;
  end;
end;

procedure TfrmJDConvertMain.RefreshUOMDetails;
var
  U: TUOM;
begin
  if lstUOMs.ItemIndex < 0 then Exit;
  U:= TUOMUtils.GetUOMByName(FSelUOM);
  lblUnitName.Caption:= U.NameSingular;
  lblUnitNamePlural.Caption:= U.NamePlural;
  lblUnitSystems.Caption:= U.Systems.DelimitedText;
  lblUnitSuffix.Caption:= U.Suffix;
  lblUnitBaseFrom.Caption:= U.ConvertFromBaseFormula;
  lblUnitBaseTo.Caption:= U.ConvertToBaseFormula;
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
    for X := 0 to lstUOMs.Items.Count-1 do begin
      U:= TUOMUtils.GetUOMByName(lstUOMs.Items[X].Caption);
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
  Cat: String;
  Base: TUOM;
begin
  Cat:= cboConvertCategory.Text;
  Base:= TUOMUtils.GetBaseUOM(Cat);
  TUOMUtils.ListUOMs(cboConvertFromUnit.Items, Cat);
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
  I: TListItem;
begin
  lstEquivalents.Items.Clear;
  FU:= TUOMUtils.GetUOMByName(cboConvertFromUnit.Text);
  if FU = nil then Exit;
  FV:= txtConvertFromValue.Value;
  L:= TStringList.Create;
  try
    TUOMUtils.ListUOMs(L, cboConvertCategory.Text);
    for X := 0 to L.Count-1 do begin
      TU:= TUOMUtils.GetUOMByName(L[X]);
      TV:= TUOMUtils.Convert(FV, FU.NameSingular, TU.NameSingular);
      if TV = 1 then UN:= TU.NameSingular else UN:= TU.NamePlural;
      //Str:= FormatFloat(NumFormat, TV)+' '+UN+' ('+TU.Suffix+')';
      Str:= FormatFloat('#,###,###,###,##0.##################', TV)+' '+UN+' ('+TU.Suffix+')';
      I:= lstEquivalents.Items.Add;
      I.Caption:= Str;
      I.Data:= TU;
    end;
  finally
    L.Free;
  end;
end;

procedure TfrmJDConvertMain.cboConvertCategoryClick(Sender: TObject);
begin
  RefreshConvert;
end;

procedure TfrmJDConvertMain.cboConvertFromUnitClick(Sender: TObject);
begin
  RefreshEquivalents;
end;

end.
