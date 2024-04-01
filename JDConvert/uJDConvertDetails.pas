unit uJDConvertDetails;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus,
  Vcl.ComCtrls, Vcl.StdCtrls, VCLTee.TeEngine, VCLTee.Series, Vcl.Mask, RzEdit,
  RzSpnEdt, VCLTee.TeeProcs, VCLTee.Chart, Vcl.ExtCtrls;

type
  TfrJDConvertDetails = class(TFrame)
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
    Label14: TLabel;
    lblUnitAliases: TLabel;
    pSystems: TPanel;
    Label12: TLabel;
    lstSystems: TListView;
    txtChartFreq: TRzSpinEdit;
    procedure lstSystemsClick(Sender: TObject);
    procedure lstSystemsItemChecked(Sender: TObject; Item: TListItem);
    procedure lstUOMsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure lstCategoriesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure chkNegativeClick(Sender: TObject);
    procedure txtChartScaleChange(Sender: TObject);
  private
    FSelSystems: String;
    FSelCategory: String;
    FSelUOM: String;
  public
    procedure RefreshUOMSystemList;
    procedure RefreshUOMCategoryList;
    procedure RefreshUOMList;
    procedure RefreshUOMDetails;
    procedure RefreshChart;
    procedure UpdateChart;
  end;

implementation

{$R *.dfm}

uses
  JD.Uom;

procedure TfrJDConvertDetails.chkNegativeClick(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrJDConvertDetails.lstCategoriesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  RefreshUOMList;
  RefreshChart;
end;

procedure TfrJDConvertDetails.lstSystemsClick(Sender: TObject);
begin
  if lstSystems.Selected <> nil then
    lstSystems.Selected.Checked:= not lstSystems.Selected.Checked;
end;

procedure TfrJDConvertDetails.lstSystemsItemChecked(Sender: TObject; Item: TListItem);
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
  RefreshUOMList;
  RefreshChart;
end;

procedure TfrJDConvertDetails.lstUOMsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if lstUOMs.Selected <> nil then
    FSelUOM:= lstUOMs.Selected.Caption
  else
    FSelUOM:= '';
  RefreshUOMDetails;
  UpdateChart;
end;

const
  WIDTH_SMALL = 3;
  WIDTH_LARGE = 7;
  WIDTH_CROSSHAIR = 1;

procedure TfrJDConvertDetails.RefreshChart;
var
  X: Integer;
  S: TLineSeries;
  U: TUOM;
  BU: TUOM;
  Y: Integer;
  V: UOMNum;
  Amt: Integer;
  Freq: UOMNum;
  Start: Integer;
begin
  Chart.SeriesList.Clear;
  Chart.Invalidate;
  Screen.Cursor:= crHourglass;
  try
    Application.ProcessMessages;
    if lstUOMs.Items.Count <= 0 then Exit;

    Amt:= Round(txtChartScale.Value);
    Freq:= txtChartFreq.Value;

    BU:= TUOMUtils.GetBaseUOM(FSelCategory);
    if Assigned(BU) then begin
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
            V:= V * Freq;
            S.Add(V, IntToStr(Y));
          end;
        finally
          Chart.AddSeries(S);
        end;
      end;

    end;
    UpdateChart;
    Chart.Invalidate;
  finally
    Screen.Cursor:= crDefault;
  end;
end;

procedure TfrJDConvertDetails.RefreshUOMCategoryList;
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

procedure TfrJDConvertDetails.RefreshUOMDetails;
var
  U: TUOM;
begin
  if lstUOMs.ItemIndex < 0 then Exit;
  U:= TUOMUtils.GetUOMByName(FSelUOM);
  lblUnitName.Caption:= U.NameSingular;
  lblUnitNamePlural.Caption:= U.NamePlural;
  lblUnitSystems.Caption:= U.Systems.DelimitedText;
  lblUnitSuffix.Caption:= U.Suffix;
  lblUnitAliases.Caption:= U.AllAliases;
  lblUnitBaseFrom.Caption:= U.ConvertFromBaseFormula;
  lblUnitBaseTo.Caption:= U.ConvertToBaseFormula;
end;

procedure TfrJDConvertDetails.RefreshUOMList;
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

procedure TfrJDConvertDetails.RefreshUOMSystemList;
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

procedure TfrJDConvertDetails.txtChartScaleChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrJDConvertDetails.UpdateChart;
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

end.
