unit uJDConvertMain;

interface

{$DEFINE TABLE_BASED}

uses
  Winapi.Windows, Winapi.Messages,
  System.Generics.Collections, System.Generics.Defaults, System.UITypes,
  System.SysUtils, System.Variants, System.Classes, System.Types,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.CheckLst, Vcl.Mask,
  Vcl.ComCtrls, System.ImageList, Vcl.ImgList, Vcl.WinXCtrls,
  ShellAPI,
  RzEdit, RzSpnEdt,
  VclTee.TeeGDIPlus, VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
  Vcl.HtmlHelpViewer,
  JD.Common, JD.Graphics, JD.FontGlyphs, JD.Ctrls, JD.Ctrls.FontButton,

  JD.Uom,
  JD.Uom.Files, Vcl.AppEvnts


  , JD.Uom.Distance
  , JD.Uom.Area
  , JD.Uom.Temperature
  , JD.Uom.Volume
  , JD.Uom.Mass
  , JD.Uom.Time
  , JD.Uom.Frequency
  , JD.Uom.Speed
  , JD.Uom.Numbers
  , JD.Uom.Data

  ;

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
    Label9: TLabel;
    lstEquivalents: TListView;
    pMenu: TPanel;
    btnConvertNormal: TJDFontButton;
    btnConvertSearch: TJDFontButton;
    btnDetails: TJDFontButton;
    pConvertNormal: TPanel;
    Panel1: TPanel;
    lblConvertTitle: TLabel;
    cboConvertCategory: TComboBox;
    pConvertSearch: TPanel;
    Panel3: TPanel;
    Label10: TLabel;
    txtConvertFromValue: TRzSpinEdit;
    Stat: TStatusBar;
    Label13: TLabel;
    cboConvertFromUnit: TComboBox;
    tabBuilder: TTabSheet;
    btnUOMBuilder: TJDFontButton;
    lstCustomUOMs: TListView;
    pEditUOM: TPanel;
    lblUserType: TLabel;
    cboUserType: TComboBox;
    lblUserNameSingular: TLabel;
    txtUserNameSingular: TEdit;
    lblUserNamePlural: TLabel;
    txtUserNamePlural: TEdit;
    lblUserCategory: TLabel;
    txtUserCategory: TEdit;
    lblUserSuffix: TLabel;
    txtUserSuffix: TEdit;
    lblUserSystems: TLabel;
    txtUserSystems: TEdit;
    Panel2: TPanel;
    btnNewUOM: TJDFontButton;
    btnEditUOM: TJDFontButton;
    btnDeleteUOM: TJDFontButton;
    btnSaveUOM: TJDFontButton;
    btnCancelUOM: TJDFontButton;
    lblUserFrom: TLabel;
    txtUserFrom: TEdit;
    lblUserTo: TLabel;
    txtUserTo: TEdit;
    lstUserUnits: TCheckListBox;
    chkUserBase: TToggleSwitch;
    lblUserBase: TLabel;
    ApplicationEvents1: TApplicationEvents;
    Panel4: TPanel;
    Label5: TLabel;
    txtSearch: TSearchBox;
    Panel5: TPanel;
    lblSearchFound: TLabel;
    procedure FormCreate(Sender: TObject);
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
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnUOMBuilderClick(Sender: TObject);
    procedure btnNewUOMClick(Sender: TObject);
    procedure cboUserTypeClick(Sender: TObject);
    procedure lstCustomUOMsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure btnCancelUOMClick(Sender: TObject);
    procedure btnEditUOMClick(Sender: TObject);
    procedure btnSaveUOMClick(Sender: TObject);
    function ApplicationEvents1Help(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure txtSearchInvokeSearch(Sender: TObject);
  private
    FSelSystems: String;
    FSelCategory: String;
    FSelUOM: String;
    FSystemUOMs: TUOMFile;
    FUserUOMs: TUOMFile;
    FEditingUOM: Boolean;
    FIsNewUOM: Boolean;
    FSelUserItem: TListItem;
    FFoundVal: UOMNum;
    FFoundUOM: TUOM;
    procedure CloseHelpWnd;
    procedure MenuButtonSelected(AButton: TJDFontButton; ATab: TTabSheet;
      const AHelpContext: Integer = 0);
    procedure ShowSearchResult(const Value: UOMNum; UOM: TUOM);
  public
    //Common
    procedure RefreshAll;

    //Main Menu Related
    procedure ResetButtonColors;

    //UOM Convert Related
    procedure RefreshConvert;
    procedure RefreshEquivalents;

    //UOM Details Related
    procedure RefreshUOMSystemList;
    procedure RefreshUOMCategoryList;
    procedure RefreshUOMList;
    procedure RefreshUOMDetails;
    procedure RefreshChart;
    procedure UpdateChart;

    //UOM Builder Related
    function SystemUOMPath: String;
    function UserUOMFilename: String;
    procedure LoadSystemUOMs;
    procedure LoadUserUOMs;
    procedure SaveUserUOMs;
    procedure LoadUserUnits;
    function GetUserUnits: String;
    procedure SetUserUnits(const Value: String);
    procedure RefreshUserUOMList;
    procedure UpdateUserUOMActions;
    procedure ClearUserUOM;
    procedure SetUserTypeMode(const AType: TUOMFileItemType);
    procedure ShowUserUOMControls(const AVisible: Boolean);
    procedure ShowUserUOMDetails;
    procedure SetUserEditMode(const Editing: Boolean; const IsNew: Boolean = False);

  end;

var
  frmJDConvertMain: TfrmJDConvertMain;

implementation

{$R *.dfm}

uses
  System.IOUtils,
  ClipBrd;

{ TfrmJDConvertMain }

procedure TfrmJDConvertMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveUserUOMs;
end;

procedure TfrmJDConvertMain.FormCreate(Sender: TObject);
var
  X: Integer;
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}

  //NOTE: Putting this in the initialization section does not work properly!
  Application.HelpFile:= TPath.Combine(ExtractFilePath(ParamStr(0)), 'JD Convert Help.chm');

  FSystemUOMs:= TUOMFile.Create(nil);
  FUserUOMs:= TUOMFile.Create(nil);
  FUserUOMs.Filename:= UserUOMFilename;
  ColorManager.BaseColor:= clBlack;
  pConvert.Align:= alClient;
  Chart.Align:= alClient;

  LoadSystemUOMs;
  LoadUserUnits;
  LoadUserUOMs;
  RefreshAll;

  for X := 0 to Pages.PageCount-1 do
    Pages.Pages[X].TabVisible:= False;
  btnConvertNormalClick(nil);
end;

procedure TfrmJDConvertMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FUserUOMs);
  FreeAndNil(FSystemUOMs);
end;

procedure TfrmJDConvertMain.CloseHelpWnd;
var
  HlpWind: HWND;
const
  HelpTitle = 'JD Convert Help';
begin
  HlpWind := FindWindow('HH Parent',HelpTitle);
  if HlpWind <> 0 then PostMessage(HlpWind,WM_Close,0,0);
end;

function TfrmJDConvertMain.ApplicationEvents1Help(Command: Word;
  Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
  CloseHelpWnd;
  Result := ShellExecute(0,'open','hh.exe', PWideChar('-mapid '+IntToStr(Data)
    +' ms-its:'+Application.HelpFile), nil,SW_SHOW) = 32;
  CallHelp := false;
end;






//Global across application

procedure TfrmJDConvertMain.ResetButtonColors;
var
  X: Integer;
begin
  for X := 0 to pMenu.ControlCount-1 do begin
    if pMenu.Controls[X] is TJDFontButton then begin
      TJDFontButton(pMenu.Controls[X]).Image.StandardColor:= fcBlue;
      TJDFontButton(pMenu.Controls[X]).DrawStyle:= fdTransparent;
    end;
  end;
end;

procedure TfrmJDConvertMain.MenuButtonSelected(AButton: TJDFontButton; ATab: TTabSheet;
  const AHelpContext: Integer = 0);
begin
  Pages.ActivePage:= ATab;
  ResetButtonColors;
  AButton.Image.StandardColor:= fcOrange;
  AButton.DrawStyle:= fdThemed;
  Self.HelpContext:= AHelpContext;
end;

procedure TfrmJDConvertMain.btnConvertNormalClick(Sender: TObject);
begin
  MenuButtonSelected(btnConvertNormal, tabConvert, 1002);
  pConvertNormal.Visible:= True;
  pConvertSearch.Visible:= False;
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.btnConvertSearchClick(Sender: TObject);
begin
  MenuButtonSelected(btnConvertSearch, tabConvert, 1003);
  pConvertSearch.Visible:= True;
  pConvertNormal.Visible:= False;
  txtSearch.SetFocus;
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.btnDetailsClick(Sender: TObject);
begin
  MenuButtonSelected(btnDetails, tabDetails, 1004);
end;

procedure TfrmJDConvertMain.btnUOMBuilderClick(Sender: TObject);
begin
  MenuButtonSelected(btnUOMBuilder, tabBuilder, 1005);
end;

procedure TfrmJDConvertMain.RefreshAll;
begin
  RefreshUOMSystemList;
  RefreshUOMCategoryList;
  TUOMUtils.ListCategories(cboConvertCategory.Items);
  if cboConvertCategory.Items.Count > 0 then
    cboConvertCategory.ItemIndex:= 0;
  RefreshConvert;
  Stat.Panels[0].Text:= IntToStr(TUOMUtils.UOMCount) + ' UOMs Registered';
end;

procedure TfrmJDConvertMain.LoadSystemUOMs;
var
  Files: TStringDynArray;
  X: Integer;
begin
  try
    if DirectoryExists(SystemUOMPath) then begin
      Files:= TDirectory.GetFiles(SystemUOMPath, '*.ini');
      for X := 0 to Length(Files)-1 do begin
        FSystemUOMs.LoadFromFile(Files[X]).RegisterAllUOMs;
      end;
    end;
  except
    on E: Exception do begin
      MessageDlg('Failed to load system UOMs: '+E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure OpenWebPageInDefaultBrowser(const URL: string);
begin
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
end;

procedure TfrmJDConvertMain.StatDblClick(Sender: TObject);
begin
  OpenWebPageInDefaultBrowser('https://github.com/djjd47130/JD-UOM');
end;

function TfrmJDConvertMain.SystemUOMPath: String;
begin
  Result:= ExtractFilePath(ParamStr(0));
  Result:= TPath.Combine(Result, 'System');
end;






//Convert Regular Mode

procedure TfrmJDConvertMain.txtConvertFromValueChange(Sender: TObject);
begin
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.ShowSearchResult(const Value: UOMNum; UOM: TUOM);
var
  S: String;
begin
  if Assigned(UOM) then begin
    S:= UOM.Category+': '+FormatFloat(NumFormat, Value) + ' ';
    if Value = 1 then
      S:= S + UOM.NameSingular
    else
      S:= S + UOM.NamePlural;
    S:= S + ' ('+UOM.Suffix+')';
    lblSearchFound.Caption:= S;
    lblSearchFound.Font.Color:= ColorManager.Color[fcGreen];
  end else begin
    lblSearchFound.Caption:= 'UOM not found or invalid search criteria!';
    lblSearchFound.Font.Color:= ColorManager.Color[fcRed];
  end;
end;

procedure TfrmJDConvertMain.txtSearchInvokeSearch(Sender: TObject);
var
  S: String;
  Val: UOMNum;
  Suf: String;
  U: TUOM;
begin
  lstEquivalents.Items.Clear;
  S:= txtSearch.Text;
  TUOMUtils.ParseSuffix(S, Val, Suf);
  U:= TUOMUtils.FindUOM(Suf);
  if U <> nil then begin
    FFoundVal:= Val;
    FFoundUOM:= U;
    RefreshEquivalents;
  end;
  ShowSearchResult(Val, U);
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

procedure TfrmJDConvertMain.RefreshEquivalents;
var
  L: TStringList;
  X: Integer;
  FU, TU: TUOM;
  FV, TV: UOMNum;
  Str: String;
  UN: String;
  I: TListItem;
begin

  //TODO: Handle EITHER regular mode OR search mode...

  lstEquivalents.Items.Clear;

  L:= TStringList.Create;
  try
    if Self.pConvertSearch.Visible then begin
      FU:= FFoundUOM;
      FV:= FFoundVal;
      if FU = nil then Exit;
      TUOMUtils.ListUOMs(L, FU.Category);
    end else begin
      FU:= TUOMUtils.GetUOMByName(cboConvertFromUnit.Text);
      FV:= txtConvertFromValue.Value;
      if FU = nil then Exit;
      TUOMUtils.ListUOMs(L, cboConvertCategory.Text);
    end;

    for X := 0 to L.Count-1 do begin
      TU:= TUOMUtils.GetUOMByName(L[X]);
      TV:= TUOMUtils.Convert(FV, FU.NameSingular, TU.NameSingular);
      if TV = 1 then UN:= TU.NameSingular else UN:= TU.NamePlural;
      //Str:= FormatFloat(NumFormat, TV)+' '+UN+' ('+TU.Suffix+')';
      Str:= FormatFloat('#,###,###,###,##0.##################', TV); //+' '+UN+' ('+TU.Suffix+')';
      I:= lstEquivalents.Items.Add;
      I.Caption:= Str;
      I.Data:= TU;
      I.SubItems.Add(UN);
      I.SubItems.Add(TU.Suffix);
      I.SubItems.Add(TU.Systems.DelimitedText);
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

procedure TfrmJDConvertMain.lstEquivalentsDblClick(Sender: TObject);
var
  S: String;
begin
  //Value + Space + Suffix
  S:= lstEquivalents.Selected.Caption;
  S:= S + ' ' + lstEquivalents.Selected.SubItems[1];
  Clipboard.AsText:= S;
end;






//Convert Search Mode







//UOM Details Mode

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

procedure TfrmJDConvertMain.RefreshUOMSystemList;
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

procedure TfrmJDConvertMain.lstCategoriesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  RefreshUOMList;
  RefreshChart;
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
  RefreshUOMList;
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

procedure TfrmJDConvertMain.RefreshUOMCategoryList;
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

function CompareUOMVal(const A, B: TUOMValue): Integer;
var
  UA, UB: TUOM;
  VA, VB: UOMNum;
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

procedure TfrmJDConvertMain.RefreshUOMList;
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







//UOM Details Mode - Chart

procedure TfrmJDConvertMain.txtChartScaleChange(Sender: TObject);
begin
  RefreshChart;
end;

procedure TfrmJDConvertMain.RefreshChart;
var
  X: Integer;
  S: TLineSeries;
  U: TUOM;
  BU: TUOM;
  Y: Integer;
  V: UOMNum;
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

procedure TfrmJDConvertMain.chkNegativeClick(Sender: TObject);
begin
  RefreshChart;
end;







//UOM Builder

procedure TfrmJDConvertMain.btnCancelUOMClick(Sender: TObject);
begin
  FEditingUOM:= False;
  SetUserEditMode(False);
  ShowUserUOMDetails;
end;

procedure TfrmJDConvertMain.btnEditUOMClick(Sender: TObject);
begin
  SetUserEditMode(True);
end;

procedure TfrmJDConvertMain.btnNewUOMClick(Sender: TObject);
begin
  SetUserEditMode(True, True);
  ClearUserUOM;
  lblUserType.Visible:= True;
  cboUserType.Visible:= True;
end;

procedure TfrmJDConvertMain.btnSaveUOMClick(Sender: TObject);
var
  T: TUOMFileItemType;
  V: UOMNum;
  I: TUOMFileItem;
  UnitsChecked: Integer;
  X: Integer;
  procedure PopulateItem;
  begin
    I.ItemType:= T;
    I.Category:= txtUserCategory.Text;
    if T <> utMetric then
      I.IsBase:= chkUserBase.IsOn;
    I.NameSingular:= txtUserNameSingular.Text;
    if T = utMetric then
      I.BaseUOM:= txtUserNamePlural.Text
    else
      I.NamePlural:= txtUserNamePlural.Text;
    I.Suffix:= txtUserSuffix.Text;
    if T <> utMetric then
      I.Systems:= txtUserSystems.Text;
    if T = utSimple then begin
      I.Factor:= StrToFloatDef(txtUserFrom.Text, 0);
    end else
    if T = utFormula then begin
      I.FromBase:= txtUserFrom.Text;
      I.ToBase:= txtUserTo.Text;
    end;
    if T = utMetric then begin
      I.Include:= GetUserUnits;
    end;
  end;
begin
  if cboUserType.ItemIndex = -1 then
    raise Exception.Create('Please select item type.');
  T:= TUOMFileItemType(cboUserType.ItemIndex);
  if txtUserCategory.Text = '' then
    raise Exception.Create('Please enter a UOM category.');
  if txtUserNameSingular.Text = '' then
    raise Exception.Create('Please enter a UOM name.');
  if txtUserSuffix.Text = '' then
    raise Exception.Create('Please enter a UOM siffix.');
  if (txtUserSystems.Text = '') and (T <> utMetric) then
    raise Exception.Create('Please enter at least 1 UOM system.');
  if T = utSimple then begin
    if txtUserFrom.Text = '' then
      raise Exception.Create('Please enter a conversion factor.');
    V:= StrToFloatDef(txtUserFrom.Text, 0);
    if V = 0 then
      raise Exception.Create('Invalid value for conversion factor.');
  end else
  if T = utFormula then begin
    if txtUserFrom.Text = '' then
      raise Exception.Create('Please enter a formula to convert from base.');
    if txtUserTo.Text = '' then
      raise Exception.Create('Please enter a formula to convert to base.');
  end;
  if T = utMetric then begin
    UnitsChecked:= 0;
    for X := 0 to lstUserUnits.Items.Count-1 do begin
      if lstUserUnits.Checked[X] then
        Inc(UnitsChecked);
    end;
    if UnitsChecked = 0 then
      raise Exception.Create('Please choose at least 1 Metric unit.');
  end;

  if FIsNewUOM then begin
    I:= FUserUOMs.Add;
    PopulateItem;
    I.RegisterUOM;
  end else begin
    I:= TUOMFileItem(lstCustomUOMs.Selected.Data);
    PopulateItem;
  end;
  FUserUOMs.Save;
  Self.SetUserEditMode(False, False);
  Self.RefreshAll;
  Self.RefreshUserUOMList;

end;

procedure TfrmJDConvertMain.ClearUserUOM;
begin
  cboUserType.ItemIndex:= -1;
  txtUserCategory.Text:= '';
  chkUserBase.State:= tssOff;
  txtUserNameSingular.Text:= '';
  txtUserNamePlural.Text:= '';
  txtUserSuffix.Text:= '';
  txtUserSystems.Text:= '';
  txtUserFrom.Text:= '';
  txtUserTo.Text:= '';
  lstUserUnits.CheckAll(TCheckBoxState.cbUnchecked);
  ShowUserUOMControls(False);
end;

procedure TfrmJDConvertMain.SetUserTypeMode(const AType: TUOMFileItemType);
begin
  chkUserBase.Visible:= AType <> utMetric;
  lblUserBase.Visible:= AType <> utMetric;
  lblUserFrom.Visible:= AType <> utMetric;
  txtUserFrom.Visible:= AType <> utMetric;
  lblUserTo.Visible:= AType = utFormula;
  txtUserTo.Visible:= AType = utFormula;
  lblUserSystems.Visible:= AType <> utMetric;
  txtUserSystems.Visible:= AType <> utMetric;
  lstUserUnits.Visible:= AType = utMetric;
  case AType of
    utMetric: begin
      lblUserNamePlural.Caption:= 'Base UOM:';
    end;
    utSimple: begin
      lblUserNamePlural.Caption:= 'Name (Plural):';
      lblUserFrom.Caption:= 'Factor:';
    end;
    utFormula: begin
      lblUserNamePlural.Caption:= 'Name (Plural):';
      lblUserFrom.Caption:= 'Convert From Base:';
    end;
  end;
end;

procedure TfrmJDConvertMain.UpdateUserUOMActions;
begin
  //lstCustomUOMs.Enabled:= (not FEditingUOM);
  btnNewUOM.Enabled:= (not FEditingUOM);
  btnEditUOM.Enabled:= (not FEditingUOM) and (lstCustomUOMs.Selected <> nil);
  btnDeleteUOM.Enabled:= (not FEditingUOM) and (lstCustomUOMs.Selected <> nil);
  btnSaveUOM.Enabled:= FEditingUOM;
  btnCancelUOM.Enabled:= FEditingUOM;
end;

procedure TfrmJDConvertMain.lstCustomUOMsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
begin
  if FEditingUOM then begin
    if Item <> FSelUserItem then begin
      //TODO: Prevent change (need a better method other than Enabled=False)...
      if FSelUserItem <> nil then
        lstCustomUOMs.ItemIndex:= FSelUserItem.Index;
    end;
  end else begin
    FSelUserItem:= Item;
    ShowUserUOMDetails;
  end;
end;

procedure TfrmJDConvertMain.RefreshUserUOMList;
var
  X: Integer;
  Itm: TUOMFileItem;
  I: TListItem;
begin
  lstCustomUOMs.Items.Clear;
  for X := 0 to FUserUOMs.Count - 1 do begin
    Itm:= FUserUOMs[X];
    I:= lstCustomUOMs.Items.Add;
    I.Data:= Itm;
    I.Caption:= Itm.NameSingular;
    case Itm.ItemType of
      utMetric:   I.SubItems.Add('Metric');
      utSimple:   I.SubItems.Add('Simple');
      utFormula:  I.SubItems.Add('Formula');
    end;
    I.SubItems.Add(Itm.Category);
    I.SubItems.Add(Itm.Suffix);
    I.SubItems.Add(Itm.Systems);
  end;
  ShowUserUOMDetails;
  SetUserEditMode(False);
end;

procedure TfrmJDConvertMain.ShowUserUOMDetails;
var
  I: TUOMFileItem;
begin
  ClearUserUOM;
  ShowUserUOMControls(lstCustomUOMs.Selected <> nil);
  lblUserType.Visible:= (lstCustomUOMs.Selected <> nil);
  cboUserType.Visible:= (lstCustomUOMs.Selected <> nil);
  if lstCustomUOMs.Selected <> nil then begin
    I:= TUOMFileItem(lstCustomUOMs.Selected.Data);
    SetUserTypeMode(I.ItemType);
    cboUserType.ItemIndex:= Integer(I.ItemType);
    txtUserCategory.Text:= I.Category;
    if I.IsBase then
      chkUserBase.State:= tssOn
    else
      chkUserBase.State:= tssOff;
    txtUserNameSingular.Text:= I.NameSingular;
    if I.ItemType = utMetric then
      txtUserNamePlural.Text:= I.BaseUOM
    else
      txtUserNamePlural.Text:= I.NamePlural;
    txtUserSuffix.Text:= I.Suffix;
    txtUserSystems.Text:= I.Systems;
    if I.ItemType = utSimple then begin
      txtUserFrom.Text:= FormatFloat(NumInternalFormat, I.Factor);
    end else
    if I.ItemType = utFormula then begin
      txtUserFrom.Text:= I.FromBase;
      txtUserTo.Text:= I.ToBase;
    end;
    if I.ItemType = utMetric then begin
      SetUserUnits(I.Include);
    end;
  end;
  UpdateUserUOMActions;
end;

procedure TfrmJDConvertMain.SetUserEditMode(const Editing: Boolean; const IsNew: Boolean = False);
begin
  FEditingUOM:= Editing;
  FIsNewUOM:= IsNew;

  cboUserType.Enabled:= Editing;
  txtUserCategory.ReadOnly:= not Editing;
  chkUserBase.ReadOnly:= not Editing;
  txtUserNameSingular.ReadOnly:= not Editing;
  txtUserNamePlural.ReadOnly:= not Editing;
  txtUserSuffix.ReadOnly:= not Editing;
  txtUserSystems.ReadOnly:= not Editing;
  txtUserFrom.ReadOnly:= not Editing;
  txtUserTo.ReadOnly:= not Editing;
  lstUserUnits.Enabled:= Editing;

  UpdateUserUOMActions;
end;

procedure TfrmJDConvertMain.LoadUserUOMs;
begin
  try
    if FileExists(UserUOMFilename) then
      FUserUOMs.Load.RegisterAllUOMs;
    RefreshUserUOMList;
  except
    on E: Exception do begin
      MessageDlg('Failed to load custom UOMs: '+E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmJDConvertMain.SaveUserUOMs;
begin
  try
    FUserUOMs.Save;
  except
    on E: Exception do begin
      MessageDlg('Failed to save custom UOMs: '+E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TfrmJDConvertMain.ShowUserUOMControls(const AVisible: Boolean);
  procedure V(Ctrl: TControl);
  begin
    Ctrl.Visible:= AVisible;
  end;
begin
  V(lblUserCategory);
  V(txtUserCategory);
  V(lblUserBase);
  V(chkUserBase);
  V(lblUserNameSingular);
  V(txtUserNameSingular);
  V(lblUserNamePlural);
  V(txtUserNamePlural);
  V(lblUserSuffix);
  V(txtUserSuffix);
  V(lblUserSystems);
  V(txtUserSystems);
  V(lblUserFrom);
  V(txtUserFrom);
  V(lblUserTo);
  V(txtUserTo);
  V(lstUserUnits);
end;

procedure TfrmJDConvertMain.cboUserTypeClick(Sender: TObject);
begin
  if cboUserType.ItemIndex = -1 then begin
    //Nothing selected...
    ShowUserUOMControls(False);
  end else begin
    ShowUserUOMControls(True);
    SetUserTypeMode(TUOMFileItemType(cboUserType.ItemIndex));
  end;
end;

function TfrmJDConvertMain.GetUserUnits: String;
var
  X: Integer;
begin
  Result:= '';
  for X := 0 to lstUserUnits.Items.Count-1 do begin
    if lstUserUnits.Checked[X] then begin
      if Result <> '' then Result:= Result  + ',';
      Result:= Result + lstUserUnits.Items[X];
    end;
  end;
end;

procedure TfrmJDConvertMain.SetUserUnits(const Value: String);
var
  Units: TUOMMetricUnits;
  X: Integer;
  U: TUOMMetricUnit;
begin
  Units:= StringToMetricUnits(Value);
  for X := 0 to lstUserUnits.Items.Count-1 do begin
    U:= StringToMetricUnit(lstUserUnits.Items[X]);
    lstUserUnits.Checked[X]:= U in Units;
  end;
end;

function TfrmJDConvertMain.UserUOMFilename: String;
begin
  Result:= TPath.GetHomePath;
  Result:= TPath.Combine(Result, 'JD Software');
  Result:= TPath.Combine(Result, 'JD Convert');
  Result:= TPath.Combine(Result, 'User UOMs.ini');
end;

procedure TfrmJDConvertMain.LoadUserUnits;
var
  U: TUOMMetricUnit;
begin
  lstUserUnits.Items.Clear;
  for U := Low(TUOMMetricUnit) to High(TUOMMetricUnit) do begin
    lstUserUnits.Items.Add(MetricUnitToString(U));
  end;
end;

end.
