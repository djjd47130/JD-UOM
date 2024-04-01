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
  Vcl.HtmlHelpViewer, Vcl.AppEvnts,

  JD.Common, JD.Graphics, JD.FontGlyphs, JD.Ctrls, JD.Ctrls.FontButton

  , JD.Uom
  , JD.Uom.Files
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

  , SynEditHighlighter, SynEditCodeFolding, SynHighlighterPas,
  SynEdit,

  uJDConvertScripting, uJDConvertDetails, System.Actions, Vcl.ActnList;

const
  WIDTH_SMALL = 2;
  WIDTH_LARGE = 6;
  WIDTH_CROSSHAIR = 1;

type
  TfrmJDConvertMain = class(TForm)
    Pages: TPageControl;
    tabConvert: TTabSheet;
    tabDetails: TTabSheet;
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
    AppEvents: TApplicationEvents;
    Panel4: TPanel;
    Label5: TLabel;
    txtSearch: TSearchBox;
    Panel5: TPanel;
    lblSearchFound: TLabel;
    btnSettings: TJDFontButton;
    tabSettings: TTabSheet;
    Panel6: TPanel;
    Label15: TLabel;
    Edit1: TEdit;
    JDFontButton1: TJDFontButton;
    btnUOMScript: TJDFontButton;
    tabScripts: TTabSheet;
    Scripting: TfrJDConvertScripting;
    UOMDetails: TfrJDConvertDetails;
    btnTESTIGNORE: TJDFontButton;
    Glyphs: TJDFontGlyphs;
    Img32: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure cboConvertFromUnitClick(Sender: TObject);
    procedure txtConvertFromValueChange(Sender: TObject);
    procedure lstEquivalentsDblClick(Sender: TObject);
    procedure cboConvertCategoryClick(Sender: TObject);
    procedure btnConvertNormalClick(Sender: TObject);
    procedure btnConvertSearchClick(Sender: TObject);
    procedure btnDetailsClick(Sender: TObject);
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
    function AppEventsHelp(Command: Word; Data: NativeInt;
      var CallHelp: Boolean): Boolean;
    procedure txtSearchInvokeSearch(Sender: TObject);
    procedure btnSettingsClick(Sender: TObject);
    procedure btnUOMScriptClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnDeleteUOMClick(Sender: TObject);
    procedure UOMDetailstxtChartScaleChange(Sender: TObject);
  private
    FCurMode: String;
    FSystemUOMs: TUOMFile;
    FUserUOMs: TUOMFile;
    FEditingUOM: Boolean;
    FIsNewUOM: Boolean;
    FSelUserItem: TListItem;
    FFoundVal: UOMNum;
    FFoundUOM: TUOM;
    procedure CloseHelpWnd;
    procedure MenuButtonSelected(const AModeName: String; AButton: TJDFontButton; ATab: TTabSheet;
      const AHelpContext: Integer = 0);
    procedure ShowSearchResult(const Value: UOMNum; UOM: TUOM);
    procedure UpdateTitle;
  public
    //Common
    procedure ProtectSystemUOMs;
    procedure RefreshAll;

    //Main Menu Related
    procedure ResetMenuButtons;

    //UOM Convert Related
    procedure RefreshConvert;
    procedure RefreshEquivalents;

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

{ TfrmJDConvertMain }

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
  UOMDetails.Chart.Align:= alClient;

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

procedure TfrmJDConvertMain.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  SaveUserUOMs;
end;

procedure TfrmJDConvertMain.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if Scripting.Modified then begin
    case MessageDlg('Save changes to script?', mtWarning, [mbYes,mbNo,mbCancel], 0) of
      mrYes: begin
        CanClose:= Scripting.Save;
      end;
      mrNo: begin
        CanClose:= True;
      end;
      else begin
        CanClose:= False;
        Self.btnUOMScriptClick(nil);
      end;
    end;
  end;
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

function TfrmJDConvertMain.AppEventsHelp(Command: Word;
  Data: NativeInt; var CallHelp: Boolean): Boolean;
begin
  CloseHelpWnd;
  Result := ShellExecute(0,'open','hh.exe', PWideChar('-mapid '+IntToStr(Data)
    +' ms-its:'+Application.HelpFile), nil,SW_SHOW) = 32;
  CallHelp := false;
end;






//Global across application

procedure TfrmJDConvertMain.ResetMenuButtons;
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

procedure TfrmJDConvertMain.MenuButtonSelected(const AModeName: String; AButton: TJDFontButton; ATab: TTabSheet;
  const AHelpContext: Integer = 0);
begin
  //TODO: Prevent change if actively editing something...
  FCurMode:= AModeName;
  Pages.ActivePage:= ATab;
  ResetMenuButtons;
  AButton.Image.StandardColor:= fcOrange;
  AButton.DrawStyle:= fdThemed;
  Self.HelpContext:= AHelpContext;
  Self.UpdateTitle;
  if ATab = Self.tabScripts then begin
    Scripting.Acts.State:= asNormal;
  end else begin
    Scripting.Acts.State:= asSuspended;
  end;
end;

procedure TfrmJDConvertMain.ProtectSystemUOMs;
var
  X: Integer;
  U: TUOM;
begin
  for X := 0 to TUOMUtils.UOMCount-1 do begin
    U:= TUOMUtils.GetUOMByIndex(X);
    U.ProtectedUOM:= True;
  end;
end;

procedure TfrmJDConvertMain.btnConvertNormalClick(Sender: TObject);
begin
  MenuButtonSelected('Convert', btnConvertNormal, tabConvert, 1002);
  pConvertNormal.Visible:= True;
  pConvertSearch.Visible:= False;
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.btnConvertSearchClick(Sender: TObject);
begin
  MenuButtonSelected('Search', btnConvertSearch, tabConvert, 1003);
  pConvertSearch.Visible:= True;
  pConvertNormal.Visible:= False;
  txtSearch.SetFocus;
  RefreshEquivalents;
end;

procedure TfrmJDConvertMain.btnDeleteUOMClick(Sender: TObject);
var
  I: TUOMFileItem;
begin
  if MessageDlg('Are you sure you wish to delete selected UOM?', mtWarning,
    [mbYes,mbNo], 0) = mrYes then
  begin
    I:= TUOMFileItem(lstCustomUOMs.Selected.Data);
    I.UnregisterUOM;
    FUserUOMs.DeleteItem(FUserUOMs.IndexOf(I));
    FUserUOMs.Save;
    Self.SetUserEditMode(False, False);
    Self.RefreshAll;
    Self.RefreshUserUOMList;
  end;
end;

procedure TfrmJDConvertMain.btnDetailsClick(Sender: TObject);
begin
  MenuButtonSelected('UOM Details', btnDetails, tabDetails, 1004);
end;

procedure TfrmJDConvertMain.btnUOMBuilderClick(Sender: TObject);
begin
  MenuButtonSelected('UOM Builder', btnUOMBuilder, tabBuilder, 1005);
end;

procedure TfrmJDConvertMain.btnSettingsClick(Sender: TObject);
begin
  MenuButtonSelected('Settings', btnSettings, tabSettings, 1006);
end;

procedure TfrmJDConvertMain.btnUOMScriptClick(Sender: TObject);
begin
  MenuButtonSelected('UOM Scripting', btnUOMScript, tabScripts, 1007);
  Scripting.UpdateActions;
end;

procedure TfrmJDConvertMain.RefreshAll;
begin
  TUOMUtils.ListCategories(cboConvertCategory.Items);

  UOMDetails.RefreshUOMSystemList;
  UOMDetails.RefreshUOMCategoryList;

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
    ProtectSystemUOMs;
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

procedure TfrmJDConvertMain.UOMDetailstxtChartScaleChange(Sender: TObject);
begin
  UOMDetails.RefreshChart;
end;

procedure TfrmJDConvertMain.UpdateTitle;
begin
  Caption:= 'JD Unit-of-Measure Conversion - '+FCurMode;
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














//UOM Builder

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

procedure TfrmJDConvertMain.btnCancelUOMClick(Sender: TObject);
begin
  FEditingUOM:= False;
  SetUserEditMode(False);
  ShowUserUOMDetails;
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

  //Protected UOMs...
  if TUOMUtils.CategoryProtected(txtUserCategory.Text) = True then begin
    case T of
      utMetric: begin
        if txtUserNamePlural.Text <> '' then
          raise Exception.Create('Cannot change base of protected category.');
      end;
      utSimple, utFormula: begin
        if chkUserBase.IsOn then
          raise Exception.Create('Cannot change base of protected category.');
      end;
    end;
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

end.
