unit uV2TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JD.Uom, Vcl.StdCtrls,
  JD.Uom.Length,
  JD.Uom.Area,
  JD.Uom.Temperature, Vcl.ExtCtrls;

type
  TfrmMain = class(TForm)
    lstUOMs: TListBox;
    lstUnits: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    cboSystem: TComboBox;
    pUnitDetail: TPanel;
    lblUnitHeader: TLabel;
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
    procedure FormCreate(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure lstUnitsClick(Sender: TObject);
  private
    FUOM: TUOMBaseClass;
  public
    procedure RefreshUOMs;
    procedure RefreshUnits;
    procedure RefreshUnitDetails;
  end;

var
  frmMain: TfrmMain;

implementation

{$R *.dfm}

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  {$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown:= True;
  {$ENDIF}
  RefreshUOMs;
end;

procedure TfrmMain.lstUOMsClick(Sender: TObject);
begin
  RefreshUnits;
end;

procedure TfrmMain.cboSystemClick(Sender: TObject);
begin
  RefreshUnits;
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
end;

procedure TfrmMain.RefreshUnits;
var
  I: Integer;
  U: TUOMUnitBaseClass;
  X: Integer;
  S: TUOMSystem;
begin
  lstUnits.Items.Clear;
  I:= lstUOMs.ItemIndex;
  if I < 0 then Exit;
  FUOM:= TUOMUtils.UOM(I);
  for X := 0 to FUOM.UnitCount-1 do begin
    U:= FUOM.GetUnit(X);
    S:= TUOMSystem(cboSystem.ItemIndex);
    if (S = ustAny) or (S in U.Systems) then
      lstUnits.Items.AddObject(U.NamePlural, Pointer(X));
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
  U: TUOMUnitBaseClass;
  I: Integer;
begin
  I:= Integer(lstUnits.Items.Objects[lstUnits.ItemIndex]);
  U:= FUOM.GetUnit(I);
  lblUnitName.Caption:= U.NamePlural;
  lblUnitID.Caption:= U.UnitID;
  lblUnitSystems.Caption:= UOMSystemsStr(U.Systems);
  lblUnitPrefix.Caption:= U.Prefix;
  lblUnitSuffix.Caption:= U.Suffix;
  lblUnitBaseFrom.Caption:= FormatFloat(NumFormat, U.ConvertFromBase(1))+U.Suffix;
  lblUnitBaseTo.Caption:= FormatFloat(NumFormat, U.ConvertToBase(1))+U.Suffix;
  //TODO: Show unit details...

end;

end.
