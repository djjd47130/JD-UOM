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
    lblUnitName: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
    procedure cboSystemClick(Sender: TObject);
    procedure lstUnitsClick(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RefreshUOMs;
    procedure RefreshUnits;
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
var
  U: TUOMUnitBase;
begin
  //TODO: Show unit details...
  //U:= ???
  //lblUnitName.Caption:= U.Name;

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
  UOM: TUOMBaseClass;
  U: TUOMUnitBaseClass;
  X: Integer;
  S: TUOMSystem;
begin
  lstUnits.Items.Clear;
  I:= lstUOMs.ItemIndex;
  if I < 0 then Exit;
  UOM:= TUOMUtils.UOM(I);
  for X := 0 to UOM.UnitCount-1 do begin
    U:= UOM.GetUnit(X);
    S:= TUOMSystem(cboSystem.ItemIndex);
    if (S = ustAny) or (S in U.Systems) then
      lstUnits.Items.Add(U.UnitName);
  end;
end;

end.
