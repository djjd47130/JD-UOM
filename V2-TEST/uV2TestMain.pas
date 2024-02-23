unit uV2TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JD.Uom, Vcl.StdCtrls,
  JD.Uom.Length, JD.Uom.Area;

type
  TfrmMain = class(TForm)
    UOMs: TUOM;
    lstUOMs: TListBox;
    lstUnits: TListBox;
    Label1: TLabel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure lstUOMsClick(Sender: TObject);
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

procedure TfrmMain.RefreshUOMs;
var
  X: Integer;
  U: TUOMBaseClass;
begin
  lstUOMs.Items.Clear;
  for X := 0 to TUOMList.Count-1 do begin
    U:= TUOMList.UOM(X);
    lstUOMs.Items.Add(U.UOMName);
  end;
end;

procedure TfrmMain.RefreshUnits;
var
  I: Integer;
  UOM: TUOMBaseClass;
  U: TUOMUnitInfo;
  X: Integer;
begin
  lstUnits.Items.Clear;
  I:= lstUOMs.ItemIndex;
  if I < 0 then Exit;
  UOM:= TUOMList.UOM(I);
  for X := 0 to UOM.UnitCount-1 do begin
    U:= UOM.GetUnit(X);
    lstUnits.Items.Add(U.Name);
  end;
end;

initialization

end.
