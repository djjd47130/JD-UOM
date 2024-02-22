unit uV2TestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, JD.Uom, Vcl.StdCtrls,
  JD.Uom.Length;

type
  TForm1 = class(TForm)
    UOMs: TUOM;
    lstUOMs: TListBox;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    procedure RefreshUOMs;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  RefreshUOMs;
end;

procedure TForm1.RefreshUOMs;
var
  X: Integer;
  U: TUOMUtilsBaseClass;
begin
  lstUOMs.Items.Clear;
  for X := 0 to TUOMList.Count-1 do begin
    U:= TUOMList.UOM(X);
    lstUOMs.Items.Add(U.UOMName);
  end;
end;

initialization

end.
