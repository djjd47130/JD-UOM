program MathExprTest;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Vcl.Forms,
  uMathExprTestMain in 'uMathExprTestMain.pas' {frmExprTest},
  JD.Uom.Expr.DWS in '..\Source\JD.Uom.Expr.DWS.pas' {dmDWS: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExprTest, frmExprTest);
  Application.Run;
end.
