program MathExprTest;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Vcl.Forms,
  uMathExprTestMain in 'uMathExprTestMain.pas' {frmExprTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmExprTest, frmExprTest);
  Application.Run;
end.
