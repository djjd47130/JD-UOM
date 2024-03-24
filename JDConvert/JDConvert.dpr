program JDConvert;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Vcl.Forms,
  uJDConvertMain in 'uJDConvertMain.pas' {frmJDConvertMain},
  Vcl.Themes,
  Vcl.Styles,
  JD.Uom.Expr.DWS in '..\Source\JD.Uom.Expr.DWS.pas' {dmDWS: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.Title := 'JD Unit of Measure Conversion';
  Application.HelpFile := '';
  Application.CreateForm(TfrmJDConvertMain, frmJDConvertMain);
  Application.Run;
end.
