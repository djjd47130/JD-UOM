program JDConvert;

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  Vcl.Forms,
  uJDConvertMain in 'uJDConvertMain.pas' {frmJDConvertMain},
  Vcl.Themes,
  Vcl.Styles;

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
