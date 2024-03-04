program JDConvert;

uses
  Vcl.Forms,
  uJDConvertMain in 'uJDConvertMain.pas' {frmJDConvertMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'JD Unit of Measure Conversion';
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TfrmJDConvertMain, frmJDConvertMain);
  Application.Run;
end.
