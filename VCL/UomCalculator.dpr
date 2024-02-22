program UomCalculator;

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'UOM Calculator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
