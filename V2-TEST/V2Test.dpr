program V2Test;

uses
  Vcl.Forms,
  uV2TestMain in 'uV2TestMain.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.Title := 'JD Unit of Measure Test';
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
