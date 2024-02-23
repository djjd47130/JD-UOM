program V2Test;

uses
  Vcl.Forms,
  uV2TestMain in 'uV2TestMain.pas' {frmMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
