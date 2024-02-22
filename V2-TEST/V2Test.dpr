program V2Test;

uses
  Vcl.Forms,
  uV2TestMain in 'uV2TestMain.pas' {Form1},
  JD.Uom.Length in '..\Source\JD.Uom.Length.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
