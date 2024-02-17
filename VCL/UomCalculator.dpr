program UomCalculator;

uses
  Vcl.Forms,
  uTestMain in 'uTestMain.pas' {frmMain},
  Vcl.Themes,
  Vcl.Styles,
  JD.Uom.Angle in '..\JD.Uom.Angle.pas',
  JD.Uom.Area in '..\JD.Uom.Area.pas',
  JD.Uom.Capacitance in '..\JD.Uom.Capacitance.pas',
  JD.Uom.Common in '..\JD.Uom.Common.pas',
  JD.Uom.Current in '..\JD.Uom.Current.pas',
  JD.Uom.Data in '..\JD.Uom.Data.pas',
  JD.Uom.Density in '..\JD.Uom.Density.pas',
  JD.Uom.Energy in '..\JD.Uom.Energy.pas',
  JD.Uom.Gravity in '..\JD.Uom.Gravity.pas',
  JD.Uom.Length in '..\JD.Uom.Length.pas',
  JD.Uom.Power in '..\JD.Uom.Power.pas',
  JD.Uom.Pressure in '..\JD.Uom.Pressure.pas',
  JD.Uom.Radiation in '..\JD.Uom.Radiation.pas',
  JD.Uom.Resistance in '..\JD.Uom.Resistance.pas',
  JD.Uom.Speed in '..\JD.Uom.Speed.pas',
  JD.Uom.Temperature in '..\JD.Uom.Temperature.pas',
  JD.Uom.Time in '..\JD.Uom.Time.pas',
  JD.Uom.Voltage in '..\JD.Uom.Voltage.pas',
  JD.Uom.Volume in '..\JD.Uom.Volume.pas',
  JD.Uom.Weight in '..\JD.Uom.Weight.pas',
  JD.UomCtrls in '..\JD.UomCtrls.pas',
  JD.UomUtils in '..\JD.UomUtils.pas',
  JD.Uom.Resolution in '..\JD.Uom.Resolution.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Windows10 Dark');
  Application.Title := 'UOM Calculator';
  Application.CreateForm(TfrmMain, frmMain);
  Application.Run;
end.
