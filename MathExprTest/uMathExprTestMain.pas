unit uMathExprTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  dwsCompiler, dwsExprs, dwsComp;

type
  TForm1 = class(TForm)
    txtExpr: TEdit;
    txtResult: TEdit;
    Button1: TButton;
    DWS: TDelphiWebScript;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  Prog: IdwsProgram;
  Exec: IdwsProgramExecution;
begin
  Prog:= DWS.Compile('PrintLn('+txtExpr.Text+');');
  if prog.Msgs.Count > 0 then
    txtResult.Text:= prog.Msgs.AsInfo
  else begin
    exec:= prog.Execute;
    txtResult.Text:= exec.Result.ToString;
    if exec.Msgs.HasErrors then begin
      raise Exception.Create(exec.Msgs.AsInfo);
    end;
  end;

end;

end.
