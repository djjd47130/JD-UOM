unit uMathExprTestMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls

  , JD.Uom.Expr
  , JD.Uom
  , JD.Uom.Distance
  , JD.Uom.Area
  , JD.Uom.Temperature
  , JD.Uom.Volume
  , JD.Uom.Mass
  , JD.Uom.Time
  , JD.Uom.Frequency
  , JD.Uom.Speed
  , JD.Uom.Numbers
  , JD.Uom.Data

  , Vcl.ExtCtrls

  ;

type
  TfrmExprTest = class(TForm)
    txtExpr: TMemo;
    Panel1: TPanel;
    Button1: TButton;
    txtOutput: TMemo;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    //FEval: TUOMEvaluator;
  public
    { Public declarations }
  end;

var
  frmExprTest: TfrmExprTest;

implementation

{$R *.dfm}

procedure TfrmExprTest.FormCreate(Sender: TObject);
begin
  //FEval:= TUOMEvaluator.Create;
  //FEval.AddConstant('TEST_VAL', '42', 'Float');
end;

procedure TfrmExprTest.FormDestroy(Sender: TObject);
begin
  //FreeAndNil(FEval);
end;

procedure TfrmExprTest.Button1Click(Sender: TObject);
begin
  try
    txtOutput.Lines.Text:= TUOMUtils.Calculate(txtExpr.Text);
  except
    on E: Exception do begin
      txtOutput.Lines.Text:= E.Message;
    end;
  end;
end;

end.
