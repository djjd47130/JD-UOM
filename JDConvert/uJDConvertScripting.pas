unit uJDConvertScripting;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter,
  SynEditCodeFolding, SynHighlighterPas, SynEdit, Vcl.ExtCtrls, Vcl.StdCtrls,
  JD.Uom, JD.Common, JD.Ctrls, JD.Ctrls.FontButton;

type
  TfrJDConvertScripting = class(TFrame)
    SynPasSyn1: TSynPasSyn;
    Panel7: TPanel;
    Splitter1: TSplitter;
    txtExpr: TSynEdit;
    btnExecScript: TJDFontButton;
    pJDConvertScriptingToolbar: TPanel;
    JDFontButton1: TJDFontButton;
    JDFontButton2: TJDFontButton;
    JDFontButton3: TJDFontButton;
    JDFontButton4: TJDFontButton;
    JDFontButton5: TJDFontButton;
    txtOutput: TSynEdit;
    procedure btnExecScriptClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses
  uJDConvertMain;

procedure TfrJDConvertScripting.btnExecScriptClick(Sender: TObject);
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
