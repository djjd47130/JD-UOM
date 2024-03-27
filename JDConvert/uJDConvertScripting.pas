unit uJDConvertScripting;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, SynEditHighlighter,
  SynEditCodeFolding, SynHighlighterPas, SynEdit, Vcl.ExtCtrls, Vcl.StdCtrls,
  JD.Uom, JD.Common, JD.Ctrls, JD.Ctrls.FontButton, SynCompletionProposal,
  Vcl.ExtDlgs, System.Actions, Vcl.ActnList, Vcl.ComCtrls;

type
  TfrJDConvertScripting = class(TFrame)
    SynPasSyn1: TSynPasSyn;
    Panel7: TPanel;
    Splitter1: TSplitter;
    txtExpr: TSynEdit;
    pJDConvertScriptingToolbar: TPanel;
    btnNew: TJDFontButton;
    btnOpen: TJDFontButton;
    btnSave: TJDFontButton;
    btnSaveAs: TJDFontButton;
    btnExec: TJDFontButton;
    txtOutput: TSynEdit;
    dlgOpen: TOpenTextFileDialog;
    dlgSave: TSaveTextFileDialog;
    Acts: TActionList;
    actNew: TAction;
    actOpen: TAction;
    actSave: TAction;
    actSaveAs: TAction;
    Stat: TStatusBar;
    actExecute: TAction;
    procedure btnExecScriptClick(Sender: TObject);
    procedure btnNewClick(Sender: TObject);
    procedure btnOpenClick(Sender: TObject);
    procedure txtExprChange(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnSaveAsClick(Sender: TObject);
    procedure txtExprClick(Sender: TObject);
    procedure txtExprKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    FFilename: String;
    FModified: Boolean;
    procedure UpdateActions;
    function New: Boolean;
    function Load(const Filename: String): Boolean;
    function Save: Boolean;
    function SaveAs: Boolean;
    function SaveToFile(const Filename: String): Boolean;
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
  UpdateActions;
end;

procedure TfrJDConvertScripting.btnNewClick(Sender: TObject);
begin
  New;
end;

procedure TfrJDConvertScripting.btnOpenClick(Sender: TObject);
begin
  if dlgOpen.Execute then begin
    Load(dlgOpen.FileName);
  end;
end;

procedure TfrJDConvertScripting.btnSaveAsClick(Sender: TObject);
begin
  SaveAs;
end;

procedure TfrJDConvertScripting.btnSaveClick(Sender: TObject);
begin
  Save;
end;

function TfrJDConvertScripting.New: Boolean;
begin
  if FModified then begin
    case MessageDlg('Save changes to current script?', mtConfirmation, [mbYes,mbNo,mbCancel], 0) of
      mrYes: begin
        Result:= Save;
      end;
      mrNo: begin
        Result:= True;
      end;
      else begin
        Result:= False;
      end;
    end;
  end else begin
    Result:= True;
  end;
  if Result then begin
    //Prepare new blank script...
    txtExpr.Lines.Clear;
    txtOutput.Lines.Clear;
    FFilename:= '';
    FModified:= False;
    txtExpr.SetFocus;
  end;
  UpdateActions;
end;

function TfrJDConvertScripting.Load(const Filename: String): Boolean;
begin
  if FModified then begin
    case MessageDlg('Save changes to current script?', mtConfirmation, [mbYes,mbNo,mbCancel], 0) of
      mrYes: begin
        Result:= Save;
      end;
      mrNo: begin
        Result:= True;
      end;
      else begin
        Result:= False;
      end;
    end;
  end else begin
    Result:= True;
  end;
  if Result then begin
    //Open script from file...
    FFilename:= Filename;
    txtExpr.Lines.LoadFromFile(FFilename);
    txtOutput.Lines.Clear;
    FModified:= False;
    txtExpr.SetFocus;
  end;
  UpdateActions;
end;

function TfrJDConvertScripting.Save: Boolean;
begin
  if FFilename = '' then begin
    Result:= SaveAs;
  end else begin
    Result:= SaveToFile(FFilename);
  end;
end;

function TfrJDConvertScripting.SaveAs: Boolean;
begin
  Result:= False;
  dlgSave.FileName:= FFilename;
  if dlgSave.Execute then begin
    Result:= SaveToFile(dlgSave.FileName);
  end;
end;

function TfrJDConvertScripting.SaveToFile(const Filename: String): Boolean;
begin
  txtExpr.Lines.SaveToFile(Filename);
  FFilename:= Filename;
  FModified:= False;
  Result:= True;
  UpdateActions;
end;

procedure TfrJDConvertScripting.txtExprChange(Sender: TObject);
begin
  FModified:= True;
  UpdateActions;
end;

procedure TfrJDConvertScripting.txtExprClick(Sender: TObject);
begin
  UpdateActions;
end;

procedure TfrJDConvertScripting.txtExprKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  UpdateActions;
end;

procedure TfrJDConvertScripting.UpdateActions;
begin
  //TODO
  actSave.Enabled:= FModified;
  Stat.Panels[0].Text:= IntToStr(txtExpr.CaretY)+':'+IntToStr(txtExpr.CaretX);
  if FModified then begin
    Stat.Panels[1].Text:= 'Modified';
  end else begin
    Stat.Panels[1].Text:= '';
  end;
end;

end.
