object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 289
  ClientWidth = 554
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object txtExpr: TEdit
    Left = 80
    Top = 56
    Width = 265
    Height = 21
    TabOrder = 0
    Text = '3.1415 / 0.0101010101'
  end
  object txtResult: TEdit
    Left = 80
    Top = 136
    Width = 265
    Height = 21
    ReadOnly = True
    TabOrder = 1
  end
  object Button1: TButton
    Left = 80
    Top = 83
    Width = 75
    Height = 25
    Caption = 'Eval'
    TabOrder = 2
    OnClick = Button1Click
  end
  object DWS: TDelphiWebScript
    Left = 384
    Top = 40
  end
end
