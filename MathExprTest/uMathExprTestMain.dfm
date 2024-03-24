object frmExprTest: TfrmExprTest
  Left = 0
  Top = 0
  Caption = 'UOM Expression Test App'
  ClientHeight = 506
  ClientWidth = 669
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object Splitter1: TSplitter
    Left = 0
    Top = 315
    Width = 669
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitTop = 248
    ExplicitWidth = 633
  end
  object txtExpr: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 663
    Height = 309
    Align = alClient
    Lines.Strings = (
      'var OutUOM: String;'
      'var V, T: Float;'
      'var Res: String;'
      ''
      'V:= UOM('#39'3ft'#39') / UOM('#39'6.9cm'#39') + Sqr(46);'
      ''
      'OutUOM:= '#39'ft'#39';'
      'T:= Convert(V, '#39'Meter'#39', OutUOM);'
      'Res:= UOMString(T, OutUOM);'
      'PrintLn(Res);'
      ''
      'OutUOM:= '#39'ly'#39';'
      'T:= Convert(V, '#39'Meter'#39', OutUOM);'
      'Res:= UOMString(T, OutUOM);'
      'PrintLn(Res);')
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitLeft = -2
    ExplicitTop = 0
    ExplicitHeight = 290
  end
  object Panel1: TPanel
    Left = 0
    Top = 323
    Width = 669
    Height = 183
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 256
    ExplicitWidth = 633
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 663
      Height = 25
      Align = alTop
      Caption = 'Evaluate Expression'
      Default = True
      TabOrder = 0
      OnClick = Button1Click
      ExplicitLeft = 8
      ExplicitTop = 183
      ExplicitWidth = 523
    end
    object txtOutput: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 34
      Width = 663
      Height = 146
      Align = alClient
      ScrollBars = ssVertical
      TabOrder = 1
      ExplicitLeft = 8
      ExplicitTop = 214
      ExplicitWidth = 523
      ExplicitHeight = 107
    end
  end
end
