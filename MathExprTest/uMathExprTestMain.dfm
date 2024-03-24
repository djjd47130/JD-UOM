object frmExprTest: TfrmExprTest
  Left = 0
  Top = 0
  Caption = 'UOM Expression Test App'
  ClientHeight = 439
  ClientWidth = 633
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
    Top = 248
    Width = 633
    Height = 8
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
  end
  object txtExpr: TMemo
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 627
    Height = 242
    Align = alClient
    Lines.Strings = (
      'var V = UOM('#39'3ft'#39') / UOM('#39'6.9cm'#39');'
      ''
      'PrintLn(V);')
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitLeft = 8
    ExplicitWidth = 523
    ExplicitHeight = 174
  end
  object Panel1: TPanel
    Left = 0
    Top = 256
    Width = 633
    Height = 183
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object Button1: TButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 627
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
      Width = 627
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
  object DWS: TDelphiWebScript
    Left = 360
    Top = 96
  end
  object dwsUnit1: TdwsUnit
    Script = DWS
    Classes = <
      item
        Fields = <
          item
            Visibility = cvPrivate
          end>
      end>
    Constants = <
      item
        Name = 'TEST'
        DataType = 'Float'
        Value = '42'
      end>
    Enumerations = <
      item
        Elements = <>
      end>
    Functions = <
      item
        Name = 'UOM'
        Parameters = <
          item
            Name = 'Expr'
            DataType = 'String'
          end>
        ResultType = 'Float'
      end>
    Records = <
      item
        Name = 'TUOM'
        Members = <>
        Properties = <>
      end>
    UnitName = 'JD.UOM'
    StaticSymbols = False
    Left = 416
    Top = 96
  end
end
