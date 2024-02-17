object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'UOM Calculator'
  ClientHeight = 378
  ClientWidth = 558
  Color = clBlack
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWhite
  Font.Height = -21
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClick = FormClick
  OnCreate = FormCreate
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    558
    378)
  PixelsPerInch = 96
  TextHeight = 25
  object lblUOM: TLabel
    Left = 16
    Top = 17
    Width = 171
    Height = 19
    Caption = 'Unit of Measurement'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblConvertFrom: TLabel
    Left = 16
    Top = 135
    Width = 110
    Height = 19
    Caption = 'Convert From'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblConvertTo: TLabel
    Left = 16
    Top = 255
    Width = 90
    Height = 19
    Caption = 'Convert To'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblUOMSystem: TLabel
    Left = 326
    Top = 16
    Width = 59
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'System'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    Visible = False
  end
  object Bevel1: TBevel
    Left = 21
    Top = 97
    Width = 529
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object lblUnitFrom: TLabel
    Left = 326
    Top = 135
    Width = 34
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Unit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblUnitTo: TLabel
    Left = 326
    Top = 253
    Width = 34
    Height = 19
    Anchors = [akTop, akRight]
    Caption = 'Unit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Bevel2: TBevel
    Left = 16
    Top = 217
    Width = 529
    Height = 9
    Anchors = [akLeft, akTop, akRight]
    Shape = bsBottomLine
  end
  object lblSuffixFrom: TLabel
    Left = 259
    Top = 165
    Width = 61
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Suffix'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    StyleElements = [seClient, seBorder]
  end
  object lblSuffixTo: TLabel
    Left = 259
    Top = 283
    Width = 61
    Height = 25
    Anchors = [akTop, akRight]
    Caption = 'Suffix'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    StyleElements = [seClient, seBorder]
  end
  object cboConvertFrom: TComboBox
    Left = 326
    Top = 160
    Width = 216
    Height = 33
    Cursor = crHandPoint
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
    TabStop = False
    OnClick = seConvertFromChange
    OnCloseUp = cboConvertFromEnter
  end
  object cboConvertTo: TComboBox
    Left = 326
    Top = 278
    Width = 216
    Height = 33
    Cursor = crHandPoint
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 3
    TabStop = False
    OnClick = seConvertToChange
    OnCloseUp = cboConvertToEnter
  end
  object cboSystem: TComboBox
    Left = 326
    Top = 40
    Width = 216
    Height = 33
    Cursor = crHandPoint
    Style = csDropDownList
    Anchors = [akTop, akRight]
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    TabStop = False
    Visible = False
    OnClick = cboSystemClick
    OnCloseUp = cboSystemEnter
  end
  object cboUOM: TComboBox
    Left = 16
    Top = 40
    Width = 291
    Height = 33
    Cursor = crHandPoint
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    TabStop = False
    StyleElements = [seClient, seBorder]
    OnClick = cboUOMClick
    OnCloseUp = cboUOMEnter
  end
  object Stat: TStatusBar
    Left = 0
    Top = 359
    Width = 558
    Height = 19
    Panels = <
      item
        Width = 50
      end>
    ExplicitLeft = -8
  end
  object seConvertFrom: TRzSpinEdit
    Left = 16
    Top = 160
    Width = 237
    Height = 33
    AllowKeyEdit = True
    Max = 2147483647.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 5
    OnChange = seConvertFromChange
  end
  object seConvertTo: TRzSpinEdit
    Left = 16
    Top = 280
    Width = 237
    Height = 33
    Max = 2147483647.000000000000000000
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 6
    OnChange = seConvertToChange
  end
end
