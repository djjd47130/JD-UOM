object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD-UOM/UOM_V2 Test'
  ClientHeight = 444
  ClientWidth = 742
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    742
    444)
  PixelsPerInch = 96
  TextHeight = 19
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 179
    Height = 19
    Caption = 'Units of Measurement'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 247
    Top = 7
    Width = 42
    Height = 19
    Caption = 'Units'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lblUnitName: TLabel
    Left = 488
    Top = 7
    Width = 72
    Height = 19
    Caption = 'Unit Info'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lstUOMs: TListBox
    Left = 8
    Top = 32
    Width = 233
    Height = 401
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 19
    TabOrder = 0
    OnClick = lstUOMsClick
  end
  object lstUnits: TListBox
    Left = 247
    Top = 32
    Width = 233
    Height = 401
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 19
    TabOrder = 1
    OnClick = lstUnitsClick
  end
  object cboSystem: TComboBox
    Left = 335
    Top = 3
    Width = 145
    Height = 27
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 2
    Text = 'Any System'
    OnClick = cboSystemClick
    Items.Strings = (
      'Any System'
      'Metric'
      'US Customary'
      'Imperial'
      'Natural')
  end
  object pUnitDetail: TPanel
    Left = 488
    Top = 32
    Width = 246
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
  end
end