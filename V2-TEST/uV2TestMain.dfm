object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD-UOM/UOM_V2 Test'
  ClientHeight = 444
  ClientWidth = 945
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    945
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
    Left = 207
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
  object lblUnitHeader: TLabel
    Left = 406
    Top = 8
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
    Width = 193
    Height = 401
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 19
    TabOrder = 0
    OnClick = lstUOMsClick
  end
  object lstUnits: TListBox
    Left = 207
    Top = 32
    Width = 193
    Height = 401
    Anchors = [akLeft, akTop, akBottom]
    ItemHeight = 19
    TabOrder = 1
    OnClick = lstUnitsClick
  end
  object cboSystem: TComboBox
    Left = 262
    Top = 3
    Width = 138
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
    Left = 406
    Top = 32
    Width = 531
    Height = 401
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    DesignSize = (
      531
      401)
    object Label3: TLabel
      Left = 8
      Top = 16
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Name'
    end
    object lblUnitName: TLabel
      Left = 111
      Top = 16
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 218
    end
    object Label5: TLabel
      Left = 8
      Top = 41
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'ID'
    end
    object lblUnitID: TLabel
      Left = 111
      Top = 41
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 218
    end
    object Label7: TLabel
      Left = 8
      Top = 66
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'System(s)'
    end
    object lblUnitSystems: TLabel
      Left = 111
      Top = 66
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 218
    end
    object Label9: TLabel
      Left = 8
      Top = 91
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Prefix'
    end
    object lblUnitPrefix: TLabel
      Left = 111
      Top = 91
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 218
    end
    object Label11: TLabel
      Left = 8
      Top = 116
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Suffix'
    end
    object lblUnitSuffix: TLabel
      Left = 111
      Top = 116
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 218
    end
    object Label4: TLabel
      Left = 8
      Top = 141
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Base From'
    end
    object lblUnitBaseFrom: TLabel
      Left = 111
      Top = 141
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label8: TLabel
      Left = 0
      Top = 166
      Width = 81
      Height = 19
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'Base To'
    end
    object lblUnitBaseTo: TLabel
      Left = 111
      Top = 166
      Width = 404
      Height = 19
      Anchors = [akLeft, akTop, akRight]
      AutoSize = False
      Caption = '...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
end
