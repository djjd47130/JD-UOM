object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD-UOM/UOM_V2 Test'
  ClientHeight = 561
  ClientWidth = 984
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object pMain: TPanel
    Left = 0
    Top = 0
    Width = 984
    Height = 265
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 962
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 208
      Height = 265
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 1
      ExplicitTop = 1
      ExplicitHeight = 279
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 202
        Height = 27
        Align = alTop
        Caption = 'Units of Measurement'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object lstUOMs: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 202
        Height = 226
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstUOMsClick
        ExplicitTop = 29
        ExplicitWidth = 200
        ExplicitHeight = 246
      end
    end
    object Panel3: TPanel
      Left = 208
      Top = 0
      Width = 177
      Height = 265
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 264
      ExplicitTop = 24
      ExplicitHeight = 209
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 177
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = -5
        ExplicitWidth = 175
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 42
          Height = 27
          Align = alLeft
          Caption = 'Units'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitLeft = 133
          ExplicitTop = 7
          ExplicitHeight = 19
        end
        object cboSystem: TComboBox
          AlignWithMargins = True
          Left = 51
          Top = 3
          Width = 123
          Height = 27
          Align = alClient
          Style = csDropDownList
          ItemIndex = 0
          TabOrder = 0
          Text = 'Any System'
          OnClick = cboSystemClick
          Items.Strings = (
            'Any System'
            'Metric'
            'US Customary'
            'Imperial'
            'Natural')
          ExplicitLeft = 37
          ExplicitWidth = 138
        end
      end
      object lstUnits: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 171
        Height = 226
        Align = alClient
        ItemHeight = 19
        TabOrder = 1
        OnClick = lstUnitsClick
        ExplicitLeft = -16
        ExplicitTop = -103
        ExplicitWidth = 193
        ExplicitHeight = 382
      end
    end
    object Panel5: TPanel
      Left = 385
      Top = 0
      Width = 599
      Height = 265
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 388
      ExplicitTop = 1
      ExplicitWidth = 390
      ExplicitHeight = 279
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 599
        Height = 33
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 2
        ExplicitTop = 1
        ExplicitWidth = 388
        object lblUnitHeader: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 72
          Height = 27
          Align = alLeft
          Caption = 'Unit Info'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          ExplicitLeft = 316
          ExplicitTop = 8
          ExplicitHeight = 19
        end
        object txtValue: TRzSpinEdit
          AlignWithMargins = True
          Left = 427
          Top = 3
          Width = 169
          Height = 27
          Decimals = 1
          IntegersOnly = False
          Max = 100.000000000000000000
          Value = 1.000000000000000000
          Align = alRight
          TabOrder = 0
          OnChange = txtValueChange
          ExplicitLeft = 219
          ExplicitTop = 0
        end
      end
      object pUnitDetail: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 36
        Width = 593
        Height = 226
        Align = alClient
        TabOrder = 1
        ExplicitLeft = 9
        ExplicitTop = -103
        ExplicitWidth = 548
        ExplicitHeight = 382
        DesignSize = (
          593
          226)
        object Label3: TLabel
          Left = 8
          Top = 16
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name'
        end
        object lblUnitName: TLabel
          Left = 127
          Top = 16
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label5: TLabel
          Left = 8
          Top = 66
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'ID'
        end
        object lblUnitID: TLabel
          Left = 127
          Top = 66
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label7: TLabel
          Left = 8
          Top = 91
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'System(s)'
        end
        object lblUnitSystems: TLabel
          Left = 127
          Top = 91
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label9: TLabel
          Left = 8
          Top = 116
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Prefix'
        end
        object lblUnitPrefix: TLabel
          Left = 127
          Top = 116
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label11: TLabel
          Left = 8
          Top = 141
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Suffix'
        end
        object lblUnitSuffix: TLabel
          Left = 127
          Top = 141
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label4: TLabel
          Left = 8
          Top = 166
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'From Base'
        end
        object lblUnitBaseFrom: TLabel
          Left = 127
          Top = 166
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label8: TLabel
          Left = 8
          Top = 191
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'To Base'
        end
        object lblUnitBaseTo: TLabel
          Left = 127
          Top = 191
          Width = 450
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
          ExplicitWidth = 388
        end
        object Label6: TLabel
          Left = 8
          Top = 41
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name Plural'
        end
        object lblUnitNamePlural: TLabel
          Left = 127
          Top = 41
          Width = 450
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
          ExplicitWidth = 388
        end
      end
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 265
    Width = 984
    Height = 296
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 64
    ExplicitTop = 280
    ExplicitWidth = 761
    ExplicitHeight = 233
    object Chart: TChart
      Left = 40
      Top = 16
      Width = 905
      Height = 249
      AllowPanning = pmNone
      LeftWall.Visible = False
      Legend.CheckBoxes = True
      Legend.Font.Color = clWhite
      Legend.Transparent = True
      Title.Font.Color = clWhite
      Title.Font.Height = -19
      Title.Font.Style = [fsBold]
      Title.Text.Strings = (
        'UOM Comparison')
      BottomAxis.Axis.Color = 1283489920
      BottomAxis.Axis.Width = 0
      BottomAxis.Grid.Color = clGray
      BottomAxis.Increment = 10.000000000000000000
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.MinorTickCount = 2
      BottomAxis.Title.Caption = 'Base Unit'
      BottomAxis.Title.Font.Color = clWhite
      BottomAxis.Title.Font.Height = -13
      BottomAxis.Title.Font.Style = [fsBold]
      LeftAxis.Axis.Color = clGray
      LeftAxis.Axis.Width = 1
      LeftAxis.Grid.Color = 2105573504
      LeftAxis.Grid.Width = 0
      LeftAxis.LabelsFormat.Font.Color = clWhite
      LeftAxis.Title.Caption = 'Conversion'
      LeftAxis.Title.Font.Color = clWhite
      LeftAxis.Title.Font.Height = -13
      LeftAxis.Title.Font.Style = [fsBold]
      Panning.MouseWheel = pmwNone
      View3D = False
      View3DWalls = False
      Zoom.Allow = False
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 0
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
      object Series1: TFastLineSeries
        Title = 'Comparison'
        LinePen.Color = 10708548
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
end
