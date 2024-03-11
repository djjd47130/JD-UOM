object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion'
  ClientHeight = 761
  ClientWidth = 1184
  Color = clBtnFace
  DoubleBuffered = True
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
    Width = 1184
    Height = 209
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 1009
    object pCategories: TPanel
      Left = 223
      Top = 0
      Width = 223
      Height = 209
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 19
        Align = alTop
        Caption = 'Categories'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 88
      end
      object lstCategories: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 217
        Height = 178
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstCategoriesClick
        ExplicitTop = 30
      end
    end
    object pUOMs: TPanel
      Left = 446
      Top = 0
      Width = 223
      Height = 209
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 19
        Align = alTop
        Caption = 'Units'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 42
      end
      object lstUOMs: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 217
        Height = 178
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstUOMsClick
      end
    end
    object pInfo: TPanel
      Left = 669
      Top = 0
      Width = 515
      Height = 209
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitWidth = 300
      object pTestVal: TPanel
        Left = 0
        Top = 0
        Width = 515
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 340
        object lblUnitHeader: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 34
          Height = 19
          Align = alLeft
          Caption = 'Unit'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          AlignWithMargins = True
          Left = 248
          Top = 3
          Width = 73
          Height = 19
          Align = alRight
          Caption = 'Test Value:'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clYellow
          Font.Height = -13
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          StyleElements = [seClient, seBorder]
          ExplicitLeft = 81
          ExplicitHeight = 16
        end
        object txtValue: TRzSpinEdit
          AlignWithMargins = True
          Left = 327
          Top = 3
          Width = 185
          Height = 21
          BlankValue = 1.000000000000000000
          AllowKeyEdit = True
          Decimals = 4
          IntegersOnly = False
          Max = 1E50
          Orientation = orHorizontal
          Value = 1.000000000000000000
          Align = alRight
          TabOrder = 0
          OnChange = txtValueChange
          ExplicitTop = 1
        end
      end
      object pUnitDetail: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 509
        Height = 178
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 334
        DesignSize = (
          509
          178)
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
          Width = 366
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
          Top = 66
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'System(s)'
        end
        object lblUnitSystems: TLabel
          Left = 127
          Top = 66
          Width = 366
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
          ExplicitWidth = 242
        end
        object Label11: TLabel
          Left = 8
          Top = 91
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Suffix'
        end
        object lblUnitSuffix: TLabel
          Left = 127
          Top = 91
          Width = 366
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
          ExplicitWidth = 191
        end
        object Label4: TLabel
          Left = 8
          Top = 116
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'From Base'
        end
        object lblUnitBaseFrom: TLabel
          Left = 127
          Top = 116
          Width = 366
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
          ExplicitWidth = 191
        end
        object Label8: TLabel
          Left = 8
          Top = 141
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'To Base'
        end
        object lblUnitBaseTo: TLabel
          Left = 127
          Top = 141
          Width = 366
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
          ExplicitWidth = 191
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
          Width = 366
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
    object pSystems: TPanel
      Left = 0
      Top = 0
      Width = 223
      Height = 209
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object Label12: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 217
        Height = 19
        Align = alTop
        Caption = 'Systems'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 67
      end
      object lstSystems: TCheckListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 217
        Height = 178
        OnClickCheck = lstSystemsClickCheck
        Align = alClient
        ItemHeight = 19
        Sorted = True
        TabOrder = 0
      end
    end
  end
  object pBottom: TPanel
    Left = 0
    Top = 209
    Width = 1184
    Height = 552
    Align = alClient
    TabOrder = 1
    ExplicitTop = 212
    ExplicitWidth = 984
    ExplicitHeight = 352
    object Chart: TChart
      AlignWithMargins = True
      Left = 566
      Top = 32
      Width = 519
      Height = 249
      Margins.Left = 50
      Margins.Right = 50
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
      BottomAxis.Grid.Width = 0
      BottomAxis.Grid.DrawEvery = 5
      BottomAxis.Increment = 10.000000000000000000
      BottomAxis.LabelsAngle = 90
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.MinorTickLength = 3
      BottomAxis.TickLength = 7
      BottomAxis.Title.Caption = 'Base Unit'
      BottomAxis.Title.Font.Color = clWhite
      BottomAxis.Title.Font.Height = -13
      BottomAxis.Title.Font.Style = [fsBold]
      Chart3DPercent = 22
      LeftAxis.Axis.Color = clGray
      LeftAxis.Axis.Width = 1
      LeftAxis.AxisValuesFormat = '#,###,###,##0.#########'
      LeftAxis.Grid.Color = 1837138048
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
      ZoomWheel = pmwNormal
      OnAfterDraw = ChartAfterDraw
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 0
      DesignSize = (
        519
        249)
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 9
      object txtChartScale: TRzSpinEdit
        AlignWithMargins = True
        Left = 382
        Top = 3
        Width = 132
        Height = 27
        Hint = 'How many base units to show in chart'#39's X axis'
        BlankValue = 1.000000000000000000
        AllowKeyEdit = True
        Max = 1E50
        Orientation = orHorizontal
        Value = 100.000000000000000000
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = txtChartScaleChange
        ExplicitLeft = 768
      end
      object chkNegative: TCheckBox
        Left = 265
        Top = 7
        Width = 97
        Height = 19
        Hint = 'Whether to include negative values in chart'#39's X axis'
        Anchors = [akTop, akRight]
        Caption = 'Negative'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        OnClick = chkNegativeClick
        ExplicitLeft = 651
      end
      object Series1: TLineSeries
        Selected.Hover.Visible = False
        Title = 'Comparison'
        Brush.BackColor = clDefault
        LinePen.Color = 16685954
        LinePen.Width = 2
        Pointer.InflateMargins = True
        Pointer.Style = psRectangle
        TreatNulls = tnIgnore
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
    object pConvert: TPanel
      Left = 1
      Top = 1
      Width = 288
      Height = 550
      Align = alLeft
      TabOrder = 1
      ExplicitLeft = 0
      ExplicitTop = 6
      object lblConvertTitle: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 4
        Width = 280
        Height = 19
        Align = alTop
        Caption = 'Convert'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 64
      end
      object Label5: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 41
        Width = 273
        Height = 19
        Margins.Top = 15
        Margins.Right = 10
        Align = alTop
        Caption = 'From:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 11
        ExplicitTop = 36
        ExplicitWidth = 47
      end
      object Label9: TLabel
        AlignWithMargins = True
        Left = 4
        Top = 188
        Width = 273
        Height = 19
        Margins.Top = 15
        Margins.Right = 10
        Align = alTop
        Caption = 'To:'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitLeft = 59
        ExplicitTop = 277
        ExplicitWidth = 266
      end
      object txtConvertFromValue: TRzSpinEdit
        AlignWithMargins = True
        Left = 11
        Top = 66
        Width = 266
        Height = 28
        Margins.Left = 10
        Margins.Right = 10
        BlankValue = 1.000000000000000000
        AllowKeyEdit = True
        ButtonWidth = 24
        Decimals = 4
        IntegersOnly = False
        Max = 1E50
        Min = -1E50
        Orientation = orHorizontal
        Value = 1.000000000000000000
        Align = alTop
        TabOrder = 0
        OnChange = txtConvertFromValueChange
        ExplicitLeft = 4
        ExplicitTop = 61
        ExplicitWidth = 280
      end
      object cboConvertFromUnit: TComboBox
        AlignWithMargins = True
        Left = 11
        Top = 100
        Width = 266
        Height = 27
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Style = csDropDownList
        TabOrder = 1
        OnClick = cboConvertFromUnitClick
        ExplicitTop = 95
      end
      object cboConvertToUnit: TComboBox
        AlignWithMargins = True
        Left = 11
        Top = 246
        Width = 266
        Height = 27
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        Style = csDropDownList
        TabOrder = 2
        OnClick = cboConvertToUnitClick
        ExplicitTop = 286
      end
      object btnConvert: TButton
        AlignWithMargins = True
        Left = 11
        Top = 145
        Width = 266
        Height = 25
        Margins.Left = 10
        Margins.Top = 15
        Margins.Right = 10
        Align = alTop
        Caption = 'Convert'
        TabOrder = 3
        OnClick = btnConvertClick
        ExplicitLeft = 80
        ExplicitTop = 256
        ExplicitWidth = 75
      end
      object txtConvertToValue: TEdit
        AlignWithMargins = True
        Left = 11
        Top = 213
        Width = 266
        Height = 27
        Margins.Left = 10
        Margins.Right = 10
        Align = alTop
        ReadOnly = True
        TabOrder = 4
        ExplicitLeft = -3
        ExplicitTop = 237
        ExplicitWidth = 280
      end
    end
  end
end
