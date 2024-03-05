object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion Library'
  ClientHeight = 561
  ClientWidth = 986
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
    Width = 986
    Height = 233
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 984
    object pCategories: TPanel
      Left = 223
      Top = 0
      Width = 223
      Height = 233
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
        Height = 202
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstCategoriesClick
        ExplicitWidth = 202
      end
    end
    object pUOMs: TPanel
      Left = 446
      Top = 0
      Width = 223
      Height = 233
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 343
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
        Height = 202
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstUOMsClick
        ExplicitWidth = 171
      end
    end
    object pInfo: TPanel
      Left = 669
      Top = 0
      Width = 317
      Height = 233
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 593
      ExplicitWidth = 391
      object pTestVal: TPanel
        Left = 0
        Top = 0
        Width = 317
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 391
        object lblUnitHeader: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 72
          Height = 19
          Align = alLeft
          Caption = 'Unit Info'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
        object Label10: TLabel
          AlignWithMargins = True
          Left = 31
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
          ExplicitLeft = 105
          ExplicitHeight = 16
        end
        object txtValue: TRzSpinEdit
          AlignWithMargins = True
          Left = 110
          Top = 3
          Width = 204
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
          ExplicitLeft = 184
          ExplicitHeight = 27
        end
      end
      object pUnitDetail: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 311
        Height = 202
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 385
        DesignSize = (
          311
          202)
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
          Width = 168
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
          Width = 168
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
        object Label9: TLabel
          Left = 8
          Top = 91
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Prefix'
        end
        object lblUnitPrefix: TLabel
          Left = 127
          Top = 91
          Width = 168
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
          Top = 116
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Suffix'
        end
        object lblUnitSuffix: TLabel
          Left = 127
          Top = 116
          Width = 168
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
        object Label4: TLabel
          Left = 8
          Top = 141
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'From Base'
        end
        object lblUnitBaseFrom: TLabel
          Left = 127
          Top = 141
          Width = 168
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
        object Label8: TLabel
          Left = 8
          Top = 166
          Width = 97
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'To Base'
        end
        object lblUnitBaseTo: TLabel
          Left = 127
          Top = 166
          Width = 168
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
          Width = 168
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
      Height = 233
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
        Height = 202
        OnClickCheck = lstSystemsClickCheck
        Align = alClient
        ItemHeight = 19
        Sorted = True
        TabOrder = 0
      end
    end
  end
  object pChart: TPanel
    Left = 0
    Top = 233
    Width = 986
    Height = 328
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 984
    object Chart: TChart
      AlignWithMargins = True
      Left = 39
      Top = 16
      Width = 905
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
      BottomAxis.Increment = 10.000000000000000000
      BottomAxis.LabelsAngle = 90
      BottomAxis.LabelsFormat.Font.Color = clWhite
      BottomAxis.MinorTickCount = 2
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
        905
        249)
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 9
      object txtChartScale: TRzSpinEdit
        AlignWithMargins = True
        Left = 768
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
      end
      object chkNegative: TCheckBox
        Left = 651
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
      end
      object Series1: TLineSeries
        Selected.Hover.Visible = False
        Title = 'Comparison'
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
  end
end
