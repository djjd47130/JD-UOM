object frJDConvertDetails: TfrJDConvertDetails
  Left = 0
  Top = 0
  Width = 958
  Height = 573
  TabOrder = 0
  object pBottom: TPanel
    Left = 0
    Top = 255
    Width = 958
    Height = 318
    Align = alClient
    TabOrder = 0
    ExplicitLeft = -97
    ExplicitTop = 220
    ExplicitWidth = 1055
    ExplicitHeight = 353
    object Chart: TChart
      AlignWithMargins = True
      Left = 11
      Top = 4
      Width = 936
      Height = 310
      Margins.Left = 10
      Margins.Right = 10
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
      Align = alClient
      BevelOuter = bvNone
      Color = 1644825
      TabOrder = 0
      ExplicitLeft = 9
      ExplicitTop = 3
      ExplicitWidth = 1033
      ExplicitHeight = 345
      DesignSize = (
        936
        310)
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 9
      object txtChartScale: TRzSpinEdit
        AlignWithMargins = True
        Left = 794
        Top = 2
        Width = 132
        Height = 21
        Hint = 'How many base units to show in chart'#39's X axis'
        BlankValue = 1.000000000000000000
        AllowKeyEdit = True
        Max = 999999.000000000000000000
        Orientation = orHorizontal
        Value = 10.000000000000000000
        Anchors = [akTop, akRight]
        ParentShowHint = False
        ShowHint = True
        TabOrder = 0
        OnChange = txtChartScaleChange
        ExplicitLeft = 891
      end
      object chkNegative: TCheckBox
        Left = 666
        Top = 6
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
        StyleElements = [seClient, seBorder]
        OnClick = chkNegativeClick
        ExplicitLeft = 763
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
  end
  object pTop: TPanel
    Left = 0
    Top = 0
    Width = 958
    Height = 255
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -97
    ExplicitWidth = 1055
    object pCategories: TPanel
      Left = 233
      Top = 0
      Width = 216
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 210
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
      object lstCategories: TListView
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 210
        Height = 224
        Align = alClient
        Columns = <
          item
            Caption = 'System'
            Width = 180
          end>
        HideSelection = False
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lstCategoriesSelectItem
      end
    end
    object pUOMs: TPanel
      Left = 449
      Top = 0
      Width = 217
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 211
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
      object lstUOMs: TListView
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 211
        Height = 224
        Align = alClient
        Columns = <
          item
            Caption = 'System'
            Width = 180
          end>
        HideSelection = False
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lstUOMsSelectItem
        ExplicitTop = 29
      end
    end
    object pInfo: TPanel
      Left = 666
      Top = 0
      Width = 292
      Height = 255
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 3
      ExplicitWidth = 389
      object pTestVal: TPanel
        Left = 0
        Top = 0
        Width = 292
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitWidth = 389
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
      end
      object pUnitDetail: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 286
        Height = 224
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 383
        DesignSize = (
          286
          224)
        object Label3: TLabel
          Left = 0
          Top = 16
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name'
        end
        object lblUnitName: TLabel
          Left = 127
          Top = 16
          Width = 143
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
          Left = 0
          Top = 66
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'System(s)'
        end
        object lblUnitSystems: TLabel
          Left = 127
          Top = 66
          Width = 143
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
          Left = 0
          Top = 91
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Suffix'
        end
        object lblUnitSuffix: TLabel
          Left = 127
          Top = 91
          Width = 143
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
          Left = 0
          Top = 140
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'From Base'
        end
        object lblUnitBaseFrom: TLabel
          Left = 127
          Top = 140
          Width = 143
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
          ExplicitWidth = 240
        end
        object Label8: TLabel
          Left = 0
          Top = 165
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'To Base'
        end
        object lblUnitBaseTo: TLabel
          Left = 127
          Top = 165
          Width = 143
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
          ExplicitWidth = 240
        end
        object Label6: TLabel
          Left = 0
          Top = 41
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name Plural'
        end
        object lblUnitNamePlural: TLabel
          Left = 127
          Top = 41
          Width = 143
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
        object Label14: TLabel
          Left = 0
          Top = 115
          Width = 113
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Aliases'
        end
        object lblUnitAliases: TLabel
          Left = 127
          Top = 115
          Width = 143
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
          ExplicitWidth = 240
        end
      end
    end
    object pSystems: TPanel
      Left = 0
      Top = 0
      Width = 233
      Height = 255
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label12: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 227
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
      object lstSystems: TListView
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 227
        Height = 224
        Align = alClient
        Checkboxes = True
        Columns = <
          item
            Caption = 'System'
            Width = 180
          end>
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        ShowColumnHeaders = False
        SortType = stText
        TabOrder = 0
        ViewStyle = vsReport
        OnClick = lstSystemsClick
        OnItemChecked = lstSystemsItemChecked
        ExplicitTop = 29
      end
    end
  end
end
