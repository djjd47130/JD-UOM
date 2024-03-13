object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion'
  ClientHeight = 620
  ClientWidth = 1192
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
  object Splitter1: TSplitter
    Left = 408
    Top = 0
    Width = 8
    Height = 620
    Beveled = True
    ResizeStyle = rsUpdate
  end
  object pConvert: TPanel
    Left = 0
    Top = 0
    Width = 408
    Height = 620
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    object Label5: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 36
      Width = 395
      Height = 19
      Margins.Right = 10
      Align = alTop
      Caption = 'From:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 47
    end
    object lblEquivalentsTitle: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 127
      Width = 395
      Height = 19
      Margins.Right = 10
      Align = alTop
      Caption = 'Equivalents:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      ExplicitWidth = 100
    end
    object Label9: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 604
      Width = 395
      Height = 13
      Margins.Right = 10
      Align = alBottom
      Caption = '(Double-click to copy to clipboard)'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      ExplicitWidth = 162
    end
    object txtConvertFromValue: TRzSpinEdit
      AlignWithMargins = True
      Left = 10
      Top = 61
      Width = 388
      Height = 27
      Margins.Left = 10
      Margins.Right = 10
      BlankValue = 1.000000000000000000
      AllowKeyEdit = True
      ButtonWidth = 24
      Decimals = 15
      IntegersOnly = False
      Max = 1E50
      Min = -1E50
      Orientation = orHorizontal
      Value = 1.000000000000000000
      Align = alTop
      TabOrder = 0
      OnChange = txtConvertFromValueChange
    end
    object cboConvertFromUnit: TComboBox
      AlignWithMargins = True
      Left = 10
      Top = 94
      Width = 388
      Height = 27
      Margins.Left = 10
      Margins.Right = 10
      Align = alTop
      Style = csDropDownList
      TabOrder = 1
      OnClick = cboConvertFromUnitClick
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 408
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object lblConvertTitle: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 64
        Height = 27
        Align = alLeft
        Caption = 'Convert'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitHeight = 19
      end
      object cboConvertCategory: TComboBox
        AlignWithMargins = True
        Left = 80
        Top = 3
        Width = 318
        Height = 27
        Margins.Left = 10
        Margins.Right = 10
        Align = alClient
        Style = csDropDownList
        TabOrder = 0
        OnClick = cboConvertCategoryClick
      end
    end
    object lstEquivalents: TListView
      AlignWithMargins = True
      Left = 3
      Top = 152
      Width = 402
      Height = 446
      Align = alClient
      Columns = <
        item
          Caption = 'System'
          Width = 380
        end>
      HideSelection = False
      ReadOnly = True
      RowSelect = True
      ShowColumnHeaders = False
      TabOrder = 3
      ViewStyle = vsReport
      OnDblClick = lstEquivalentsDblClick
    end
  end
  object pMain: TPanel
    Left = 416
    Top = 0
    Width = 776
    Height = 620
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 620
    object pTop: TPanel
      Left = 0
      Top = 0
      Width = 776
      Height = 209
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 620
      object pCategories: TPanel
        Left = 223
        Top = 0
        Width = 223
        Height = 209
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
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
        object lstCategories: TListView
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 217
          Height = 178
          Align = alClient
          Columns = <
            item
              Caption = 'System'
              Width = 180
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lstCategoriesSelectItem
          ExplicitLeft = 6
        end
      end
      object pUOMs: TPanel
        Left = 446
        Top = 0
        Width = 223
        Height = 209
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
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
        object lstUOMs: TListView
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 217
          Height = 178
          Align = alClient
          Columns = <
            item
              Caption = 'System'
              Width = 180
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 0
          ViewStyle = vsReport
          OnSelectItem = lstUOMsSelectItem
          ExplicitTop = 94
          ExplicitHeight = 112
        end
      end
      object pInfo: TPanel
        Left = 669
        Top = 0
        Width = 107
        Height = 209
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitWidth = 32
        object pTestVal: TPanel
          Left = 0
          Top = 0
          Width = 107
          Height = 25
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          ExplicitWidth = 32
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
            Left = -160
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
            ExplicitLeft = -235
            ExplicitHeight = 16
          end
          object txtValue: TRzSpinEdit
            AlignWithMargins = True
            Left = -81
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
            ExplicitLeft = -156
            ExplicitHeight = 27
          end
        end
        object pUnitDetail: TPanel
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 101
          Height = 178
          Align = alClient
          TabOrder = 1
          ExplicitWidth = 26
          DesignSize = (
            101
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
            Width = -42
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
            Width = -42
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
            Width = -42
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
            Width = -42
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
            Width = -42
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
            Width = -42
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
        TabOrder = 0
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
        object lstSystems: TListView
          AlignWithMargins = True
          Left = 3
          Top = 28
          Width = 217
          Height = 178
          Align = alClient
          Checkboxes = True
          Columns = <
            item
              Caption = 'System'
              Width = 180
            end>
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          SortType = stText
          TabOrder = 0
          ViewStyle = vsReport
          OnItemChecked = lstSystemsItemChecked
        end
      end
    end
    object pBottom: TPanel
      Left = 0
      Top = 209
      Width = 776
      Height = 411
      Align = alClient
      TabOrder = 1
      ExplicitWidth = 620
      object Chart: TChart
        AlignWithMargins = True
        Left = 11
        Top = 4
        Width = 754
        Height = 403
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
        OnAfterDraw = ChartAfterDraw
        Align = alClient
        BevelOuter = bvNone
        Color = clBlack
        TabOrder = 0
        ExplicitWidth = 598
        DesignSize = (
          754
          403)
        DefaultCanvas = 'TGDIPlusCanvas'
        ColorPaletteIndex = 9
        object txtChartScale: TRzSpinEdit
          AlignWithMargins = True
          Left = 617
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
          ExplicitLeft = 461
        end
        object chkNegative: TCheckBox
          Left = 500
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
          StyleElements = [seClient, seBorder]
          OnClick = chkNegativeClick
          ExplicitLeft = 344
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
  end
end
