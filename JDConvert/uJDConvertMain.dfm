object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion Library'
  ClientHeight = 561
  ClientWidth = 947
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
    Width = 947
    Height = 233
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 984
    object pCategories: TPanel
      Left = 161
      Top = 0
      Width = 161
      Height = 233
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 158
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 155
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
        Width = 155
        Height = 202
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstCategoriesClick
        ExplicitWidth = 202
      end
    end
    object pUOMs: TPanel
      Left = 322
      Top = 0
      Width = 177
      Height = 233
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 366
      ExplicitTop = -6
      object Label2: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 171
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
        Width = 171
        Height = 202
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstUOMsClick
      end
    end
    object pInfo: TPanel
      Left = 499
      Top = 0
      Width = 448
      Height = 233
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 593
      ExplicitWidth = 391
      object pTestVal: TPanel
        Left = 0
        Top = 0
        Width = 448
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
          Left = 162
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
          Left = 241
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
        Width = 442
        Height = 202
        Align = alClient
        TabOrder = 1
        ExplicitWidth = 385
        DesignSize = (
          442
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
          Width = 299
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
          Width = 299
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
          Width = 299
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
          Width = 299
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
          Width = 299
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
          Width = 299
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
          Width = 299
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
      Width = 161
      Height = 233
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object Label12: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 155
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
        Width = 155
        Height = 202
        OnClickCheck = lstSystemsClickCheck
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        ExplicitLeft = -29
        ExplicitWidth = 202
      end
    end
  end
  object pChart: TPanel
    Left = 0
    Top = 233
    Width = 947
    Height = 328
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 984
    object Chart: TChart
      AlignWithMargins = True
      Left = 40
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
      LeftAxis.Axis.Color = clGray
      LeftAxis.Axis.Width = 1
      LeftAxis.AxisValuesFormat = '#,###,###,##0.#########'
      LeftAxis.Grid.Color = 1837138048
      LeftAxis.Grid.Width = 0
      LeftAxis.Increment = 10.000000000000000000
      LeftAxis.LabelsFormat.Font.Color = clWhite
      LeftAxis.Title.Caption = 'Conversion'
      LeftAxis.Title.Font.Color = clWhite
      LeftAxis.Title.Font.Height = -13
      LeftAxis.Title.Font.Style = [fsBold]
      Panning.MouseWheel = pmwNone
      View3D = False
      View3DWalls = False
      Zoom.Allow = False
      OnAfterDraw = ChartAfterDraw
      BevelOuter = bvNone
      Color = clBlack
      TabOrder = 0
      DesignSize = (
        905
        249)
      DefaultCanvas = 'TGDIPlusCanvas'
      ColorPaletteIndex = 13
      object txtChartScale: TRzSpinEdit
        AlignWithMargins = True
        Left = 768
        Top = 3
        Width = 132
        Height = 27
        BlankValue = 1.000000000000000000
        AllowKeyEdit = True
        Max = 1E50
        Orientation = orHorizontal
        Value = 500.000000000000000000
        Anchors = [akTop, akRight]
        TabOrder = 0
        OnChange = txtChartScaleChange
      end
      object Series1: TFastLineSeries
        Title = 'Comparison'
        LinePen.Color = 10708548
        LinePen.Width = 2
        XValues.Name = 'X'
        XValues.Order = loAscending
        YValues.Name = 'Y'
        YValues.Order = loNone
      end
    end
  end
end
