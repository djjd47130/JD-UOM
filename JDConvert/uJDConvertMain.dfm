object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion Library'
  ClientHeight = 561
  ClientWidth = 984
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
    Width = 984
    Height = 265
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Panel2: TPanel
      Left = 208
      Top = 0
      Width = 208
      Height = 265
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 202
        Height = 19
        Align = alTop
        Caption = 'Units of Measurement'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 179
      end
      object lstUOMs: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 202
        Height = 234
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
        OnClick = lstUOMsClick
      end
    end
    object Panel3: TPanel
      Left = 416
      Top = 0
      Width = 177
      Height = 265
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      object Panel4: TPanel
        Left = 0
        Top = 0
        Width = 177
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 42
          Height = 19
          Align = alLeft
          Caption = 'Units'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
        end
      end
      object lstUnits: TListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 171
        Height = 234
        Align = alClient
        ItemHeight = 19
        TabOrder = 1
        OnClick = lstUnitsClick
      end
    end
    object Panel5: TPanel
      Left = 593
      Top = 0
      Width = 391
      Height = 265
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 2
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 391
        Height = 25
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
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
          Left = 105
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
          ExplicitHeight = 16
        end
        object txtValue: TRzSpinEdit
          AlignWithMargins = True
          Left = 184
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
          ExplicitHeight = 27
        end
      end
      object pUnitDetail: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 385
        Height = 234
        Align = alClient
        TabOrder = 1
        DesignSize = (
          385
          234)
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
          Width = 242
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
      Width = 208
      Height = 265
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 3
      object Label12: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 202
        Height = 19
        Align = alTop
        Caption = 'Measurement Systems'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        ParentFont = False
        ExplicitWidth = 183
      end
      object lstSystems: TCheckListBox
        AlignWithMargins = True
        Left = 3
        Top = 28
        Width = 202
        Height = 234
        OnClickCheck = lstSystemsClickCheck
        Align = alClient
        ItemHeight = 19
        TabOrder = 0
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
