object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion'
  ClientHeight = 620
  ClientWidth = 1007
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
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 1007
    Height = 620
    ActivePage = tabConvert
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Images = Img32
    ParentFont = False
    TabOrder = 0
    object tabConvert: TTabSheet
      Caption = '    Convert    '
      ExplicitLeft = 0
      ExplicitTop = 34
      ExplicitWidth = 1409
      ExplicitHeight = 586
      object pConvert: TPanel
        Left = 0
        Top = 0
        Width = 427
        Height = 575
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 16
        ExplicitHeight = 586
        object Label5: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 36
          Width = 414
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
          Width = 414
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
          Top = 559
          Width = 414
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
          ExplicitTop = 570
          ExplicitWidth = 162
        end
        object txtConvertFromValue: TRzSpinEdit
          AlignWithMargins = True
          Left = 10
          Top = 61
          Width = 407
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
          Alignment = taLeftJustify
          TabOrder = 0
          OnChange = txtConvertFromValueChange
        end
        object cboConvertFromUnit: TComboBox
          AlignWithMargins = True
          Left = 10
          Top = 94
          Width = 407
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
          Width = 427
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
            Width = 337
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
          Width = 421
          Height = 401
          Align = alClient
          Columns = <
            item
              Caption = 'System'
              Width = 400
            end>
          HideSelection = False
          ReadOnly = True
          RowSelect = True
          ShowColumnHeaders = False
          TabOrder = 3
          ViewStyle = vsReport
          OnDblClick = lstEquivalentsDblClick
          ExplicitHeight = 412
        end
      end
    end
    object tabFind: TTabSheet
      Caption = '    Find    '
      ImageIndex = 2
    end
    object tabDetails: TTabSheet
      Caption = '    Details    '
      ImageIndex = 1
      ExplicitLeft = 0
      ExplicitTop = 33
      ExplicitWidth = 585
      ExplicitHeight = 347
      object pBottom: TPanel
        Left = 0
        Top = 255
        Width = 999
        Height = 320
        Align = alClient
        TabOrder = 0
        ExplicitTop = 209
        ExplicitWidth = 620
        ExplicitHeight = 411
        object Chart: TChart
          AlignWithMargins = True
          Left = 11
          Top = 4
          Width = 977
          Height = 312
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
          Color = clBlack
          TabOrder = 0
          ExplicitLeft = 19
          ExplicitTop = 6
          ExplicitHeight = 323
          DesignSize = (
            977
            312)
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 9
          object txtChartScale: TRzSpinEdit
            AlignWithMargins = True
            Left = 1237
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
            Left = 1120
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
            ExplicitLeft = 706
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
        Width = 999
        Height = 255
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitTop = 8
        ExplicitWidth = 585
        object pCategories: TPanel
          Left = 223
          Top = 0
          Width = 202
          Height = 255
          Align = alLeft
          BevelOuter = bvNone
          TabOrder = 1
          object Label1: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 196
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
            Width = 196
            Height = 224
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
            ExplicitWidth = 217
          end
        end
        object pUOMs: TPanel
          Left = 425
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
            ReadOnly = True
            RowSelect = True
            ShowColumnHeaders = False
            TabOrder = 0
            ViewStyle = vsReport
            OnSelectItem = lstUOMsSelectItem
            ExplicitTop = 29
            ExplicitWidth = 217
          end
        end
        object pInfo: TPanel
          Left = 642
          Top = 0
          Width = 357
          Height = 255
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 3
          ExplicitLeft = 669
          ExplicitWidth = 313
          object pTestVal: TPanel
            Left = 0
            Top = 0
            Width = 357
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            ExplicitWidth = 313
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
            Width = 351
            Height = 224
            Align = alClient
            TabOrder = 1
            ExplicitWidth = 307
            DesignSize = (
              351
              224)
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
              Width = 208
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
              Width = 208
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
              Width = 208
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
              Width = 208
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
              Width = 208
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
              Width = 208
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
          Height = 255
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
            Height = 224
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
    end
  end
  object Glyphs: TJDFontGlyphs
    ImageLists = <
      item
        ImageList = Img32
      end>
    Glyphs = <
      item
        Caption = 'Convert'
        Glyph = #61561
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61561
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end
      item
        Caption = 'Details'
        Glyph = #61950
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61950
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end
      item
        Caption = 'Search'
        Glyph = #61442
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61442
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end>
    Left = 360
    Top = 8
  end
  object Img32: TImageList
    Height = 32
    Width = 32
    Left = 416
    Top = 8
    Bitmap = {
      494C010103000800040020002000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000002000000001002000000000000040
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FA999200F739
      2B00F7392B00FDE1DF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEE8E600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FA999200F7392B00F739
      2B00F7392B00FBA69F0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FA999200F7392B00F7392B00F739
      2B00F7392B00FBB1AB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FA999200F7392B00F7392B00F7392B00F739
      2B00F9756B00FEEEEC0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBB1AB00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FDDAD8000000000000000000FEEEEC00F9756B00FCBAB6000000
      000000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEEEEC00FCC3BF00FBA6
      9F00F7392B00F7392B00F7392B00FA8A8100FBB1AB00FDDAD800000000000000
      00000000000000000000FA999200F7392B00F7392B00F7392B00F7392B00F975
      6B00FEEEEC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FA9992000000000000000000FEF3F300F9756B00F7392B00F7392B00FDD3
      D00000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEEEEC00FBA69F00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FCCC
      C800FEF3F300FA999200F7392B00F7392B00F7392B00F7392B00F9756B00FEEE
      EC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FA8A
      8100000000000000000000000000FA999200F7392B00F7392B00F7392B00F739
      2B00FDE1DF00000000000000000000000000F7392B00FEE8E60000000000FCC3
      BF00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA6
      9F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA6
      9F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA69F00FBA6
      9F00FBA69F00FBA69F00FBA69F00000000000000000000000000000000000000
      00000000000000000000FCCCC800F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEEEC000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEE
      EC000000000000000000FBB1AB00F7392B00F7392B00F7392B00F7392B00F739
      2B00F9756B00FEEEEC000000000000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FBA69F00000000000000000000000000000000000000
      000000000000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEEEC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCBAB600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEEEEC000000
      000000000000FCC3BF00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FA8A81000000000000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FDD3D000000000000000000000000000000000000000
      0000FCCCC800F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FCBA
      B600FDDAD800FEE8E600FEE8E600FCCCC800FA999200F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F9756B00FEEEEC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCCCC80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDDAD800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FA99920000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FEEEEC0000000000000000000000000000000000FEEE
      EC00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB600000000000000
      00000000000000000000000000000000000000000000FEE8E600FA8A8100F739
      2B00F7392B00F7392B00F7392B00FBA69F000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCCCC80000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00FCBAB600F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FA8A81000000000000000000000000000000000000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00FDDAD80000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FA99
      9200F7392B00F7392B00F7392B00F7392B00FDDAD80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCCCC80000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FCBAB60000000000000000000000000000000000FEEEEC00F739
      2B00F7392B00F7392B00F7392B00FCBAB6000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEF3
      F300F9756B00F7392B00F7392B00F7392B00FA99920000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCCCC80000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00FCCCC800FCCCC800FCCCC800FBA69F00F7392B00F7392B00F7392B00FA8A
      8100FCCCC800FCCCC800FCCCC800FDD3D000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FDE1DF0000000000000000000000000000000000FCC3BF00F739
      2B00F7392B00F7392B00F9756B00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCCCC800F7392B00F7392B00F7392B00F7392B00FEF3F300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBA69F00F7392B00F7392B00F7392B00FCCCC80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B000000000000000000000000000000000000000000FBA69F00F739
      2B00F7392B00F7392B00FCBAB600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000F7392B00F7392B00F7392B00F7392B00FDDAD800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEF3F300FEE8E600FEE8E600FEE8
      E600FA999200F7392B00F7392B00F7392B00FCBAB600FEE8E600FEE8E600FEE8
      E600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000F7392B00F739
      2B00F7392B00F7392B00FDDAD800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FA999200F7392B00F7392B00F7392B00FCCCC800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F9756B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E60000000000FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FDD3D0000000000000000000000000000000000000000000F7392B00F739
      2B00F7392B00F7392B00FEE8E600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FBA69F00F7392B00F7392B00F7392B00FCCCC800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FBB1AB00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCC3BF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E60000000000FEEE
      EC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B60000000000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FEEEEC000000000000000000000000000000000000000000F7392B00F739
      2B00F7392B00F7392B00FEE8E600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FBA69F00F7392B00F7392B00F7392B00FCCCC800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FA999200F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA6
      9F00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000FDDAD800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB6000000
      00000000000000000000FCCCC800F7392B00F7392B00F7392B00F7392B00FA8A
      8100000000000000000000000000000000000000000000000000FA8A8100F739
      2B00F7392B00F7392B00FCCCC800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FA8A8100F7392B00F7392B00F7392B00FCCCC800000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEF3F300F9756B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FA9992000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      000000000000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB600000000000000
      0000000000000000000000000000FDDAD800F9756B00F7392B00F7392B00FCBA
      B600000000000000000000000000000000000000000000000000FBB1AB00F739
      2B00F7392B00F7392B00FA999200000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00F7392B00F7392B00F7392B00F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEEEC000000
      000000000000FBA69F00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      00000000000000000000FA999200F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00FCBAB60000000000000000000000
      000000000000000000000000000000000000FEEEEC00F9756B00F7392B00FDE1
      DF00000000000000000000000000000000000000000000000000FDDAD800F739
      2B00F7392B00F7392B00F7392B00FEE8E6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FBB1AB00F7392B00F7392B00F7392B00F9756B0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDDA
      D800F7392B00F7392B00F7392B00F7392B00F7392B00FEE8E600000000000000
      0000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      00000000000000000000FEF3F300F9756B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00FCBAB6000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FA9992000000
      000000000000000000000000000000000000000000000000000000000000F975
      6B00F7392B00F7392B00F7392B00FA8A81000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDDA
      D800F7392B00F7392B00F7392B00F7392B00FCBAB60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCC3BF00F7392B00F7392B00F7392B00FDDAD8000000000000000000FDDA
      D800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000FEE8E600F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FCBAB600000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCCC
      C800F7392B00F7392B00F7392B00F7392B00FA999200FEF3F300000000000000
      0000000000000000000000000000000000000000000000000000FDDAD800F739
      2B00F7392B00F7392B00F7392B00F7392B00FEF3F30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FBB1AB00F7392B00FCBAB6000000000000000000FEEEEC00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA6
      9F0000000000000000000000000000000000F7392B00FEE8E600000000000000
      000000000000000000000000000000000000FCCCC800F7392B00F7392B00F739
      2B00F7392B00FCBAB60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FA8A8100F7392B00F7392B00F7392B00F7392B00F9756B00FCCCC8000000
      000000000000000000000000000000000000FEEEEC00FBB1AB00F7392B00F739
      2B00F7392B00F7392B00F7392B00FCCCC8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEE8E60000000000000000000000000000000000FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8
      E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEEE
      EC0000000000000000000000000000000000F7392B00FEE8E600000000000000
      00000000000000000000000000000000000000000000FBB1AB00F7392B00F739
      2B00FCBAB6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FA999200FBA69F00FBA69F00FA8A8100F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FBB1AB00000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000FA8A8100FCBA
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEEEEC00F9756B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FBB1AB0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEF3F300FBA69F00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCCCC8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000F7392B00FEE8E600000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDDAD800FBA69F00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FCBAB600FEF3
      F300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FCCCC800FEF3F300000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEF3F300FDDA
      D800FCCCC800FCCCC800FCCCC800FCCCC800FEE8E60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000080000000200000000100010000000000000200000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFFFFFFFFFFFFFF00000000
      FFFFFFFFFFFFFFFFFFFFFFC300000000FFFFFFFF00000000FFFFFF8300000000
      FFFFFFFF00000000FFFFFF0300000000FFFFFFFF00000000FFFFFE0300000000
      F000031F3FFFFFFFFF803C0700000000F000060F3FFFFFFFFE00000F00000000
      F0000E0720000001FC00001F00000000F0000C0320000001F800003F00000000
      F000180320000001F000007F00000000F07FF00120000001E03F80FF00000000
      F07FE00020000003E07FE07F00000000F07FE00020000003C0FFE07F00000000
      F07FE00020000003C1FFF03F00000000F07FFE0F20000007C1FFF83F00000000
      000FFE0F20000007C1FFF83F000000000007FE0F20000007C1FFF83F00000000
      0007FE0F20000807C1FFF83F00000000800FFE0F30001C0FC1FFF83F00000000
      801FFE0F38003E0FC1FFF03F00000000C018000F3C007F0FC0FFF07F00000000
      E030000F3C00FFDFE0FFE07F00000000F060000F3E01FFFFE03FC07F00000000
      F8C0000F3F03FFFFF01F00FF00000000FDE0000F3F87FFFFF00001FF00000000
      FFFFFFFF3FCFFFFFF80003FF00000000FFFFFFFF3FDFFFFFFC0007FF00000000
      FFFFFFFF3FFFFFFFFF000FFF00000000FFFFFFFF3FFFFFFFFFC07FFF00000000
      FFFFFFFFFFFFFFFFFFFFFFFF00000000FFFFFFFFFFFFFFFFFFFFFFFF00000000
      FFFFFFFFFFFFFFFFFFFFFFFF00000000}
  end
end
