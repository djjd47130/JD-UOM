object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion'
  ClientHeight = 661
  ClientWidth = 1104
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
    Left = 57
    Top = 0
    Width = 1047
    Height = 642
    ActivePage = tabConvert
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    ExplicitLeft = 63
    object tabConvert: TTabSheet
      Caption = '    Convert    '
      ExplicitLeft = 0
      ExplicitTop = 34
      ExplicitWidth = 1409
      ExplicitHeight = 586
      object pConvert: TPanel
        Left = 0
        Top = 0
        Width = 801
        Height = 608
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object Label9: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 592
          Width = 788
          Height = 13
          Margins.Right = 10
          Align = alBottom
          Caption = '(Double-click any item to copy to clipboard)'
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Tahoma'
          Font.Style = []
          ParentFont = False
          ExplicitWidth = 206
        end
        object lstEquivalents: TListView
          AlignWithMargins = True
          Left = 3
          Top = 153
          Width = 795
          Height = 433
          Align = alClient
          Columns = <
            item
              Caption = 'Equivalent Value'
              Width = 300
            end
            item
              Caption = 'Unit of Measure'
              Width = 200
            end
            item
              Caption = 'Suffix'
              Width = 90
            end
            item
              Caption = 'Systems'
              Width = 380
            end>
          HideSelection = False
          HotTrackStyles = [htHandPoint, htUnderlineHot]
          ReadOnly = True
          RowSelect = True
          TabOrder = 0
          ViewStyle = vsReport
          OnDblClick = lstEquivalentsDblClick
          ExplicitLeft = 2
          ExplicitTop = 152
        end
        object pConvertNormal: TPanel
          Left = 0
          Top = 0
          Width = 801
          Height = 97
          Align = alTop
          TabOrder = 1
          object Panel1: TPanel
            Left = 1
            Top = 1
            Width = 799
            Height = 42
            Align = alTop
            BevelOuter = bvNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -24
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 0
            object lblConvertTitle: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 134
              Height = 36
              Align = alLeft
              AutoSize = False
              Caption = 'Category:'
              Layout = tlCenter
            end
            object Label13: TLabel
              AlignWithMargins = True
              Left = 384
              Top = 3
              Width = 100
              Height = 36
              Align = alLeft
              AutoSize = False
              Caption = 'UOM:'
              Layout = tlCenter
              ExplicitLeft = 507
              ExplicitTop = 0
              ExplicitHeight = 27
            end
            object cboConvertCategory: TComboBox
              AlignWithMargins = True
              Left = 143
              Top = 3
              Width = 235
              Height = 37
              Align = alLeft
              Style = csDropDownList
              TabOrder = 0
              OnClick = cboConvertCategoryClick
              ExplicitLeft = 150
            end
            object cboConvertFromUnit: TComboBox
              AlignWithMargins = True
              Left = 490
              Top = 3
              Width = 306
              Height = 37
              Align = alClient
              Style = csDropDownList
              TabOrder = 1
              OnClick = cboConvertFromUnitClick
              ExplicitLeft = 455
              ExplicitWidth = 334
            end
          end
          object Panel3: TPanel
            Left = 1
            Top = 43
            Width = 799
            Height = 42
            Align = alTop
            BevelOuter = bvNone
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -24
            Font.Name = 'Tahoma'
            Font.Style = [fsBold]
            ParentFont = False
            TabOrder = 1
            object Label10: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 100
              Height = 36
              Align = alLeft
              AutoSize = False
              Caption = 'Value:'
              Layout = tlCenter
              ExplicitHeight = 27
            end
            object txtConvertFromValue: TRzSpinEdit
              AlignWithMargins = True
              Left = 109
              Top = 3
              Width = 687
              Height = 36
              BlankValue = 1.000000000000000000
              AllowKeyEdit = True
              ButtonWidth = 24
              Decimals = 15
              IntegersOnly = False
              Max = 1E50
              Min = -1E50
              Orientation = orHorizontal
              Value = 1.000000000000000000
              Align = alClient
              Alignment = taLeftJustify
              TabOrder = 0
              OnChange = txtConvertFromValueChange
              ExplicitLeft = 116
              ExplicitTop = 4
              ExplicitWidth = 673
            end
          end
        end
        object pConvertSearch: TPanel
          Left = 0
          Top = 97
          Width = 801
          Height = 53
          Align = alTop
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object Label5: TLabel
            AlignWithMargins = True
            Left = 4
            Top = 4
            Width = 100
            Height = 45
            Align = alLeft
            AutoSize = False
            Caption = 'Value:'
            Layout = tlCenter
            ExplicitLeft = 3
            ExplicitTop = 3
            ExplicitHeight = 27
          end
          object txtSearch: TSearchBox
            AlignWithMargins = True
            Left = 110
            Top = 4
            Width = 687
            Height = 45
            Align = alClient
            TabOrder = 0
            ExplicitLeft = 4
            ExplicitWidth = 793
            ExplicitHeight = 37
          end
        end
      end
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
        Width = 1039
        Height = 353
        Align = alClient
        TabOrder = 0
        ExplicitTop = 209
        ExplicitWidth = 620
        ExplicitHeight = 411
        object Chart: TChart
          AlignWithMargins = True
          Left = 11
          Top = 4
          Width = 1017
          Height = 345
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
          ExplicitWidth = 977
          ExplicitHeight = 323
          DesignSize = (
            1017
            345)
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 9
          object txtChartScale: TRzSpinEdit
            AlignWithMargins = True
            Left = 875
            Top = 2
            Width = 132
            Height = 27
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
            ExplicitLeft = 778
          end
          object chkNegative: TCheckBox
            Left = 747
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
            ExplicitLeft = 650
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
        Width = 1039
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
            HotTrackStyles = [htHandPoint, htUnderlineHot]
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
            HotTrackStyles = [htHandPoint, htUnderlineHot]
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
          Width = 397
          Height = 255
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 3
          ExplicitLeft = 669
          ExplicitWidth = 313
          object pTestVal: TPanel
            Left = 0
            Top = 0
            Width = 397
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
            Width = 391
            Height = 224
            Align = alClient
            TabOrder = 1
            ExplicitWidth = 307
            DesignSize = (
              391
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
              Width = 248
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
              Width = 248
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
              Width = 248
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
              Width = 248
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
              Width = 248
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
              Width = 248
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
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 57
    Height = 642
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -1
    ExplicitTop = 8
    ExplicitHeight = 565
    object btnConvertNormal: TJDFontButton
      Left = 0
      Top = 0
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'Normal Conversion (Lookup)'
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61561
      Image.Font.Charset = DEFAULT_CHARSET
      Image.Font.Color = clWindowText
      Image.Font.Height = -27
      Image.Font.Name = 'FontAwesome'
      Image.Font.Style = []
      Image.Font.Quality = fqAntialiased
      Image.StandardColor = fcBlue
      Overlay.Text = #57715
      Overlay.Font.Charset = DEFAULT_CHARSET
      Overlay.Font.Color = clWindowText
      Overlay.Font.Height = -7
      Overlay.Font.Name = 'FontAwesome'
      Overlay.Font.Style = []
      Overlay.Font.Quality = fqAntialiased
      Overlay.Position = foNone
      Overlay.Margin = 3
      ImagePosition = fpImgOnly
      ShowHint = True
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 0
      Text = 'btnConvertNormal'
      OnClick = btnConvertNormalClick
      ExplicitLeft = 2
      ExplicitTop = -6
    end
    object btnConvertSearch: TJDFontButton
      Left = 0
      Top = 49
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'Search Conversion (Auto-Detect)'
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61442
      Image.Font.Charset = DEFAULT_CHARSET
      Image.Font.Color = clWindowText
      Image.Font.Height = -27
      Image.Font.Name = 'FontAwesome'
      Image.Font.Style = []
      Image.Font.Quality = fqAntialiased
      Image.StandardColor = fcBlue
      Overlay.Text = #57715
      Overlay.Font.Charset = DEFAULT_CHARSET
      Overlay.Font.Color = clWindowText
      Overlay.Font.Height = -7
      Overlay.Font.Name = 'FontAwesome'
      Overlay.Font.Style = []
      Overlay.Font.Quality = fqAntialiased
      Overlay.Position = foNone
      Overlay.Margin = 3
      ImagePosition = fpImgOnly
      ShowHint = True
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 1
      Text = 'JDFontButton1'
      OnClick = btnConvertSearchClick
      ExplicitLeft = -1
      ExplicitTop = 43
    end
    object btnDetails: TJDFontButton
      Left = 0
      Top = 98
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'Unit of Measure Details (Statistics)'
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61950
      Image.Font.Charset = DEFAULT_CHARSET
      Image.Font.Color = clWindowText
      Image.Font.Height = -27
      Image.Font.Name = 'FontAwesome'
      Image.Font.Style = []
      Image.Font.Quality = fqAntialiased
      Image.StandardColor = fcBlue
      Overlay.Text = #57715
      Overlay.Font.Charset = DEFAULT_CHARSET
      Overlay.Font.Color = clWindowText
      Overlay.Font.Height = -7
      Overlay.Font.Name = 'FontAwesome'
      Overlay.Font.Style = []
      Overlay.Font.Quality = fqAntialiased
      Overlay.Position = foNone
      Overlay.Margin = 3
      ImagePosition = fpImgOnly
      ShowHint = True
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 2
      Text = 'JDFontButton1'
      OnClick = btnDetailsClick
      ExplicitLeft = 1
      ExplicitTop = 102
      ExplicitWidth = 73
    end
  end
  object Stat: TStatusBar
    Left = 0
    Top = 642
    Width = 1104
    Height = 19
    Cursor = crHandPoint
    Panels = <
      item
        Width = 150
      end
      item
        Text = 
          'UOM Library and Application Created by Jerry Dodge - https://git' +
          'hub.com/djjd47130/JD-UOM'
        Width = 50
      end>
    OnDblClick = StatDblClick
    ExplicitLeft = 512
    ExplicitTop = 296
    ExplicitWidth = 0
  end
end
