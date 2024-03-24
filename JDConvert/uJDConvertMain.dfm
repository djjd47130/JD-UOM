object frmJDConvertMain: TfrmJDConvertMain
  Left = 0
  Top = 0
  Caption = 'JD Unit-of-Measure Conversion'
  ClientHeight = 661
  ClientWidth = 1120
  Color = clBtnFace
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object Pages: TPageControl
    Left = 57
    Top = 0
    Width = 1063
    Height = 642
    ActivePage = tabScripts
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 0
    object tabConvert: TTabSheet
      Caption = '    Convert    '
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
          Top = 204
          Width = 795
          Height = 382
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
        end
        object pConvertNormal: TPanel
          Left = 0
          Top = 0
          Width = 801
          Height = 97
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 1
          object Panel1: TPanel
            Left = 0
            Top = 0
            Width = 801
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
            end
            object cboConvertFromUnit: TComboBox
              AlignWithMargins = True
              Left = 490
              Top = 3
              Width = 308
              Height = 37
              Align = alClient
              Style = csDropDownList
              TabOrder = 1
              OnClick = cboConvertFromUnitClick
            end
          end
          object Panel3: TPanel
            Left = 0
            Top = 42
            Width = 801
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
              Width = 134
              Height = 36
              Align = alLeft
              AutoSize = False
              Caption = 'Value:'
              Layout = tlCenter
            end
            object txtConvertFromValue: TRzSpinEdit
              AlignWithMargins = True
              Left = 143
              Top = 3
              Width = 655
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
              ExplicitHeight = 37
            end
          end
        end
        object pConvertSearch: TPanel
          Left = 0
          Top = 97
          Width = 801
          Height = 104
          Align = alTop
          BevelOuter = bvNone
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -24
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          ParentFont = False
          TabOrder = 2
          object Panel4: TPanel
            Left = 0
            Top = 0
            Width = 801
            Height = 49
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
            object Label5: TLabel
              AlignWithMargins = True
              Left = 3
              Top = 3
              Width = 134
              Height = 43
              Align = alLeft
              AutoSize = False
              Caption = 'Search:'
              Layout = tlCenter
              ExplicitTop = 0
            end
            object txtSearch: TSearchBox
              AlignWithMargins = True
              Left = 143
              Top = 3
              Width = 655
              Height = 43
              Align = alClient
              TabOrder = 0
              OnInvokeSearch = txtSearchInvokeSearch
              ExplicitHeight = 37
            end
          end
          object Panel5: TPanel
            Left = 0
            Top = 49
            Width = 801
            Height = 49
            Align = alTop
            TabOrder = 1
            object lblSearchFound: TLabel
              AlignWithMargins = True
              Left = 4
              Top = 4
              Width = 793
              Height = 41
              Align = alClient
              Alignment = taCenter
              AutoSize = False
              Caption = 'Enter your search above'
              Font.Charset = DEFAULT_CHARSET
              Font.Color = clWhite
              Font.Height = -24
              Font.Name = 'Tahoma'
              Font.Style = [fsBold]
              ParentFont = False
              Layout = tlCenter
              StyleElements = [seClient, seBorder]
              ExplicitLeft = 3
              ExplicitTop = 0
              ExplicitWidth = 134
              ExplicitHeight = 43
            end
          end
        end
      end
    end
    object tabDetails: TTabSheet
      Caption = '    Details    '
      ImageIndex = 1
      object pBottom: TPanel
        Left = 0
        Top = 255
        Width = 1055
        Height = 353
        Align = alClient
        TabOrder = 0
        object Chart: TChart
          AlignWithMargins = True
          Left = 11
          Top = 4
          Width = 1033
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
          Color = 1644825
          TabOrder = 0
          DesignSize = (
            1033
            345)
          DefaultCanvas = 'TGDIPlusCanvas'
          ColorPaletteIndex = 9
          object txtChartScale: TRzSpinEdit
            AlignWithMargins = True
            Left = 891
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
          end
          object chkNegative: TCheckBox
            Left = 763
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
        Width = 1055
        Height = 255
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
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
          end
        end
        object pInfo: TPanel
          Left = 666
          Top = 0
          Width = 389
          Height = 255
          Align = alClient
          BevelOuter = bvNone
          TabOrder = 3
          object pTestVal: TPanel
            Left = 0
            Top = 0
            Width = 389
            Height = 25
            Align = alTop
            BevelOuter = bvNone
            TabOrder = 0
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
            Width = 383
            Height = 224
            Align = alClient
            TabOrder = 1
            DesignSize = (
              383
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
              Width = 240
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
              Width = 240
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
              Width = 240
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
              Width = 240
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
              Width = 240
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
              Width = 240
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
              Width = 240
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
          end
        end
      end
    end
    object tabBuilder: TTabSheet
      Caption = '    UOM Builder    '
      ImageIndex = 2
      object lstCustomUOMs: TListView
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1049
        Height = 413
        Align = alClient
        Color = clBlack
        Columns = <
          item
            Caption = 'Custom UOM Name'
            Width = 220
          end
          item
            Caption = 'Type'
            Width = 100
          end
          item
            Caption = 'Category'
            Width = 180
          end
          item
            Caption = 'Suffix'
            Width = 100
          end
          item
            Caption = 'Systems'
            Width = 400
          end>
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWhite
        Font.Height = -16
        Font.Name = 'Tahoma'
        Font.Style = [fsBold]
        HideSelection = False
        HotTrackStyles = [htHandPoint, htUnderlineHot]
        ReadOnly = True
        RowSelect = True
        ParentFont = False
        TabOrder = 0
        ViewStyle = vsReport
        OnSelectItem = lstCustomUOMsSelectItem
      end
      object pEditUOM: TPanel
        Left = 0
        Top = 419
        Width = 1055
        Height = 189
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 1
        object lblUserType: TLabel
          Left = 18
          Top = 18
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Item Type:'
        end
        object lblUserNameSingular: TLabel
          Left = 18
          Top = 84
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name (Singular):'
        end
        object lblUserNamePlural: TLabel
          Left = 395
          Top = 84
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Name (Plural):'
        end
        object lblUserCategory: TLabel
          Left = 18
          Top = 48
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Category:'
        end
        object lblUserSuffix: TLabel
          Left = 18
          Top = 117
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Suffix:'
        end
        object lblUserSystems: TLabel
          Left = 395
          Top = 117
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'System(s):'
        end
        object lblUserFrom: TLabel
          Left = 18
          Top = 150
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Convert From Base:'
        end
        object lblUserTo: TLabel
          Left = 386
          Top = 150
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Convert To Base:'
        end
        object lblUserBase: TLabel
          Left = 393
          Top = 48
          Width = 161
          Height = 19
          Alignment = taRightJustify
          AutoSize = False
          Caption = 'Category Base:'
        end
        object cboUserType: TComboBox
          Left = 194
          Top = 15
          Width = 193
          Height = 27
          Style = csDropDownList
          TabOrder = 0
          OnClick = cboUserTypeClick
          Items.Strings = (
            'Metric'
            'Simple'
            'Formula')
        end
        object txtUserNameSingular: TEdit
          Left = 194
          Top = 81
          Width = 193
          Height = 27
          TabOrder = 2
        end
        object txtUserNamePlural: TEdit
          Left = 562
          Top = 81
          Width = 193
          Height = 27
          TabOrder = 3
        end
        object txtUserCategory: TEdit
          Left = 194
          Top = 48
          Width = 193
          Height = 27
          TabOrder = 1
        end
        object txtUserSuffix: TEdit
          Left = 194
          Top = 114
          Width = 193
          Height = 27
          TabOrder = 4
        end
        object txtUserSystems: TEdit
          Left = 562
          Top = 114
          Width = 193
          Height = 27
          TabOrder = 5
        end
        object Panel2: TPanel
          Left = 1017
          Top = 0
          Width = 38
          Height = 189
          Align = alRight
          TabOrder = 8
          object btnNewUOM: TJDFontButton
            Left = 1
            Top = 1
            Width = 36
            Height = 33
            Cursor = crHandPoint
            Hint = 'Create New UOM'
            Align = alTop
            DrawStyle = fdTransparent
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            Image.AutoSize = False
            Image.Text = #61543
            Image.Font.Charset = DEFAULT_CHARSET
            Image.Font.Color = clWindowText
            Image.Font.Height = -21
            Image.Font.Name = 'FontAwesome'
            Image.Font.Style = []
            Image.Font.Quality = fqAntialiased
            Image.StandardColor = fcGreen
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
            OnClick = btnNewUOMClick
          end
          object btnEditUOM: TJDFontButton
            Left = 1
            Top = 34
            Width = 36
            Height = 33
            Cursor = crHandPoint
            Hint = 'Edit Selected UOM'
            Align = alTop
            DrawStyle = fdTransparent
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            Image.AutoSize = False
            Image.Text = #61504
            Image.Font.Charset = DEFAULT_CHARSET
            Image.Font.Color = clWindowText
            Image.Font.Height = -21
            Image.Font.Name = 'FontAwesome'
            Image.Font.Style = []
            Image.Font.Quality = fqAntialiased
            Image.StandardColor = fcYellow
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
            Text = 'btnConvertNormal'
            OnClick = btnEditUOMClick
          end
          object btnDeleteUOM: TJDFontButton
            Left = 1
            Top = 67
            Width = 36
            Height = 33
            Cursor = crHandPoint
            Hint = 'Delete Selected UOM'
            Align = alTop
            DrawStyle = fdTransparent
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            Image.AutoSize = False
            Image.Text = #61544
            Image.Font.Charset = DEFAULT_CHARSET
            Image.Font.Color = clWindowText
            Image.Font.Height = -21
            Image.Font.Name = 'FontAwesome'
            Image.Font.Style = []
            Image.Font.Quality = fqAntialiased
            Image.StandardColor = fcRed
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
            Text = 'btnConvertNormal'
          end
          object btnSaveUOM: TJDFontButton
            Left = 1
            Top = 100
            Width = 36
            Height = 33
            Cursor = crHandPoint
            Hint = 'Save Changes to UOM'
            Align = alTop
            DrawStyle = fdTransparent
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            Image.AutoSize = False
            Image.Text = #61452
            Image.Font.Charset = DEFAULT_CHARSET
            Image.Font.Color = clWindowText
            Image.Font.Height = -21
            Image.Font.Name = 'FontAwesome'
            Image.Font.Style = []
            Image.Font.Quality = fqAntialiased
            Image.StandardColor = fcGreen
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
            TabOrder = 3
            Text = 'btnConvertNormal'
            OnClick = btnSaveUOMClick
          end
          object btnCancelUOM: TJDFontButton
            Left = 1
            Top = 133
            Width = 36
            Height = 33
            Cursor = crHandPoint
            Hint = 'Cancel Changes to UOM'
            Align = alTop
            DrawStyle = fdTransparent
            Enabled = False
            Font.Charset = DEFAULT_CHARSET
            Font.Color = clWindowText
            Font.Height = -16
            Font.Name = 'Tahoma'
            Font.Style = []
            Image.AutoSize = False
            Image.Text = #61453
            Image.Font.Charset = DEFAULT_CHARSET
            Image.Font.Color = clWindowText
            Image.Font.Height = -21
            Image.Font.Name = 'FontAwesome'
            Image.Font.Style = []
            Image.Font.Quality = fqAntialiased
            Image.StandardColor = fcRed
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
            TabOrder = 4
            Text = 'btnConvertNormal'
            OnClick = btnCancelUOMClick
          end
        end
        object txtUserFrom: TEdit
          Left = 194
          Top = 147
          Width = 193
          Height = 27
          TabOrder = 6
        end
        object txtUserTo: TEdit
          Left = 562
          Top = 147
          Width = 193
          Height = 27
          TabOrder = 7
        end
        object lstUserUnits: TCheckListBox
          Left = 784
          Top = 16
          Width = 185
          Height = 161
          ItemHeight = 19
          TabOrder = 9
        end
        object chkUserBase: TToggleSwitch
          Left = 562
          Top = 49
          Width = 98
          Height = 21
          StateCaptions.CaptionOn = 'True'
          StateCaptions.CaptionOff = 'False'
          TabOrder = 10
        end
      end
    end
    object tabScripts: TTabSheet
      Caption = '    Scripts    '
      ImageIndex = 4
      ExplicitLeft = 6
      ExplicitTop = 28
      object Splitter1: TSplitter
        Left = 0
        Top = 353
        Width = 1055
        Height = 7
        Cursor = crVSplit
        Align = alBottom
        Beveled = True
        ExplicitTop = 408
      end
      object Panel7: TPanel
        Left = 0
        Top = 360
        Width = 1055
        Height = 248
        Align = alBottom
        BevelOuter = bvNone
        TabOrder = 0
        object Button1: TButton
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 1049
          Height = 25
          Align = alTop
          Caption = 'Evaluate Expression'
          Default = True
          TabOrder = 0
          OnClick = Button1Click
        end
        object txtOutput: TMemo
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 1049
          Height = 211
          Align = alClient
          ScrollBars = ssVertical
          TabOrder = 1
          ExplicitHeight = 146
        end
      end
      object txtExpr: TMemo
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 1049
        Height = 347
        Align = alClient
        Lines.Strings = (
          'var OutUOM: String;'
          'var V, T: Float;'
          'var Res: String;'
          ''
          'V:= UOM('#39'3ft'#39') / UOM('#39'6.9cm'#39') + Sqr(46);'
          ''
          'Res:= UOMString(V, '#39'Meter'#39')+'#39' is the same as:'#39';'
          'PrintLn(Res);'
          ''
          'OutUOM:= '#39'ft'#39';'
          'T:= Convert(V, '#39'Meter'#39', OutUOM);'
          'Res:= UOMString(T, OutUOM);'
          'PrintLn(Res);'
          ''
          'OutUOM:= '#39'ly'#39';'
          'T:= Convert(V, '#39'Meter'#39', OutUOM);'
          'Res:= UOMString(T, OutUOM);'
          'PrintLn(Res);')
        ScrollBars = ssVertical
        TabOrder = 1
        ExplicitHeight = 366
      end
    end
    object tabSettings: TTabSheet
      Caption = '    Settings    '
      ImageIndex = 3
      ExplicitLeft = 6
      ExplicitTop = 28
      object Panel6: TPanel
        Left = 0
        Top = 0
        Width = 1055
        Height = 41
        Align = alTop
        TabOrder = 0
        object Label15: TLabel
          AlignWithMargins = True
          Left = 4
          Top = 4
          Width = 309
          Height = 33
          Align = alLeft
          AutoSize = False
          Caption = 'Custom UOM File Location'
        end
        object Edit1: TEdit
          AlignWithMargins = True
          Left = 319
          Top = 4
          Width = 691
          Height = 33
          Align = alClient
          TabOrder = 0
          Text = 'Edit1'
          ExplicitHeight = 27
        end
        object JDFontButton1: TJDFontButton
          AlignWithMargins = True
          Left = 1016
          Top = 4
          Width = 35
          Height = 33
          Cursor = crHandPoint
          Align = alRight
          DrawStyle = fdTransparent
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clWindowText
          Font.Height = -16
          Font.Name = 'Tahoma'
          Font.Style = [fsBold]
          Image.AutoSize = False
          Image.Text = #61564
          Image.Font.Charset = DEFAULT_CHARSET
          Image.Font.Color = clWindowText
          Image.Font.Height = -21
          Image.Font.Name = 'FontAwesome'
          Image.Font.Style = []
          Image.Font.Quality = fqAntialiased
          Image.StandardColor = fcOrange
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
          SubTextFont.Charset = DEFAULT_CHARSET
          SubTextFont.Color = clGray
          SubTextFont.Height = -11
          SubTextFont.Name = 'Tahoma'
          SubTextFont.Style = []
          TabOrder = 1
          Text = 'JDFontButton1'
        end
      end
    end
  end
  object pMenu: TPanel
    Left = 0
    Top = 0
    Width = 57
    Height = 642
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitLeft = -2
    ExplicitTop = -6
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
    end
    object btnUOMBuilder: TJDFontButton
      Left = 0
      Top = 147
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'Build Custom UOMs'
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61875
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
      TabOrder = 3
      Text = 'btnUOMBuilder'
      OnClick = btnUOMBuilderClick
    end
    object btnSettings: TJDFontButton
      Left = 0
      Top = 593
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'App Settings'
      Align = alBottom
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61459
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
      TabOrder = 4
      Text = 'Settings'
      Visible = False
      OnClick = btnSettingsClick
    end
    object btnUOMScript: TJDFontButton
      Left = 0
      Top = 196
      Width = 57
      Height = 49
      Cursor = crHandPoint
      Hint = 'Execute UOM Conversion Scripts'
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61788
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
      TabOrder = 5
      Text = 'btnUOMBuilder'
      OnClick = btnUOMScriptClick
      ExplicitLeft = -2
      ExplicitTop = 283
    end
  end
  object Stat: TStatusBar
    Left = 0
    Top = 642
    Width = 1120
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
  end
  object ApplicationEvents1: TApplicationEvents
    OnHelp = ApplicationEvents1Help
    Left = 413
    Top = 254
  end
end
