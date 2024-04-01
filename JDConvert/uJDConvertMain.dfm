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
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 19
  object Pages: TPageControl
    Left = 57
    Top = 0
    Width = 1063
    Height = 642
    ActivePage = tabDetails
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
      inline UOMDetails: TfrJDConvertDetails
        Left = 0
        Top = 0
        Width = 1055
        Height = 608
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 1055
        ExplicitHeight = 608
        inherited pBottom: TPanel
          Width = 1055
          Height = 353
          ExplicitWidth = 1055
          ExplicitHeight = 353
          inherited Chart: TChart
            Width = 1033
            Height = 345
            ExplicitWidth = 1033
            ExplicitHeight = 345
            inherited txtChartScale: TRzSpinEdit
              Left = 891
              Height = 27
              OnChange = UOMDetailstxtChartScaleChange
              ExplicitLeft = 891
              ExplicitHeight = 27
            end
            inherited chkNegative: TCheckBox
              Left = 635
              Top = 6
              ExplicitLeft = 635
              ExplicitTop = 6
            end
            inherited txtChartFreq: TRzSpinEdit
              Left = 753
              Height = 27
              OnChange = UOMDetailstxtChartScaleChange
              ExplicitLeft = 753
              ExplicitHeight = 27
            end
          end
        end
        inherited pTop: TPanel
          Width = 1055
          ExplicitWidth = 1055
          inherited pCategories: TPanel
            inherited lstCategories: TListView
              SmallImages = Img32
            end
          end
          inherited pUOMs: TPanel
            Width = 240
            ExplicitWidth = 240
            inherited Label2: TLabel
              Width = 234
            end
            inherited lstUOMs: TListView
              Width = 234
              ExplicitWidth = 234
            end
          end
          inherited pInfo: TPanel
            Left = 689
            Width = 366
            ExplicitLeft = 689
            ExplicitWidth = 366
            inherited pTestVal: TPanel
              Width = 366
              ExplicitWidth = 366
            end
            inherited pUnitDetail: TPanel
              Width = 360
              ExplicitWidth = 360
              inherited lblUnitName: TLabel
                Width = 217
                ExplicitWidth = 240
              end
              inherited lblUnitSystems: TLabel
                Width = 217
                ExplicitWidth = 240
              end
              inherited lblUnitSuffix: TLabel
                Width = 217
                ExplicitWidth = 240
              end
              inherited lblUnitBaseFrom: TLabel
                Width = 217
                ExplicitWidth = 217
              end
              inherited lblUnitBaseTo: TLabel
                Width = 217
                ExplicitWidth = 217
              end
              inherited lblUnitNamePlural: TLabel
                Width = 217
                ExplicitWidth = 240
              end
              inherited lblUnitAliases: TLabel
                Width = 217
                ExplicitWidth = 217
              end
            end
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
            Width = 80
          end
          item
            Caption = 'Category'
            Width = 160
          end
          item
            Caption = 'Suffix'
            Width = 100
          end
          item
            Caption = 'Systems'
            Width = 380
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
            OnClick = btnDeleteUOMClick
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
      inline Scripting: TfrJDConvertScripting
        Left = 0
        Top = 0
        Width = 1055
        Height = 608
        Align = alClient
        TabOrder = 0
        ExplicitWidth = 1055
        ExplicitHeight = 608
        inherited Splitter1: TSplitter
          Top = 353
          Width = 1055
        end
        inherited Panel7: TPanel
          Top = 360
          Width = 1055
          ExplicitTop = 360
          ExplicitWidth = 1055
          inherited txtOutput: TSynEdit
            Width = 1049
            ExplicitWidth = 1049
          end
        end
        inherited txtExpr: TSynEdit
          Width = 1049
          Height = 287
          ExplicitWidth = 1049
          ExplicitHeight = 287
        end
        inherited pJDConvertScriptingToolbar: TPanel
          Width = 1055
          ExplicitWidth = 1055
          inherited btnNew: TJDFontButton
            Width = 33
            ExplicitWidth = 33
          end
          inherited btnOpen: TJDFontButton
            Left = 42
            ExplicitLeft = 42
          end
          inherited btnSave: TJDFontButton
            Left = 128
            ExplicitLeft = 128
          end
          inherited btnSaveAs: TJDFontButton
            Left = 171
            ExplicitLeft = 171
          end
          inherited btnExec: TJDFontButton
            Left = 214
            ExplicitLeft = 214
          end
          inherited btnOpenRecent: TJDFontButton
            Left = 85
            ExplicitLeft = 85
          end
          inherited JDFontButton2: TJDFontButton
            Left = 257
            ExplicitLeft = 257
          end
        end
        inherited Stat: TStatusBar
          Top = 334
          Width = 1055
          ExplicitLeft = 0
          ExplicitTop = 334
          ExplicitWidth = 1055
        end
      end
    end
    object tabSettings: TTabSheet
      Caption = '    Settings    '
      ImageIndex = 3
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
    end
    object btnTESTIGNORE: TJDFontButton
      Left = 0
      Top = 544
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
      Image.Text = #61541
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
      TabOrder = 6
      Text = 'TEST IGNORE'
      Visible = False
      ExplicitLeft = 2
      ExplicitTop = 520
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
  object AppEvents: TApplicationEvents
    OnHelp = AppEventsHelp
    Left = 421
    Top = 302
  end
  object Glyphs: TJDFontGlyphs
    ImageLists = <
      item
        ImageList = Img32
      end>
    Glyphs = <
      item
        Caption = 'Distance'
        Glyph = #61541
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61541
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end
      item
        Caption = 'Area'
        Glyph = #61511
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61511
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end
      item
        Caption = 'Volume'
        Glyph = #61874
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61874
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end
      item
        Caption = 'Mass'
        Glyph = #61754
        Color = fcBlue
        Scale = 0.960000000000000000
        Ref.FontName = 'FontAwesome'
        Ref.Glyph = #61754
        Ref.Color = clBlue
        Ref.StandardColor = fcBlue
        Ref.UseStandardColor = True
      end>
    Left = 157
    Top = 486
  end
  object Img32: TImageList
    Height = 32
    Width = 32
    Left = 205
    Top = 486
    Bitmap = {
      494C010104000800040020002000FFFFFFFFFF00FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000800000004000000001002000000000000080
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
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEEEEC00F9756B00F9756B00FEEEEC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEE8E600FBA69F00F7392B00F7392B00FBA69F00FEEEEC00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEE8E600FEEE
      EC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEEEEC00F9756B00F7392B00F7392B00F9756B00FEEEEC000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEF3F300FBA6
      9F00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB600FEF3
      F300000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEEEEC00FEF3F30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FA99
      9200F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCCCC8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00F9756B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEE
      EC00000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEF3F300FCBAB600F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00FBA69F00F7392B00F7392B00F739
      2B00FCCCC8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDDA
      D800FBB1AB00F9756B00F7392B00F7392B00F7392B00F7392B00FA8A8100FCBA
      B600FDE1DF000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FDD3D0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F975
      6B00FEEEEC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F9756B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B0000000000FEE8E600FBA69F00F739
      2B00F7392B00F9756B00FCCCC800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCCCC800F9756B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FA8A8100FDE1DF00000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEF3F300F975
      6B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F9756B00FEF3F30000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FDD3D000F9756B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00000000000000000000000000FDDA
      D800FA8A8100F7392B00F7392B00FA8A8100FDDAD80000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FEF3F300FA999200F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00FCBAB6000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FEEEEC0000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDDA
      D800FA8A8100F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      000000000000FDD3D000F9756B00F7392B00F7392B00FA999200FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FEEEEC00F9756B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00FA99920000000000000000000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FDDAD8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEF3
      F300FEE8E600FDD3D000F7392B00F7392B00F7392B00F7392B00FDD3D000FEE8
      E600FEF3F3000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEEEEC00FBA69F00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000FCCCC800F9756B00F7392B00F7392B00FBA6
      9F00000000000000000000000000000000000000000000000000000000000000
      000000000000FEF3F300F9756B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA69F00000000000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FDDA
      D800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FA8A8100F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      000000000000000000000000000000000000FEF3F300FCBAB600F7392B00F739
      2B00FBB1AB000000000000000000000000000000000000000000000000000000
      000000000000FBA69F00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00FA999200FA8A8100F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCCCC8000000
      000000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FDDAD8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FDE1DF00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000FDDAD800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FCBAB6000000000000000000FA999200F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FEF3
      F30000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00FCBAB600FEEEEC00F9756B00F7392B00F7392B00F739
      2B00F7392B00FDDAD80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEEEEC00FCC3BF00000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      00000000000000000000000000000000000000000000FCC3BF00FEEEEC000000
      00000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000FA8A8100F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FCBAB60000000000000000000000000000000000FA999200F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B60000000000000000000000000000000000000000000000000000000000F739
      2B00F7392B00FCBAB6000000000000000000FEEEEC00F9756B00F7392B00F739
      2B00F7392B00F7392B00FDDAD800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00F9756B00F7392B00FCCCC8000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      000000000000000000000000000000000000FCCCC800F7392B00F9756B00FEEE
      EC000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FDE1
      DF00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCBAB600000000000000000000000000000000000000000000000000FA99
      9200F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B0000000000000000000000000000000000000000000000000000000000FDE1
      DF00FDDAD80000000000000000000000000000000000FEEEEC00F9756B00F739
      2B00F7392B00F7392B00F7392B00FDE1DF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00F9756B00F7392B00F7392B00FCCCC8000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      000000000000000000000000000000000000FCCCC800F7392B00F7392B00F975
      6B00FEEEEC0000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FCBA
      B600F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B600000000000000000000000000000000000000000000000000000000000000
      0000FA999200F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FDE1DF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00F975
      6B00F7392B00F7392B00F7392B00FDDAD8000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00F975
      6B00F7392B00F7392B00F7392B00FBA69F00FCCCC800FCCCC800FCCCC800FCCC
      C800FCCCC800FCBAB600F7392B00F7392B00F7392B00F7392B00FCBAB600FCCC
      C800FCCCC800FCCCC800FCCCC800FCCCC800FBA69F00F7392B00F7392B00F739
      2B00F9756B00FEEEEC00000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FA99
      9200F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB6000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FA999200F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCCCC8000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00F9756B00F7392B00FDDAD800000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEEEEC00F9756B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F9756B00FEEEEC000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBAB600000000000000
      0000000000000000000000000000FEF3F3000000000000000000000000000000
      00000000000000000000FA999200F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00FDE1DF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F9756B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F9756B000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00F7392B00FCBAB60000000000000000000000
      00000000000000000000FEEEEC00F9756B00FCBAB60000000000000000000000
      0000000000000000000000000000FA999200F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEEEEC00F9756B00FDDA
      D800000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCBAB600F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FCBAB6000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00FA999200FDD3D000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000F739
      2B00F7392B00F7392B00F7392B00FCBAB6000000000000000000000000000000
      000000000000FEEEEC00F9756B00F7392B00F7392B00FCBAB600000000000000
      000000000000000000000000000000000000FA999200F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEEEEC00F9756B00F7392B00F739
      2B00FDDAD8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FCBAB600F739
      2B00F7392B00F7392B00F7392B00FA8A8100FBA69F00FBA69F00FBA69F00FBA6
      9F00FBA69F00FA999200F7392B00F7392B00F7392B00F7392B00FA999200FBA6
      9F00FBA69F00FBA69F00FBA69F00FBA69F00FA8A8100F7392B00F7392B00F739
      2B00F7392B00FCBAB600000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBB1
      AB00FDE1DF000000000000000000000000000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FA8A
      8100F7392B00F7392B00F7392B00FEE8E6000000000000000000000000000000
      0000FEEEEC00F9756B00F7392B00F7392B00F7392B00F7392B00FCBAB6000000
      000000000000000000000000000000000000FCCCC800F7392B00F7392B00F739
      2B00FCC3BF000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FA999200F7392B00F7392B00F739
      2B00F7392B00FDDAD80000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCBA
      B600F7392B00F7392B00F7392B00FCCCC8000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      000000000000000000000000000000000000FCCCC800F7392B00F7392B00F739
      2B00FCBAB60000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FA999200FDD3D00000000000FEF3F300FCCCC800FA8A8100F7392B00F739
      2B00F7392B00F9756B00FCBAB600FEEEEC000000000000000000FEE8E600F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FBB1
      AB00F7392B00F7392B00F7392B00FA999200000000000000000000000000FEEE
      EC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B600000000000000000000000000FEEEEC00F9756B00F7392B00F7392B00F739
      2B00FDD3D0000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEEEEC00F9756B00F7392B00F739
      2B00F7392B00F7392B00FDDAD800000000000000000000000000FDDAD800F739
      2B00FCC3BF000000000000000000000000000000000000000000000000000000
      0000FCBAB600F7392B00F7392B00FCCCC8000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      000000000000000000000000000000000000FCCCC800F7392B00F7392B00FCBA
      B6000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FA8A8100FCC3BF00FEF3
      F300000000000000000000000000000000000000000000000000FEEEEC00FCBA
      B600F9756B00F7392B00F7392B00F7392B00FA8A8100FCCCC800FDDAD800F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FDD3
      D000F7392B00F7392B00F7392B00F7392B00FA99920000000000FEEEEC00F975
      6B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCBAB60000000000FEEEEC00F9756B00F7392B00F7392B00F7392B00F739
      2B00FEEEEC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FEEEEC00F9756B00F739
      2B00F7392B00F7392B00F7392B00FDDAD80000000000FDDAD800F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      000000000000FCBAB600F7392B00FEE8E6000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      000000000000000000000000000000000000FEE8E600F7392B00FCBAB6000000
      00000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00F7392B00F7392B00F9756B00FCBAB600FEE8E60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FDE1DF00FBB1AB00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F00000000000000000000000000000000000000000000000000FEF3
      F300F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FA99
      9200000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00F975
      6B00F7392B00F7392B00F7392B00F7392B00FCBAB600F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F7392B00F7392B00F739
      2B00FBB1AB00FDDAD80000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDDAD800FA999200F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FDDA
      D800000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FEEE
      EC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000F9756B00F7392B00FA99
      9200FDDAD8000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FDD3D000FA8A8100F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000FEF3F300F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FBA69F000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEEEEC00F9756B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEE8E600F7392B00F7392B00F7392B00F7392B00FEE8E6000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000FCCCC800F7392B00F739
      2B00F7392B00F9756B00FCBAB600FDE1DF000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FDDAD800FBB1AB00F7392B00F7392B00F7392B00F739
      2B00FDE1DF000000000000000000000000000000000000000000000000000000
      000000000000FDDAD800F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F9756B00FEEEEC000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCCCC800F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FBB1
      AB00FBA69F00FA999200F7392B00F7392B00F7392B00F7392B00FA999200FBA6
      9F00FBB1AB000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEEEEC00FCC3
      BF00FA8A8100F7392B00F7392B00F7392B00FA8A8100FCC3BF00FEEEEC000000
      000000000000000000000000000000000000000000000000000000000000FEE8
      E600FCBAB600F9756B00F7392B00F7392B00F7392B00FA8A8100FCCCC800FEF3
      F300000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FCBAB600F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00FDE1DF00000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FDDAD800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEE8E600F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00FEE8E60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FDE1DF00FCBAB600F9756B00F7392B00F7392B00F7392B00FA99
      9200FDD3D000FEF3F3000000000000000000FEF3F300FCC3BF00FA8A8100F739
      2B00F7392B00F7392B00F9756B00FCBAB600FEEEEC0000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000FCCCC800F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00F7392B00F9756B00FEE8E60000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDDA
      D800F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FCBA
      B600F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FCBAB6000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FDDAD800FBB1AB00F7392B00F739
      2B00F7392B00F7392B00FBB1AB00FA999200F7392B00F7392B00F7392B00F739
      2B00FBB1AB00FDE1DF0000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000FEE8E600FA8A8100F7392B00F739
      2B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00F7392B00F7392B00FBA69F00FEF3F3000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FA8A
      8100F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBA69F000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FCBAB600F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00FCBA
      B600000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FEF3F300FDD3
      D000FA999200F7392B00F7392B00F7392B00F7392B00FA999200FDDAD8000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000FDDAD800FA8A
      8100F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F7392B00F739
      2B00FBB1AB00FEE8E60000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000FDE1
      DF00FCCCC800FCCCC800FCCCC800FCCCC800FCCCC800FCCCC800FCCCC800FCCC
      C800FEEEEC000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FCBAB600F7392B00F7392B00F7392B00F7392B00FCBAB6000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000FEEEEC00FCCCC800FDD3D000FEF3F30000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000FEF3F300FDDAD800FCCCC800FCCCC800FCCCC800FCCCC800FDE1DF000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000FCBAB600F7392B00F7392B00FCBAB600000000000000
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
      0000000000000000000000000000FCC3BF00FCC3BF0000000000000000000000
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
      2800000080000000400000000100010000000000000400000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
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
      00000000000000000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFC3FFFFFF03FFFFFFFFFFFF00FFFFFFFF81FFFFFC00FFFFFFE7FFF
      E007FFFFFFF00FFFFF0007FFFFE007FFE007FFFFFFE007FFFE0081FFFF8001FF
      E00FFFFFFFC003FFF800E07FFE0000FFE01FFFFFFFC003FFE000F81FFC00007F
      E01FFFFFFFE007FF8000FE0FF800003FE00FFFFFFFF81FFF8000FF07F800001F
      E007FFFFFFF81FFF8000FFC7F001800FE003FFFFF9F81F9F8000FFC7F003C00F
      E301FFFFF0F81F0F8000FFC7E007E00FE780FFFFE0F81F078000FFC7E00FF007
      FFC0FFFFC00000038000FFC7E01FF807FFE1FFFF800000018000FFC7E03EFC07
      FFF3FFFF800000018000FFC7E07C7E07FFFF8FFF8000000180003FC7E0F83F07
      FFFF07FFC0000003800007C7E0F01F07FFFF03FFE0F81F07800200C7E0E00E07
      FFFF01C7F0F81F0F800FC007E0400407FFFF8087F8F81F1F807FF807E000000F
      FFFFC007FFF81FFF83FFFF07F000000FFFFFE007FFF81FFF87FFFF87F000001F
      FFFFF007FFF81FFF80FFFC07F800001FFFFFF807FFE007FFC01FE00FFC00003F
      FFFFF007FFC003FFF803007FFE00007FFFFFE007FFE007FFFF0003FFFF0000FF
      FFFFE007FFF00FFFFFC01FFFFFC003FFFFFFE007FFF81FFFFFF87FFFFFF01FFF
      FFFFFFFFFFFC3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE7FFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
  end
end
