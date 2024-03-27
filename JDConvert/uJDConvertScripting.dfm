object frJDConvertScripting: TfrJDConvertScripting
  Left = 0
  Top = 0
  Width = 923
  Height = 613
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 358
    Width = 923
    Height = 7
    Cursor = crVSplit
    Align = alBottom
    Beveled = True
    ExplicitLeft = -193
    ExplicitTop = 353
    ExplicitWidth = 1055
  end
  object Panel7: TPanel
    Left = 0
    Top = 365
    Width = 923
    Height = 248
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object txtOutput: TSynEdit
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 917
      Height = 242
      Align = alClient
      DoubleBuffered = True
      Color = 1644825
      ActiveLineColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      ParentDoubleBuffered = False
      TabOrder = 0
      UseCodeFolding = False
      Gutter.Color = 1644825
      Gutter.BorderColor = clSilver
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clWhite
      Gutter.Font.Height = -16
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.ShowLineNumbers = True
      Gutter.Gradient = True
      Gutter.GradientStartColor = clBlack
      Gutter.GradientEndColor = 1644825
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
      Options = [eoAutoIndent, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoShowLigatures]
      ReadOnly = True
      SelectedColor.Alpha = 0.400000005960464500
      WantTabs = True
      ExplicitTop = 39
      ExplicitHeight = 206
    end
  end
  object txtExpr: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 917
    Height = 292
    Align = alClient
    DoubleBuffered = True
    Color = 1644825
    ActiveLineColor = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    ParentDoubleBuffered = False
    TabOrder = 1
    OnClick = txtExprClick
    OnKeyUp = txtExprKeyUp
    UseCodeFolding = False
    Gutter.Color = 1644825
    Gutter.BorderColor = clSilver
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWhite
    Gutter.Font.Height = -16
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    Gutter.Font.Quality = fqClearTypeNatural
    Gutter.ShowLineNumbers = True
    Gutter.Gradient = True
    Gutter.GradientStartColor = clBlack
    Gutter.GradientEndColor = 1644825
    Gutter.Bands = <
      item
        Kind = gbkMarks
        Width = 13
      end
      item
        Kind = gbkLineNumbers
      end
      item
        Kind = gbkFold
      end
      item
        Kind = gbkTrackChanges
      end
      item
        Kind = gbkMargin
        Width = 3
      end>
    Highlighter = SynPasSyn1
    Lines.Strings = (
      '//Using Distance UOM category...'
      'const CAT: String = '#39'Distance'#39';'
      ''
      '//Register Jerry Distance UOM, if doesn'#39't exist...'
      'if not UOMExists('#39'Jerry'#39') then'
      
        '  RegisterSimpleUOM(CAT, '#39'Jerry'#39', '#39'Jerrys'#39', '#39'Jry'#39', '#39'Random'#39', 1.7' +
        '272, '#39#39');'
      ''
      '//Perform random calculation on different UOMs...'
      
        'var V: Float = (UOM('#39'3ft'#39') / UOM('#39'6.9cm'#39')) + (Sqr(46) - UOM('#39'6yd' +
        #39'));'
      ''
      
        '//Central procedure to print out converted version of V (Value).' +
        '..'
      'procedure OutputConversion(const UOM: String);'
      'begin'
      
        '  PrintLn(UOMString(Convert(V, BaseUOM(CAT).NameSingular, UOM), ' +
        'UOM) + '#39' ('#39'+FindUOM(UOM).Suffix+'#39')'#39');'
      'end;'
      ''
      '//Print base value and UOM message...'
      
        'PrintLn(UOMString(V, BaseUOM(CAT).NameSingular)+'#39' ('#39'+BaseUOM(CAT' +
        ').Suffix+'#39') is the same as:'#39');'
      ''
      '//Print conversion for each possible UOM in category...'
      'var U: TUOM;'
      'for var X:= 0 to UOMCount - 1 do begin'
      '  U:= UOMByIndex(X);'
      '  //UOM is same category, and not the base...'
      
        '  if (U.Category = CAT) and (U.NameSingular <> BaseUOM(CAT).Name' +
        'Singular) then'
      '    OutputConversion(U.NameSingular);'
      'end;')
    Options = [eoAutoIndent, eoDisableScrollArrows, eoDragDropEditing, eoDropFiles, eoEnhanceHomeKey, eoEnhanceEndKey, eoGroupUndo, eoHideShowScrollbars, eoKeepCaretX, eoSmartTabDelete, eoTabIndent, eoTabsToSpaces, eoShowLigatures]
    SelectedColor.Alpha = 0.400000005960464500
    WantTabs = True
    OnChange = txtExprChange
    ExplicitHeight = 311
  end
  object pJDConvertScriptingToolbar: TPanel
    Left = 0
    Top = 0
    Width = 923
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    object btnNew: TJDFontButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Action = actNew
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
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
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 0
      Text = 'New Script'
      ExplicitLeft = 46
    end
    object btnOpen: TJDFontButton
      AlignWithMargins = True
      Left = 46
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Action = actOpen
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
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
      Text = 'Open Script...'
      ExplicitLeft = 89
    end
    object btnSave: TJDFontButton
      AlignWithMargins = True
      Left = 89
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Action = actSave
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61639
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
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 2
      Text = 'Save Script'
      ExplicitLeft = 132
    end
    object btnSaveAs: TJDFontButton
      AlignWithMargins = True
      Left = 132
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Action = actSaveAs
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61639
      Image.Font.Charset = DEFAULT_CHARSET
      Image.Font.Color = clWindowText
      Image.Font.Height = -21
      Image.Font.Name = 'FontAwesome'
      Image.Font.Style = []
      Image.Font.Quality = fqAntialiased
      Image.StandardColor = fcYellow
      Overlay.Text = #61564
      Overlay.Font.Charset = DEFAULT_CHARSET
      Overlay.Font.Color = clWindowText
      Overlay.Font.Height = -13
      Overlay.Font.Name = 'FontAwesome'
      Overlay.Font.Style = []
      Overlay.Font.Quality = fqAntialiased
      Overlay.StandardColor = fcOrange
      Overlay.Position = foBottomRight
      Overlay.Margin = 3
      ImagePosition = fpImgOnly
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 3
      Text = 'Save Script As...'
      ExplicitLeft = 175
    end
    object btnExec: TJDFontButton
      AlignWithMargins = True
      Left = 175
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Action = actExecute
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61515
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
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 4
      Text = 'Execute Script'
      ExplicitLeft = 299
    end
  end
  object Stat: TStatusBar
    Left = 0
    Top = 339
    Width = 923
    Height = 19
    Panels = <
      item
        Width = 120
      end
      item
        Width = 120
      end
      item
        Width = 50
      end>
    ExplicitLeft = 96
    ExplicitTop = 240
    ExplicitWidth = 0
  end
  object SynPasSyn1: TSynPasSyn
    CommentAttri.Foreground = clLime
    CommentAttri.Style = [fsBold]
    KeyAttri.Foreground = 14021631
    NumberAttri.Foreground = clMoneyGreen
    StringAttri.Foreground = clSkyBlue
    Left = 733
    Top = 86
  end
  object dlgOpen: TOpenTextFileDialog
    Left = 184
    Top = 160
  end
  object dlgSave: TSaveTextFileDialog
    Left = 248
    Top = 160
  end
  object Acts: TActionList
    Left = 736
    Top = 144
    object actNew: TAction
      Caption = 'New Script'
      Hint = 'New Script'
      ShortCut = 16462
      OnExecute = btnNewClick
    end
    object actOpen: TAction
      Caption = 'Open Script...'
      Hint = 'Open Script...'
      ShortCut = 16463
      OnExecute = btnOpenClick
    end
    object actSave: TAction
      Caption = 'Save Script'
      Hint = 'Save Script'
      ShortCut = 16467
      OnExecute = btnSaveClick
    end
    object actSaveAs: TAction
      Caption = 'Save Script As...'
      Hint = 'Save Script As...'
      ShortCut = 24659
      OnExecute = btnSaveAsClick
    end
    object actExecute: TAction
      Caption = 'Execute Script'
      Hint = 'Execute Script'
      ShortCut = 120
      OnExecute = btnExecScriptClick
    end
  end
end
