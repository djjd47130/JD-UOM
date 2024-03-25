object frJDConvertScripting: TfrJDConvertScripting
  Left = 0
  Top = 0
  Width = 862
  Height = 545
  TabOrder = 0
  object Splitter1: TSplitter
    Left = 0
    Top = 290
    Width = 862
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
    Top = 297
    Width = 862
    Height = 248
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = -193
    ExplicitWidth = 1055
    object txtOutput: TMemo
      AlignWithMargins = True
      Left = 3
      Top = 39
      Width = 856
      Height = 206
      Align = alClient
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 0
      WordWrap = False
      ExplicitTop = 34
      ExplicitWidth = 1049
      ExplicitHeight = 211
    end
    object btnExecScript: TJDFontButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 856
      Height = 30
      Cursor = crHandPoint
      Align = alTop
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61537
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
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 1
      Text = 'Execute Script'
      OnClick = btnExecScriptClick
      ExplicitLeft = 4
    end
  end
  object txtExpr: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 44
    Width = 856
    Height = 243
    Align = alClient
    Color = 1644825
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -16
    Font.Name = 'Consolas'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    TabOrder = 1
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
      'var V: Float = UOM('#39'3ft'#39') / UOM('#39'6.9cm'#39') + Sqr(46);'
      'var BU: TUOM = BaseUOM('#39'Distance'#39');'
      ''
      'procedure OutputConversion(const UOM: String);'
      'var'
      '  T: Float;'
      '  Res: String;'
      'begin'
      '  T:= Convert(V, '#39'Meter'#39', UOM);'
      '  Res:= UOMString(T, UOM);'
      '  PrintLn(Res);'
      'end;'
      ''
      'PrintLn(UOMString(V, BU.NameSingular)+'#39' is the same as:'#39');'
      'OutputConversion('#39'ft'#39');'
      'OutputConversion('#39'km'#39');'
      'OutputConversion('#39'ly'#39');')
    SelectedColor.Alpha = 0.400000005960464500
    ExplicitLeft = -187
    ExplicitTop = 3
    ExplicitWidth = 1049
    ExplicitHeight = 347
  end
  object pJDConvertScriptingToolbar: TPanel
    Left = 0
    Top = 0
    Width = 862
    Height = 41
    Align = alTop
    BevelOuter = bvNone
    ParentShowHint = False
    ShowHint = True
    TabOrder = 2
    ExplicitTop = -3
    object JDFontButton1: TJDFontButton
      AlignWithMargins = True
      Left = 46
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Hint = 'Create New Script'
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
      Text = ''
      ExplicitLeft = 63
      ExplicitTop = 5
      ExplicitHeight = 33
    end
    object JDFontButton2: TJDFontButton
      AlignWithMargins = True
      Left = 89
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Hint = 'Open Existing Script'
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
      Text = ''
      ExplicitLeft = 47
      ExplicitTop = 5
      ExplicitHeight = 33
    end
    object JDFontButton3: TJDFontButton
      AlignWithMargins = True
      Left = 132
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Hint = 'Save Script'
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
      Text = ''
      ExplicitLeft = 90
      ExplicitTop = 5
      ExplicitHeight = 33
    end
    object JDFontButton4: TJDFontButton
      AlignWithMargins = True
      Left = 175
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Hint = 'Save Script As...'
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
      Text = ''
      ExplicitLeft = 213
      ExplicitTop = 0
      ExplicitHeight = 33
    end
    object JDFontButton5: TJDFontButton
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 37
      Height = 35
      Cursor = crHandPoint
      Hint = 'Preset Scripts'
      Align = alLeft
      DrawStyle = fdTransparent
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      Image.AutoSize = False
      Image.Text = #61732
      Image.Font.Charset = DEFAULT_CHARSET
      Image.Font.Color = clWindowText
      Image.Font.Height = -21
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
      SubTextFont.Charset = DEFAULT_CHARSET
      SubTextFont.Color = clGray
      SubTextFont.Height = -11
      SubTextFont.Name = 'Tahoma'
      SubTextFont.Style = []
      TabOrder = 4
      Text = ''
      ExplicitLeft = -12
      ExplicitTop = 5
      ExplicitHeight = 33
    end
  end
  object SynPasSyn1: TSynPasSyn
    KeyAttri.Foreground = 14021631
    NumberAttri.Foreground = clMoneyGreen
    StringAttri.Foreground = clSkyBlue
    Left = 677
    Top = 102
  end
end
