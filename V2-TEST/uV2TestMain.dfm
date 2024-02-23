object frmMain: TfrmMain
  Left = 0
  Top = 0
  Caption = 'JD-UOM/UOM_V2 Test'
  ClientHeight = 348
  ClientWidth = 698
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 179
    Height = 19
    Caption = 'Units of Measurement'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object Label2: TLabel
    Left = 247
    Top = 8
    Width = 42
    Height = 19
    Caption = 'Units'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object lstUOMs: TListBox
    Left = 8
    Top = 32
    Width = 233
    Height = 305
    ItemHeight = 19
    TabOrder = 0
    OnClick = lstUOMsClick
  end
  object lstUnits: TListBox
    Left = 247
    Top = 32
    Width = 233
    Height = 305
    ItemHeight = 19
    TabOrder = 1
  end
  object UOMs: TUOM
    UOM.UOMIndex = 0
    UOM.UnitFromUndex = 0
    UOM.UnitToIndex = 0
    Left = 56
    Top = 152
  end
end
