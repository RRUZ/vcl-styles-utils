object FrmHueSat: TFrmHueSat
  Left = 809
  Top = 234
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'VCL Styles Equalizer'
  ClientHeight = 572
  ClientWidth = 382
  Color = clBtnFace
  TransparentColorValue = clFuchsia
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  GlassFrame.Bottom = 80
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ImageVCLStyle: TImage
    Left = 8
    Top = 54
    Width = 357
    Height = 202
  end
  object Label4: TLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 13
    Caption = 'VCL Styles'
  end
  object BtnApply: TButton
    Left = 8
    Top = 539
    Width = 225
    Height = 25
    Caption = 'Apply changes to the current style'
    TabOrder = 0
    OnClick = BtnApplyClick
  end
  object BtnSave: TButton
    Left = 239
    Top = 539
    Width = 96
    Height = 25
    Caption = 'Save'
    TabOrder = 1
    OnClick = BtnSaveClick
  end
  object ComboBoxVclStyles: TComboBox
    Left = 8
    Top = 27
    Width = 249
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ComboBoxVclStylesChange
  end
  object Button1: TButton
    Left = 263
    Top = 23
    Width = 75
    Height = 25
    Action = ActionApplyStyle
    Caption = 'Apply Style'
    TabOrder = 3
  end
  object CheckBoxSepia: TCheckBox
    Left = 8
    Top = 516
    Width = 73
    Height = 17
    Caption = 'Sepia'
    TabOrder = 4
    OnClick = CheckBoxSepiaClick
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 291
    Width = 361
    Height = 219
    ActivePage = TabSheet1
    TabOrder = 5
    object TabSheet1: TTabSheet
      Caption = 'HSL'
      object Label1: TLabel
        Left = 11
        Top = 14
        Width = 19
        Height = 13
        Caption = 'Hue'
      end
      object Bevel4: TBevel
        Left = 11
        Top = 28
        Width = 329
        Height = 5
        Shape = bsTopLine
      end
      object Label2: TLabel
        Left = 11
        Top = 70
        Width = 50
        Height = 13
        Caption = 'Saturation'
      end
      object Bevel2: TBevel
        Left = 11
        Top = 85
        Width = 329
        Height = 5
        Shape = bsTopLine
      end
      object Label3: TLabel
        Left = 11
        Top = 126
        Width = 45
        Height = 13
        Caption = 'Lightness'
      end
      object Bevel3: TBevel
        Left = 62
        Top = 134
        Width = 278
        Height = 5
        Shape = bsTopLine
      end
      object TrackBarHue: TTrackBar
        Left = 3
        Top = 33
        Width = 249
        Height = 30
        DoubleBuffered = True
        Max = 180
        Min = -180
        ParentDoubleBuffered = False
        TabOrder = 0
        TickStyle = tsManual
        OnChange = TrackBarHueChange
      end
      object EditLight: TEdit
        Left = 258
        Top = 145
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '0'
        OnExit = EditLightExit
      end
      object UpDownLight: TUpDown
        Left = 290
        Top = 145
        Width = 16
        Height = 21
        Associate = EditLight
        Min = -255
        Max = 255
        TabOrder = 2
        OnChanging = UpDownLightChanging
      end
      object EditSat: TEdit
        Left = 258
        Top = 89
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 3
        Text = '0'
        OnExit = EditSatExit
      end
      object UpDownSat: TUpDown
        Left = 290
        Top = 89
        Width = 16
        Height = 21
        Associate = EditSat
        TabOrder = 4
        OnChanging = UpDownSatChanging
      end
      object EditHue: TEdit
        Left = 258
        Top = 35
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 5
        Text = '0'
        OnExit = EditHueExit
      end
      object UpDownHue: TUpDown
        Left = 290
        Top = 35
        Width = 16
        Height = 21
        Associate = EditHue
        Min = -180
        Max = 180
        TabOrder = 6
        OnChanging = UpDownHueChanging
      end
      object ButtonHue: TButton
        Left = 314
        Top = 33
        Width = 26
        Height = 25
        BiDiMode = bdRightToLeftNoAlign
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        ParentBiDiMode = False
        TabOrder = 7
        OnClick = ButtonHueClick
      end
      object TrackBarSaturation: TTrackBar
        Left = 3
        Top = 90
        Width = 249
        Height = 30
        DoubleBuffered = True
        Max = 100
        ParentDoubleBuffered = False
        TabOrder = 8
        TickStyle = tsManual
        OnChange = TrackBarSaturationChange
      end
      object ButtonSaturation: TButton
        Left = 312
        Top = 90
        Width = 26
        Height = 25
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        TabOrder = 9
        OnClick = ButtonSaturationClick
      end
      object TrackBarLightness: TTrackBar
        Left = 3
        Top = 145
        Width = 249
        Height = 30
        DoubleBuffered = False
        Max = 255
        Min = -255
        ParentDoubleBuffered = False
        TabOrder = 10
        TickStyle = tsManual
        OnChange = TrackBarLightnessChange
      end
      object ButtonLightness: TButton
        Left = 312
        Top = 145
        Width = 26
        Height = 25
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        TabOrder = 11
        OnClick = ButtonLightnessClick
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'RGB'
      ImageIndex = 1
      object Label5: TLabel
        Left = 11
        Top = 14
        Width = 19
        Height = 13
        Caption = 'Red'
      end
      object Bevel5: TBevel
        Left = 11
        Top = 28
        Width = 329
        Height = 5
        Shape = bsTopLine
      end
      object Label6: TLabel
        Left = 11
        Top = 62
        Width = 29
        Height = 13
        Caption = 'Green'
      end
      object Bevel6: TBevel
        Left = 11
        Top = 76
        Width = 329
        Height = 5
        Shape = bsTopLine
      end
      object Label7: TLabel
        Left = 11
        Top = 118
        Width = 20
        Height = 13
        Caption = 'Blue'
      end
      object Bevel7: TBevel
        Left = 11
        Top = 132
        Width = 329
        Height = 5
        Shape = bsTopLine
      end
      object TrackBarRed: TTrackBar
        Left = 3
        Top = 33
        Width = 249
        Height = 30
        DoubleBuffered = True
        Max = 255
        ParentDoubleBuffered = False
        TabOrder = 0
        TickStyle = tsManual
        OnChange = TrackBarRedChange
      end
      object EditRed: TEdit
        Left = 258
        Top = 35
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 1
        Text = '0'
        OnExit = EditHueExit
      end
      object UpDownRed: TUpDown
        Left = 290
        Top = 35
        Width = 16
        Height = 21
        Associate = EditRed
        Max = 255
        TabOrder = 2
        OnChanging = UpDownHueChanging
      end
      object Button2: TButton
        Left = 314
        Top = 33
        Width = 26
        Height = 25
        BiDiMode = bdRightToLeftNoAlign
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        ParentBiDiMode = False
        TabOrder = 3
        OnClick = Button2Click
      end
      object TrackBarGreen: TTrackBar
        Left = 3
        Top = 81
        Width = 249
        Height = 30
        DoubleBuffered = True
        Max = 255
        ParentDoubleBuffered = False
        TabOrder = 4
        TickStyle = tsManual
        OnChange = TrackBarRedChange
      end
      object EditGreen: TEdit
        Left = 258
        Top = 83
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 5
        Text = '0'
        OnExit = EditHueExit
      end
      object UpDownGreen: TUpDown
        Left = 290
        Top = 83
        Width = 16
        Height = 21
        Associate = EditGreen
        Max = 255
        TabOrder = 6
        OnChanging = UpDownHueChanging
      end
      object Button3: TButton
        Left = 314
        Top = 81
        Width = 26
        Height = 25
        BiDiMode = bdRightToLeftNoAlign
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        ParentBiDiMode = False
        TabOrder = 7
        OnClick = Button3Click
      end
      object TrackBarBlue: TTrackBar
        Left = 3
        Top = 137
        Width = 249
        Height = 30
        DoubleBuffered = True
        Max = 255
        ParentDoubleBuffered = False
        TabOrder = 8
        TickStyle = tsManual
        OnChange = TrackBarRedChange
      end
      object EditBlue: TEdit
        Left = 258
        Top = 139
        Width = 32
        Height = 21
        NumbersOnly = True
        TabOrder = 9
        Text = '0'
        OnExit = EditHueExit
      end
      object UpDownBlue: TUpDown
        Left = 290
        Top = 139
        Width = 16
        Height = 21
        Associate = EditBlue
        Max = 255
        TabOrder = 10
        OnChanging = UpDownHueChanging
      end
      object Button4: TButton
        Left = 312
        Top = 143
        Width = 26
        Height = 25
        BiDiMode = bdRightToLeftNoAlign
        ImageAlignment = iaCenter
        ImageIndex = 0
        Images = ImageList1
        ParentBiDiMode = False
        TabOrder = 11
        OnClick = Button4Click
      end
    end
    object TabSheet3: TTabSheet
      Caption = 'Blend'
      ImageIndex = 2
      ExplicitLeft = -52
      ExplicitTop = 28
      object Label8: TLabel
        Left = 3
        Top = 52
        Width = 26
        Height = 13
        Caption = 'Mode'
      end
      object Label9: TLabel
        Left = 3
        Top = 5
        Width = 25
        Height = 13
        Caption = 'Color'
      end
      object ButtonApplyBlend: TButton
        Left = 3
        Top = 103
        Width = 75
        Height = 25
        Caption = 'Apply Blend'
        TabOrder = 0
        OnClick = ButtonApplyBlendClick
      end
      object ColorBoxblend: TColorBox
        Left = 3
        Top = 24
        Width = 145
        Height = 22
        TabOrder = 1
      end
      object Button6: TButton
        Left = 154
        Top = 24
        Width = 27
        Height = 22
        Caption = '...'
        TabOrder = 2
        OnClick = Button6Click
      end
      object ComboBoxBlend: TComboBox
        Left = 3
        Top = 71
        Width = 145
        Height = 21
        Style = csDropDownList
        TabOrder = 3
      end
    end
  end
  object RadioButtonHSL: TRadioButton
    Left = 8
    Top = 268
    Width = 50
    Height = 17
    Caption = 'HSL'
    Checked = True
    TabOrder = 6
    TabStop = True
    OnClick = RadioButtonHSLClick
  end
  object RadioButtonRGB: TRadioButton
    Tag = 1
    Left = 74
    Top = 268
    Width = 58
    Height = 17
    Caption = 'RGB'
    TabOrder = 7
    OnClick = RadioButtonHSLClick
  end
  object RadioButtonBlend: TRadioButton
    Tag = 2
    Left = 138
    Top = 268
    Width = 55
    Height = 17
    Caption = 'Blend'
    TabOrder = 8
    OnClick = RadioButtonHSLClick
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    Left = 216
    Top = 136
    Bitmap = {
      494C010101000800380010001000FFFFFFFF2110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000104
      0106336D38B80000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000000000000458F
      4CD9499B52F70E21103800000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004A9553CF6FBD
      79FF78C082FF59A962FF45984DFF3F9047FF398740FF337E39FF2D7633FF286E
      2DFF236727FF1F6122FF1B5C1EFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004C9555C479C885FF9BD5
      A4FF97D3A0FF93D09CFF8FCE97FF8ACB92FF86C98DFF81C588FF7CC283FF78C0
      7EFF74BD7AFF70BC76FF1F6122FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000509D5AC97DCC89FFA1D8
      AAFF9DD6A6FF99D4A2FF95D29EFF92CF99FF8DCC94FF88CA8FFF84C78AFF80C4
      86FF7BC181FF76BF7CFF236727FF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000053A35DD17ECD
      8AFF7AC986FF5BB766FF56B060FF51A85AFF4BA054FF45984DFF3F9047FF3987
      40FF337E39FF2D7633FF286E2DFF000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000000000057AB
      62DB60BC6BF70000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000105
      03064B9254BB0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
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
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF000000000000E7FF000000000000E3FF000000000000
      C00100000000000080010000000000008001000000000000C001000000000000
      E7FF000000000000E7FF000000000000FFFF000000000000FFFF000000000000
      FFFF000000000000FFFF00000000000000000000000000000000000000000000
      000000000000}
  end
  object ActionManager1: TActionManager
    Left = 168
    Top = 144
    StyleName = 'Platform Default'
    object ActionApplyStyle: TAction
      Caption = 'ActionApplyStyle'
      OnExecute = ActionApplyStyleExecute
      OnUpdate = ActionApplyStyleUpdate
    end
  end
  object SaveDialog1: TSaveDialog
    DefaultExt = '*.vsf'
    Filter = 'Visual Style Files|*.vsf'
    Left = 112
    Top = 152
  end
  object ColorDialog1: TColorDialog
    Options = [cdFullOpen, cdShowHelp, cdSolidColor, cdAnyColor]
    Left = 224
    Top = 328
  end
end
