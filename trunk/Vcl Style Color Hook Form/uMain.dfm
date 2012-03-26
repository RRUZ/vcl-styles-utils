object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Vcl Styles Demo'
  ClientHeight = 373
  ClientWidth = 362
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 45
    Height = 13
    Caption = 'Vcl Styles'
  end
  object Image1: TImage
    Left = 184
    Top = 64
    Width = 105
    Height = 105
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 84
    Width = 337
    Height = 121
    Caption = 'Non client area'
    TabOrder = 0
    object EditNCImage: TEdit
      Left = 16
      Top = 91
      Width = 226
      Height = 21
      Enabled = False
      TabOrder = 0
    end
    object RadioButtonNCImage: TRadioButton
      Left = 16
      Top = 68
      Width = 75
      Height = 17
      Caption = 'Use Image'
      TabOrder = 1
      OnClick = RadioButtonNCColorClick
    end
    object RadioButtonNCColor: TRadioButton
      Left = 16
      Top = 40
      Width = 81
      Height = 17
      Caption = 'Use Color'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = RadioButtonNCColorClick
    end
    object ColorBoxNC: TColorBox
      Left = 97
      Top = 40
      Width = 145
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      TabOrder = 3
      OnChange = ColorBoxNCChange
      OnGetColors = ColorBoxNCGetColors
    end
    object BtnSetNCImage: TButton
      Left = 248
      Top = 89
      Width = 75
      Height = 25
      Caption = 'Set  Image'
      Enabled = False
      TabOrder = 4
      OnClick = BtnSetNCImageClick
    end
    object CheckBoxNC: TCheckBox
      Left = 16
      Top = 17
      Width = 65
      Height = 17
      Caption = 'Enabled'
      TabOrder = 5
      OnClick = CheckBoxNCClick
    end
  end
  object GroupBox2: TGroupBox
    Left = 8
    Top = 211
    Width = 337
    Height = 154
    Caption = 'Background'
    TabOrder = 1
    object EditBackImage: TEdit
      Left = 16
      Top = 106
      Width = 226
      Height = 21
      Enabled = False
      TabOrder = 0
    end
    object RadioButtonBackImage: TRadioButton
      Left = 16
      Top = 83
      Width = 113
      Height = 17
      Caption = 'Use Image'
      TabOrder = 1
      OnClick = RadioButtonBackColorClick
    end
    object RadioButtonBackColor: TRadioButton
      Left = 16
      Top = 56
      Width = 81
      Height = 17
      Caption = 'Use Color'
      Checked = True
      TabOrder = 2
      TabStop = True
      OnClick = RadioButtonBackColorClick
    end
    object ColorBoxBackground: TColorBox
      Left = 97
      Top = 55
      Width = 145
      Height = 22
      Selected = clRed
      Style = [cbStandardColors, cbExtendedColors, cbCustomColor, cbPrettyNames, cbCustomColors]
      TabOrder = 3
      OnChange = ColorBoxBackgroundChange
      OnGetColors = ColorBoxNCGetColors
    end
    object BtnSetBackImage: TButton
      Left = 248
      Top = 105
      Width = 75
      Height = 25
      Caption = 'Set  Image'
      Enabled = False
      TabOrder = 4
      OnClick = BtnSetBackImageClick
    end
    object CheckBoxBack: TCheckBox
      Left = 16
      Top = 33
      Width = 65
      Height = 17
      Caption = 'Enabled'
      TabOrder = 5
      OnClick = CheckBoxBackClick
    end
  end
  object ComboBoxStyles: TComboBox
    Left = 8
    Top = 27
    Width = 337
    Height = 21
    Style = csDropDownList
    TabOrder = 2
    OnChange = ComboBoxStylesChange
  end
  object CheckBoxMerge: TCheckBox
    Left = 8
    Top = 61
    Width = 97
    Height = 17
    Caption = 'Merge Images'
    Checked = True
    State = cbChecked
    TabOrder = 3
    OnClick = CheckBoxMergeClick
  end
  object OpenPictureDialog1: TOpenPictureDialog
    Left = 288
    Top = 166
  end
end
