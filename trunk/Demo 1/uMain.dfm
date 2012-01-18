object FrmMain: TFrmMain
  Left = 0
  Top = 0
  Caption = 'Simple Demo'
  ClientHeight = 214
  ClientWidth = 311
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
  object Button1: TButton
    Left = 8
    Top = 184
    Width = 75
    Height = 25
    Caption = 'Close'
    TabOrder = 0
    OnClick = Button1Click
  end
  object RadioButton1: TRadioButton
    Left = 8
    Top = 8
    Width = 113
    Height = 17
    Caption = 'RadioButton1'
    TabOrder = 1
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 31
    Width = 97
    Height = 17
    Caption = 'CheckBox1'
    TabOrder = 2
  end
  object PageControl1: TPageControl
    Left = 8
    Top = 62
    Width = 289
    Height = 105
    ActivePage = TabSheet1
    TabOrder = 3
    object TabSheet1: TTabSheet
      Caption = 'TabSheet1'
      object Edit1: TEdit
        Left = 3
        Top = 16
        Width = 121
        Height = 21
        TabOrder = 0
        Text = 'Edit1'
      end
    end
    object TabSheet2: TTabSheet
      Caption = 'TabSheet2'
      ImageIndex = 1
    end
  end
  object Button2: TButton
    Left = 218
    Top = 181
    Width = 75
    Height = 25
    Caption = 'Button2'
    TabOrder = 4
  end
end
