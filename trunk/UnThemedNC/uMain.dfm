object Form1: TForm1
  Left = 679
  Top = 314
  BorderStyle = bsSingle
  Caption = 'Form1'
  ClientHeight = 173
  ClientWidth = 292
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object SpeedButton1: TSpeedButton
    Left = 64
    Top = 104
    Width = 169
    Height = 22
    Caption = 'Show OpenDialog'
    OnClick = SpeedButton1Click
  end
  object OpenDialog1: TOpenDialog
    Left = 120
    Top = 32
  end
end
