object FormLogin: TFormLogin
  Left = 0
  Top = 0
  Caption = #30331#24405
  ClientHeight = 229
  ClientWidth = 621
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poDesigned
  TextHeight = 15
  object Label1: TLabel
    Left = 162
    Top = 59
    Width = 39
    Height = 15
    Caption = #29992#25143#21517
  end
  object Label2: TLabel
    Left = 162
    Top = 121
    Width = 26
    Height = 15
    Caption = #23494#30721
  end
  object EditUsername: TEdit
    Left = 218
    Top = 56
    Width = 203
    Height = 23
    TabOrder = 0
  end
  object EditPassword: TEdit
    Left = 218
    Top = 118
    Width = 203
    Height = 23
    TabOrder = 1
  end
  object BtnRegister: TButton
    Left = 162
    Top = 174
    Width = 81
    Height = 25
    Caption = #27880#20876
    TabOrder = 2
    OnClick = BtnRegisterClick
  end
  object BtnLogin: TButton
    Left = 346
    Top = 174
    Width = 75
    Height = 25
    Caption = #30331#24405
    TabOrder = 3
    OnClick = BtnLoginClick
  end
end
