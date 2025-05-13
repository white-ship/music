object FormUploadDialog: TFormUploadDialog
  Left = 0
  Top = 0
  Caption = #19978#20256#38899#20048
  ClientHeight = 325
  ClientWidth = 452
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  TextHeight = 15
  object lblFile: TLabel
    Left = 40
    Top = 16
    Width = 52
    Height = 15
    Caption = #38899#20048#25991#20214
  end
  object lblName: TLabel
    Left = 40
    Top = 64
    Width = 52
    Height = 15
    Caption = #27468#26354#21517#31216
  end
  object lblAlbum: TLabel
    Left = 40
    Top = 128
    Width = 52
    Height = 15
    Caption = #19987#36753#21517#31216
  end
  object lblDuration: TLabel
    Left = 40
    Top = 192
    Width = 65
    Height = 15
    Caption = #26102#38271#65288#31186#65289
  end
  object EditFile: TEdit
    Left = 200
    Top = 13
    Width = 121
    Height = 23
    ReadOnly = True
    TabOrder = 0
  end
  object btnBrowse: TButton
    Left = 344
    Top = 12
    Width = 75
    Height = 25
    Caption = #27983#35272'...'
    TabOrder = 1
    OnClick = btnBrowseClick
  end
  object EditName: TEdit
    Left = 200
    Top = 61
    Width = 121
    Height = 23
    TabOrder = 2
  end
  object EditAlbum: TEdit
    Left = 200
    Top = 125
    Width = 121
    Height = 23
    TabOrder = 3
  end
  object EditDuration: TEdit
    Left = 200
    Top = 189
    Width = 121
    Height = 23
    TabOrder = 4
  end
  object btnOK: TButton
    Left = 320
    Top = 272
    Width = 75
    Height = 25
    Caption = #19978#20256
    ModalResult = 1
    TabOrder = 5
  end
  object btnCancel: TButton
    Left = 104
    Top = 272
    Width = 75
    Height = 25
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 6
  end
end
