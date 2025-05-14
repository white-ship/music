object FormHistory: TFormHistory
  Left = 0
  Top = 0
  Caption = #25773#25918#21382#21490
  ClientHeight = 400
  ClientWidth = 600
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  TextHeight = 15
  object ListViewHistory: TListView
    Left = 8
    Top = 8
    Width = 584
    Height = 340
    Columns = <>
    TabOrder = 0
    ViewStyle = vsReport
    OnDblClick = ListViewHistoryDblClick
  end
  object BtnRefresh: TButton
    Left = 8
    Top = 354
    Width = 80
    Height = 30
    Caption = #21047#26032
    TabOrder = 1
    OnClick = BtnRefreshClick
  end
end
