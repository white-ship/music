object FormUserManage: TFormUserManage
  Left = 0
  Top = 0
  Caption = #29992#25143#31649#29702
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
  object ListViewUsers: TListView
    Left = 10
    Top = 10
    Width = 580
    Height = 300
    Columns = <
      item
        Caption = 'ID'
      end
      item
        Caption = #29992#25143#21517
        Width = 200
      end
      item
        Caption = #31649#29702#21592
        Width = 100
      end>
    TabOrder = 0
    ViewStyle = vsReport
  end
  object BtnRefresh: TButton
    Left = 10
    Top = 320
    Width = 80
    Height = 30
    Caption = #21047#26032
    TabOrder = 1
    OnClick = BtnRefreshClick
  end
  object BtnDelete: TButton
    Left = 100
    Top = 320
    Width = 80
    Height = 30
    Caption = #21024#38500#29992#25143
    TabOrder = 2
    OnClick = BtnDeleteClick
  end
  object BtnSetAdmin: TButton
    Left = 190
    Top = 320
    Width = 100
    Height = 30
    Caption = #35774#20026#31649#29702#21592
    TabOrder = 3
    OnClick = BtnSetAdminClick
  end
  object BtnRemoveAdmin: TButton
    Left = 300
    Top = 320
    Width = 100
    Height = 30
    Caption = #21462#28040#31649#29702#21592
    TabOrder = 4
    OnClick = BtnRemoveAdminClick
  end
end
