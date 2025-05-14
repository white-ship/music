object FormMusicList: TFormMusicList
  Left = 0
  Top = 0
  Caption = 'FormMusicList'
  ClientHeight = 440
  ClientWidth = 624
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnShow = FormShow
  TextHeight = 15
  object ListView1: TListView
    Left = 32
    Top = 45
    Width = 553
    Height = 385
    Columns = <>
    PopupMenu = PopupMenu1
    TabOrder = 0
    OnDblClick = ListView1DblClick
  end
  object EditSearch: TEdit
    Left = 32
    Top = 16
    Width = 441
    Height = 23
    TabOrder = 1
  end
  object ButtonSearch: TButton
    Left = 510
    Top = 14
    Width = 75
    Height = 25
    Caption = #25628#32034
    TabOrder = 2
    OnClick = ButtonSearchClick
  end
  object PopupMenu1: TPopupMenu
    Left = 152
    Top = 184
    object MenuItemRemoveFavorite: TMenuItem
      Caption = #21462#28040#25910#34255
      OnClick = MenuItemRemoveFavoriteClick
    end
    object MunuItemFav: TMenuItem
      Caption = #25910#34255#38899#20048
      OnClick = MenuItemAddFavoriteClick
    end
    object MenuItemDelete: TMenuItem
      Caption = #21024#38500#35813#38899#20048
      OnClick = MenuItemDeleteClick
    end
  end
end
