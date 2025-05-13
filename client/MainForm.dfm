object formMain: TformMain
  Left = 0
  Top = 0
  Caption = #38899#20048#25773#25918#22120
  ClientHeight = 600
  ClientWidth = 900
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object pnlPlaylist: TPanel
    Left = 0
    Top = 0
    Width = 250
    Height = 560
    Align = alLeft
    TabOrder = 0
    ExplicitHeight = 552
    object lblPlaylist: TLabel
      Left = 16
      Top = 16
      Width = 52
      Height = 15
      Caption = #25773#25918#21015#34920
    end
    object lstPlaylist: TListBox
      Left = 16
      Top = 37
      Width = 218
      Height = 480
      ItemHeight = 15
      TabOrder = 0
      OnDblClick = lstPlaylistDblClick
    end
    object btnAddSong: TButton
      Left = 16
      Top = 536
      Width = 100
      Height = 25
      Caption = #28155#21152#27468#26354
      TabOrder = 1
      OnClick = btnAddSongClick
    end
    object btnRemoveSong: TButton
      Left = 134
      Top = 536
      Width = 100
      Height = 25
      Caption = #21024#38500#27468#26354
      TabOrder = 2
      OnClick = btnRemoveSongClick
    end
  end
  object pnlAlbumArt: TPanel
    Left = 250
    Top = 0
    Width = 650
    Height = 560
    Align = alClient
    TabOrder = 1
    ExplicitWidth = 648
    ExplicitHeight = 552
    object imgAlbumArt: TImage
      Left = 66
      Top = 82
      Width = 350
      Height = 350
      Center = True
      Proportional = True
      Stretch = True
    end
    object lblUserInfoTitle: TLabel
      Left = 20
      Top = 16
      Width = 65
      Height = 15
      Caption = #24403#21069#29992#25143#65306
    end
    object lblUserInfo: TLabel
      Left = 20
      Top = 37
      Width = 3
      Height = 15
    end
    object btnUploadMusic: TButton
      Left = 552
      Top = 98
      Width = 75
      Height = 25
      Caption = #19978#20256#38899#20048
      TabOrder = 0
      OnClick = btnUploadMusicClick
    end
  end
  object pnlControls: TPanel
    Left = 0
    Top = 560
    Width = 900
    Height = 40
    Align = alBottom
    TabOrder = 2
    ExplicitTop = 552
    ExplicitWidth = 898
    object lblCurrentTime: TLabel
      Left = 300
      Top = 12
      Width = 35
      Height = 14
      Caption = '00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
    object lblTotalTime: TLabel
      Left = 660
      Top = 12
      Width = 35
      Height = 14
      Caption = '00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
    end
    object btnPlay: TButton
      Left = 20
      Top = 8
      Width = 75
      Height = 25
      Caption = #25773#25918
      TabOrder = 0
      OnClick = btnPlayClick
    end
    object btnPause: TButton
      Left = 110
      Top = 8
      Width = 75
      Height = 25
      Caption = #26242#20572
      TabOrder = 1
      OnClick = btnPauseClick
    end
    object btnStop: TButton
      Left = 200
      Top = 8
      Width = 75
      Height = 25
      Caption = #20572#27490
      TabOrder = 2
      OnClick = btnStopClick
    end
    object tbProgress: TTrackBar
      Left = 354
      Top = 8
      Width = 300
      Height = 25
      Max = 100
      TabOrder = 3
      OnChange = tbProgressChange
    end
  end
  object MediaPlayer1: TMediaPlayer
    Left = 647
    Top = 532
    Width = 253
    Height = 30
    DoubleBuffered = True
    Visible = False
    ParentDoubleBuffered = False
    TabOrder = 3
  end
  object OpenDialog1: TOpenDialog
    Filter = #38899#39057#25991#20214'|*.mp3;*.wav;*.wma|'#25152#26377#25991#20214'|*.*'
    Left = 392
    Top = 472
  end
  object Timer1: TTimer
    Interval = 500
    OnTimer = Timer1Timer
    Left = 532
    Top = 472
  end
end
