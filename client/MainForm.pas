﻿unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, Vcl.StdCtrls,
  Vcl.ComCtrls, Vcl.MPlayer, Vcl.Imaging.jpeg,CurrentUser, System.JSON,
  System.Net.HttpClient,System.Net.URLClient,IdHTTP, IdMultipartFormData,
  UploadDialog,MusicListForm,UserManageForm,HistoryForm;

type
    TString = class(TObject)
    public
      Data: string;
      constructor Create(const S: string);
    end;

    TformMain = class(Tform)
    pnlPlaylist: TPanel;
    lblPlaylist: TLabel;
    lstPlaylist: TListBox;
    btnAddSong: TButton;
    btnRemoveSong: TButton;
    tbProgress: TTrackBar;
    lblCurrentTime: TLabel;
    lblTotalTime: TLabel;
    Timer1: TTimer;

    // 中央专辑封面面板
    pnlAlbumArt: TPanel;
    imgAlbumArt: TImage;

    // 底部控制面板
    pnlControls: TPanel;
    btnPlay: TButton;
    btnPause: TButton;
    btnStop: TButton;
    MediaPlayer1: TMediaPlayer;
    OpenDialog1: TOpenDialog;
    lblUserInfoTitle: TLabel;
    lblUserInfo: TLabel;
    btnUploadMusic: TButton;
    btnShowMusicList: TButton;
    BtnUserManage: TButton;
    BtnHistory: TButton;
    ButtonFavorites: TButton;

    procedure FormCreate(Sender: TObject);
    procedure btnAddSongClick(Sender: TObject);
    procedure btnRemoveSongClick(Sender: TObject);
    procedure lstPlaylistDblClick(Sender: TObject);
    procedure btnPlayClick(Sender: TObject);
    procedure btnPauseClick(Sender: TObject);
    procedure btnStopClick(Sender: TObject);
    procedure MediaPlayer1Notify(Sender: TObject);
    function GetRealFilePath(Index: Integer): string;
    procedure Timer1Timer(Sender: TObject);
    procedure tbProgressChange(Sender: TObject);
    procedure btnUploadMusicClick(Sender: TObject);
    procedure btnShowMusicListClick(Sender: TObject);
    procedure imgAlbumArtClick(Sender: TObject);
    procedure BtnUserManageClick(Sender: TObject);
    procedure BtnHistoryClick(Sender: TObject);
    procedure ButtonFavoritesClick(Sender: TObject);

  private
    { Private declarations }
    FIsTrackLoaded: Boolean;
    FCurrentPosition: Integer;
    CurrentTrackIndex: Integer;

    procedure LoadTrack(Index: Integer);
    function GetSongIDFromPath(const FilePath: string): Integer;
    procedure UpdateProgress;
    procedure FormatTimeLabel(Seconds: Integer; var Minutes, Sec: Integer);
  public
    { Public declarations }
  end;

var
  FormMain: TformMain;

implementation

{$R *.dfm}

constructor TString.Create(const S: string);
begin
  inherited Create;
  Data := S;
end;

procedure TformMain.btnHistoryClick(Sender: TObject);
begin
  with TFormHistory.Create(nil) do
  try
    ShowModal;
  finally
    Free;
  end;
end;

function TformMain.GetSongIDFromPath(const FilePath: string): Integer;
var
  FileName: string;
begin
  FileName := ExtractFileName(FilePath);
  Result := StrToIntDef(Copy(FileName, 1, Pos('-', FileName)-1), -1);
end;

procedure TformMain.FormCreate(Sender: TObject);
var
  Http: THttpClient;
  Resp: IHTTPResponse;
  JsonObj: TJSONObject;
  isArtist: Boolean;
begin
  // 初始化进度组件
  tbProgress.Min := 0;
  tbProgress.Max := 100;
  tbProgress.Position := 0;
  lblCurrentTime.Caption := '00:00';
  lblTotalTime.Caption := '00:00';

  // 显示用户信息
  lblUserInfo.Caption := Format('用户名: %s, ID: %d', [AppUser.Username, AppUser.UserID]);

  // 初始化定时器
  Timer1.Interval := 500; // 每500毫秒更新一次
  Timer1.Enabled := False;

  // 初始化界面
  lstPlaylist.Items.Add('示例音乐1 - 艺术家1');imgAlbumArt.Picture.Assign(nil);
  CurrentTrackIndex := -1;

    // 检查当前用户是否为音乐家
  Http := THttpClient.Create;
  try
    Resp := Http.Get(Format('http://localhost:4567/user/isArtist?userId=%d', [AppUser.UserID]));
    if Resp.StatusCode = 200 then
    begin
      JsonObj := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8)) as TJSONObject;
      try
        isArtist := JsonObj.GetValue<Boolean>('isArtist');
      finally
        JsonObj.Free;
      end;
    end
    else
      isArtist := False;
  finally
    Http.Free;
  end;

  btnUploadMusic.Visible := isArtist or AppUser.IsAdmin;
  BtnUserManage.Visible := AppUser.IsAdmin;
  ButtonFavorites.OnClick := ButtonFavoritesClick;
end;

//用户管理
procedure TFormMain.BtnUserManageClick(Sender: TObject);
begin
  if AppUser.IsAdmin then
    FormUserManage.ShowModal
  else
    ShowMessage('只有管理员可以访问用户管理功能');
end;

//添加
procedure TformMain.btnAddSongClick(Sender: TObject);
var
  i: Integer;
  FileName: string;
begin
  OpenDialog1.Options := OpenDialog1.Options + [ofAllowMultiSelect];

  if OpenDialog1.Execute then
  begin
    lstPlaylist.Items.BeginUpdate;
    try
      // 遍历所有选中文件
      for i := 0 to OpenDialog1.Files.Count - 1 do
      begin
        // 提取纯文件名（不带扩展名）
        FileName := ChangeFileExt(ExtractFileName(OpenDialog1.Files[i]), '');

        // 将完整路径存储到Object属性，显示美化后的文件名
        lstPlaylist.Items.AddObject(FileName, TString.Create(OpenDialog1.Files[i]));
      end;

      // 自动排序（可选）
      lstPlaylist.Sorted := True;
    finally
      lstPlaylist.Items.EndUpdate;
    end;
  end;
end;

function TformMain.GetRealFilePath(Index: Integer): string;
begin
  if (Index >= 0) and (Index < lstPlaylist.Items.Count) then
    Result := TString(lstPlaylist.Items.Objects[Index]).Data
  else
    Result := '';
end;

procedure TformMain.imgAlbumArtClick(Sender: TObject);
begin

end;

//删除
procedure TformMain.btnRemoveSongClick(Sender: TObject);
begin
  if lstPlaylist.ItemIndex <> -1 then
  begin
    lstPlaylist.Items.Delete(lstPlaylist.ItemIndex);
    MediaPlayer1.Stop;
  end;
end;

//双击播放列表
procedure TformMain.lstPlaylistDblClick(Sender: TObject);
const
  DEFAULT_ALBUM = 'Images\default_album.jpg';
  ERROR_ALBUM = 'Images\error_album.jpg';
begin
  // 所有情况先显示默认封面
  try
    imgAlbumArt.Picture.LoadFromFile(DEFAULT_ALBUM);
  except
    imgAlbumArt.Picture.Assign(nil);
  end;

  if lstPlaylist.ItemIndex = -1 then Exit;

  // 示例音乐不执行播放操作
  if Pos('示例音乐', lstPlaylist.Items[lstPlaylist.ItemIndex]) > 0 then
    Exit;

  // 真实曲目播放处理
  try
    if (CurrentTrackIndex <> lstPlaylist.ItemIndex) or not FIsTrackLoaded then
    begin
      LoadTrack(lstPlaylist.ItemIndex);
      CurrentTrackIndex := lstPlaylist.ItemIndex;
      FIsTrackLoaded := True;
    end;

    if MediaPlayer1.Mode = mpPaused then
      MediaPlayer1.Position := FCurrentPosition;

    MediaPlayer1.Play;
    Timer1.Enabled := True;

  except
    on E: Exception do
    begin
      FIsTrackLoaded := False;
      try
        imgAlbumArt.Picture.LoadFromFile(ERROR_ALBUM);
      except
        imgAlbumArt.Picture.Assign(nil);
      end;
      ShowMessage('播放失败');
    end;
  end;
end;

procedure TformMain.LoadTrack(Index: Integer);
var
  RealPath: string;
begin
  RealPath := GetRealFilePath(Index);
  if RealPath = '' then Exit;

  try
    MediaPlayer1.FileName := RealPath;
    MediaPlayer1.Open;

    // 初始化进度条
    tbProgress.Position := 0;
    tbProgress.Max := MediaPlayer1.Length;
    Timer1.Enabled := True;

  except
    on E: Exception do
    begin
      Timer1.Enabled := False;
      raise;
    end;
  end;
end;


//暂停，播放，停止
procedure TformMain.btnPlayClick(Sender: TObject);
begin
  if lstPlaylist.ItemIndex = -1 then Exit;

  // 如果未加载或切换了曲目
  if not FIsTrackLoaded or (CurrentTrackIndex <> lstPlaylist.ItemIndex) then
  begin
    LoadTrack(lstPlaylist.ItemIndex);
    CurrentTrackIndex := lstPlaylist.ItemIndex;
    FIsTrackLoaded := True;
  end
  else if MediaPlayer1.Mode = mpPaused then
  begin
    // 从暂停位置继续播放
    MediaPlayer1.Position := FCurrentPosition;
  end;

  MediaPlayer1.Play;
  Timer1.Enabled := True;
end;

procedure TformMain.btnPauseClick(Sender: TObject);
begin
  if MediaPlayer1.Mode = mpPlaying then
  begin
    FCurrentPosition := MediaPlayer1.Position; // 记录暂停位置
    MediaPlayer1.Pause;
    Timer1.Enabled := False;
  end;
end;

procedure TformMain.btnShowMusicListClick(Sender: TObject);
begin
  FormMusicList := TFormMusicList.Create(nil);
  try
    // 手动初始化列表
    FormMusicList.HTTP := THttpClient.Create;
    FormMusicList.ListView1.ViewStyle := vsReport;
    FormMusicList.ListView1.Columns.Clear;
    FormMusicList.ListView1.Columns.Add.Caption := 'ID';
    FormMusicList.ListView1.Columns.Add.Caption := '标题';
    FormMusicList.ListView1.Columns.Add.Caption := '专辑';
    FormMusicList.ListView1.Columns.Add.Caption := '时长';
    FormMusicList.ListView1.Columns.Add.Caption := '音乐家id';
    FormMusicList.ListView1.Columns.Add.Caption := '下载次数';
    FormMusicList.IsFavoritesMode := False;
    FormMusicList.LoadMusicList;
    FormMusicList.Show;
  except
  FormMusicList.HTTP.Free;
    FormMusicList.Free;
    raise;
  end;
end;

procedure TformMain.btnStopClick(Sender: TObject);
begin
  MediaPlayer1.Stop;
  Timer1.Enabled := False;
  tbProgress.Position := 0;
  lblCurrentTime.Caption := '00:00';
end;

procedure TFormMain.btnUploadMusicClick(Sender: TObject);
var
  HTTP: TIdHTTP;
  FormData: TIdMultiPartFormDataStream;
begin
  FormUploadDialog := TFormUploadDialog.Create(Self);
  try
    if FormUploadDialog.ShowModal = mrOk then
    begin
      if not FileExists(FormUploadDialog.EditFile.Text) then
      begin
        ShowMessage('请选择有效的音乐文件');
        Exit;
      end;

      // 创建上传请求
      HTTP := TIdHTTP.Create(nil);
      FormData := TIdMultiPartFormDataStream.Create;
      try
        FormData.AddFormField('title', FormUploadDialog.EditName.Text);
        FormData.AddFormField('album', FormUploadDialog.EditAlbum.Text);
        FormData.AddFormField('duration', FormUploadDialog.EditDuration.Text);
        FormData.AddFormField('uploader_id', IntToStr(AppUser.UserID));
        FormData.AddFormField('uploader_name', AppUser.Username);
        FormData.AddFile('file', UTF8Encode(FormUploadDialog.EditFile.Text), 'audio/mpeg');

        HTTP.Request.ContentType := FormData.RequestContentType;
        HTTP.Request.CharSet := 'utf-8';

        HTTP.Post('http://localhost:4567/upload', FormData);
        ShowMessage('上传成功');
      except
        on E: Exception do
          ShowMessage('上传失败');
      end;
      HTTP.Free;
      FormData.Free;
    end;
  finally
    FormUploadDialog.Free;
  end;
end;

//自动连续播放
procedure TformMain.MediaPlayer1Notify(Sender: TObject);
begin
  try
    case MediaPlayer1.NotifyValue of
      nvSuccessful:
        begin
          if CurrentTrackIndex < lstPlaylist.Items.Count - 1 then
          begin
              Inc(CurrentTrackIndex);
              LoadTrack(CurrentTrackIndex);
              MediaPlayer1.Play;
          end;
        end;
    end;
    MediaPlayer1.Notify := True;
  except
    on E: Exception do
      ShowMessage('播放控制错误');
  end;
end;

procedure TformMain.Timer1Timer(Sender: TObject);
begin
  if MediaPlayer1.Mode = mpPlaying then
    UpdateProgress;
end;

procedure TformMain.UpdateProgress;
var
  CurrentMin, CurrentSec: Integer;
  TotalMin, TotalSec: Integer;
begin
  try
    // 设置进度条范围
    if tbProgress.Max <> MediaPlayer1.Length then
    begin
      tbProgress.Min := 0;
      tbProgress.Max := MediaPlayer1.Length;
    end;

    // 更新当前位置
    tbProgress.Position := MediaPlayer1.Position;

    // 格式化时间显示
    FormatTimeLabel(MediaPlayer1.Position div 1000, CurrentMin, CurrentSec);
    FormatTimeLabel(MediaPlayer1.Length div 1000, TotalMin, TotalSec);

    lblCurrentTime.Caption := Format('%.2d:%.2d', [CurrentMin, CurrentSec]);
    lblTotalTime.Caption := Format('%.2d:%.2d', [TotalMin, TotalSec]);

  except
    on E: Exception do
      OutputDebugString(PChar('进度更新错误'));
  end;
end;

procedure TformMain.FormatTimeLabel(Seconds: Integer; var Minutes, Sec: Integer);
begin
  Minutes := Seconds div 60;
  Sec := Seconds mod 60;
end;

procedure TformMain.tbProgressChange(Sender: TObject);
begin
  if MediaPlayer1.Mode in [mpPlaying, mpPaused] then
  begin
    MediaPlayer1.Pause;
    MediaPlayer1.Position := tbProgress.Position;
    if FIsTrackLoaded then
      MediaPlayer1.Play;
  end;
end;

procedure TformMain.ButtonFavoritesClick(Sender: TObject);
var
  Resp: IHTTPResponse;
  RespText: string;
  JsonVal: TJSONValue;
  JsonArr: TJSONArray;
  Obj: TJSONObject;
  Item: TListItem;
  I: Integer;
  URL: string;
begin
  // 创建音乐列表窗口
  FormMusicList := TFormMusicList.Create(nil);
  try
    // 初始化 HTTP 和列表样式
    FormMusicList.HTTP := THttpClient.Create;
    FormMusicList.ListView1.ViewStyle := vsReport;
    FormMusicList.ListView1.Columns.Clear;
    FormMusicList.ListView1.Columns.Add.Caption := 'ID';
    FormMusicList.ListView1.Columns.Add.Caption := '标题';
    FormMusicList.ListView1.Columns.Add.Caption := '专辑';
    FormMusicList.ListView1.Columns.Add.Caption := '时长';
    FormMusicList.ListView1.Columns.Add.Caption := '上传者';
    FormMusicList.ListView1.Columns.Add.Caption := '下载次数';
    FormMusicList.IsFavoritesMode := True;

    // 请求收藏数据
    URL := Format('http://localhost:4567/favorites/list?userId=%d', [AppUser.UserID]);
    Resp := FormMusicList.HTTP.Get(URL);
    RespText := Resp.ContentAsString(TEncoding.UTF8);

    if Resp.StatusCode <> 200 then
    begin
      ShowMessage('加载收藏失败：' + Resp.StatusCode.ToString);
      Exit;
    end;

    // 解析 JSON 并填充列表
    JsonVal := TJSONObject.ParseJSONValue(RespText);
    JsonArr := JsonVal as TJSONArray;

    FormMusicList.ListView1.Items.BeginUpdate;
    try
      FormMusicList.ListView1.Items.Clear;
      for I := 0 to JsonArr.Count - 1 do
      begin
        if not (JsonArr.Items[I] is TJSONObject) then Continue;

        Obj := JsonArr.Items[I] as TJSONObject;
        Item := FormMusicList.ListView1.Items.Add;
        Item.Caption := Obj.GetValue('id', '0');
        Item.SubItems.Add(Obj.GetValue('title', '未知标题'));
        Item.SubItems.Add(Obj.GetValue('album', '未知专辑'));
        Item.SubItems.Add(Obj.GetValue('duration', '未知时长'));
        Item.SubItems.Add(Obj.GetValue('uploaded_by', '未知上传者'));
        Item.SubItems.Add(Obj.GetValue('downloads', '0'));
      end;
    finally
      FormMusicList.ListView1.Items.EndUpdate;
    end;

    var
      TotalWidth: Integer;
    begin
      TotalWidth := FormMusicList.ListView1.ClientWidth;
      with FormMusicList.ListView1 do
      begin
        Columns[0].Width := Round(TotalWidth * 0.10);
        Columns[1].Width := Round(TotalWidth * 0.30);
        Columns[2].Width := Round(TotalWidth * 0.20);
        Columns[3].Width := Round(TotalWidth * 0.10);
        Columns[4].Width := Round(TotalWidth * 0.30);
      end;
    end;

    JsonArr.Free;

    FormMusicList.Show;
  except
    on E: Exception do
    begin
      if Assigned(FormMusicList.HTTP) then
        FormMusicList.HTTP.Free;
      FormMusicList.Free;
      ShowMessage('发生错误：' + E.Message);
    end;
  end;
end;


end.
