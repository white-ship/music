unit MusicListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  System.Net.HttpClient,System.NetEncoding, System.Net.URLClient, System.JSON, Vcl.Dialogs,
  Vcl.Menus,CurrentUser;

type
  TFormMusicList = class(TForm)
    ListView1: TListView;
    PopupMenu1: TPopupMenu;
    MenuItemDelete: TMenuItem;
    EditSearch: TEdit;
    ButtonSearch: TButton;
    MunuItemFav: TMenuItem;
    MenuItemRemoveFavorite: TMenuItem;
    procedure ListView1DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
    procedure ButtonSearchClick(Sender: TObject);
    procedure MenuItemAddFavoriteClick(Sender: TObject);
    procedure MenuItemRemoveFavoriteClick(Sender: TObject);
  public
    HTTP: THttpClient;
    IsFavoritesMode: Boolean;
    procedure DownloadByID(const ID: Integer; const Title: string);
    procedure LoadMusicList(const Keyword: string = '');
  end;

var
  FormMusicList: TFormMusicList;

implementation

{$R *.dfm}

procedure TFormMusicList.FormShow(Sender: TObject);
begin
  MenuItemDelete.Visible := not IsFavoritesMode;
  MunuItemFav.Visible := not IsFavoritesMode;
  MenuItemRemoveFavorite.Visible := IsFavoritesMode;
end;


procedure TFormMusicList.FormCreate(Sender: TObject);
begin
  HTTP := THttpClient.Create;
  MenuItemRemoveFavorite.Visible := False;
end;

procedure TFormMusicList.ListView1ContextPopup(Sender: TObject; MousePos: TPoint; var Handled: Boolean);
begin
  if not Assigned(ListView1.GetItemAt(MousePos.X, MousePos.Y)) then
    Exit;

  MenuItemDelete.Visible := AppUser.IsAdmin and (not IsFavoritesMode);
  MunuItemFav.Visible := not IsFavoritesMode;
  MenuItemRemoveFavorite.Visible := IsFavoritesMode;

  PopupMenu1.Popup(Mouse.CursorPos.X, Mouse.CursorPos.Y);
end;

procedure TFormMusicList.MenuItemDeleteClick(Sender: TObject);
var
  ID: Integer;
  Resp: IHTTPResponse;
  URL, RespText: string;
  JSON: TJSONObject;
begin
  if not Assigned(ListView1.Selected) then Exit;

  ID := StrToInt(ListView1.Selected.Caption);
  if MessageDlg('确定要删除该音乐吗？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    URL := Format('http://localhost:4567/music/delete?id=%d', [ID]);

    // 加上 userId 头部，如果需要验证权限
    HTTP.CustomHeaders['userId'] := IntToStr(AppUser.UserID); // 假设 AppUser.UserID 是你保存的用户 ID

    Resp := HTTP.Delete(URL);
    RespText := Resp.ContentAsString(TEncoding.UTF8); // ⭐ 确保使用 UTF-8 编码
    try
      JSON := TJSONObject.ParseJSONValue(RespText) as TJSONObject;
      try
        if (Resp.StatusCode = 200) and (JSON.GetValue('status').Value = 'success') then
        begin
          ShowMessage('删除成功：' + JSON.GetValue('message').Value);
          LoadMusicList;
        end
        else
        begin
          ShowMessage('删除失败：' + JSON.GetValue('message').Value);
        end;
      finally
        JSON.Free;
      end;
    except
      on E: Exception do
        ShowMessage('响应解析失败：' + E.Message + sLineBreak + '原始响应：' + RespText);
    end;
  end;
end;

procedure TFormMusicList.LoadMusicList(const Keyword: string = '');
var
  Resp: IHTTPResponse;
  RespText: string;
  JsonVal: TJSONValue;
  JsonArr: TJSONArray;
  Obj: TJSONObject;
  Item: TListItem;
  I: Integer;
  URL: string;
  TotalWidth: Integer;
begin
  try
     try
      if IsFavoritesMode then
      begin
        // 收藏模式：从收藏接口获取
        URL := Format('http://localhost:4567/favorites/list?userId=%d', [AppUser.UserID]);
      end
      else
      begin
        if Keyword.Trim = '' then
          URL := 'http://localhost:4567/music/list'
        else
          URL := 'http://localhost:4567/music/search?keyword=' + TNetEncoding.URL.Encode(Keyword);
      end;
       finally

     end;
    Resp := HTTP.Get(URL);
    RespText := Resp.ContentAsString(TEncoding.UTF8);

    if Resp.StatusCode <> 200 then
    begin
      ShowMessage('加载失败：' + Resp.StatusCode.ToString);
      Exit;
    end;

    JsonVal := TJSONObject.ParseJSONValue(RespText);
    JsonArr := JsonVal as TJSONArray;

    ListView1.Items.BeginUpdate;
    try
      ListView1.Items.Clear;
      for I := 0 to JsonArr.Count - 1 do
      begin
        if not (JsonArr.Items[I] is TJSONObject) then
          Continue;

        Obj := JsonArr.Items[I] as TJSONObject;
        Item := ListView1.Items.Add;
        Item.Caption := Obj.GetValue('id', '0');
        Item.SubItems.Add(Obj.GetValue('title', '未知标题'));
        Item.SubItems.Add(Obj.GetValue('album', '未知专辑'));
        Item.SubItems.Add(Obj.GetValue('duration', '未知时长'));
        Item.SubItems.Add(Obj.GetValue('uploaded by', '未知上传者'));
        Item.SubItems.Add(Obj.GetValue('downloads', '0'));
      end;
    finally
      ListView1.Items.EndUpdate;
    end;

    TotalWidth := ListView1.ClientWidth;
    with ListView1 do
    begin
      Columns[0].Width := Round(TotalWidth * 0.10); // ID
      Columns[1].Width := Round(TotalWidth * 0.30); // 标题
      Columns[2].Width := Round(TotalWidth * 0.20); // 专辑
      Columns[3].Width := Round(TotalWidth * 0.10); // 时长
      Columns[4].Width := Round(TotalWidth * 0.20); // 上传者
      Columns[5].Width := Round(TotalWidth * 0.10); // 下载次数
    end;

    JsonArr.Free;
  except
    on E: Exception do
      ShowMessage('发生错误：' + E.Message);
  end;
end;

procedure TFormMusicList.ButtonSearchClick(Sender: TObject);
begin
  LoadMusicList(EditSearch.Text);
end;

procedure TFormMusicList.ListView1DblClick(Sender: TObject);
var
  Sel: TListItem;
  ID: Integer;
  Title: string;
begin
  Sel := ListView1.Selected;
  if not Assigned(Sel) then Exit;
  ID := StrToInt(Sel.Caption);
  Title := Sel.SubItems[0];
  DownloadByID(ID, Title);
end;

procedure TFormMusicList.DownloadByID(const ID: Integer; const Title: string);
var
  Resp: IHTTPResponse;
  SaveDialog: TSaveDialog;
  FileStream: TFileStream;
  URL: string;
begin
  SaveDialog := TSaveDialog.Create(Self);
  try
    SaveDialog.FileName := Title;
    SaveDialog.Filter := '所有文件|*.*';
    if not SaveDialog.Execute then Exit;

    URL := Format('http://localhost:4567/music/download?id=%d', [ID]);
    HTTP.CustomHeaders['userId'] := IntToStr(AppUser.UserID);
    Resp := HTTP.Get(URL);
    if Resp.StatusCode = 200 then
    begin
      FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
      try
        Resp.ContentStream.Position := 0;
        FileStream.CopyFrom(Resp.ContentStream, Resp.ContentStream.Size);
        ShowMessage('下载完成：' + SaveDialog.FileName);
        LoadMusicList;
      finally
        FileStream.Free;
      end;
    end
    else
      ShowMessage('下载失败，HTTP ' + Resp.StatusCode.ToString);
  finally
    SaveDialog.Free;
  end;
end;

procedure TFormMusicList.FormDestroy(Sender: TObject);
begin
  HTTP.Free;
end;

procedure TFormMusicList.MenuItemAddFavoriteClick(Sender: TObject);
var
  ID: Integer;
  URL: string;
  Resp: IHTTPResponse;
  JSON: TJSONObject;
  RespText: string;
begin
  if not Assigned(ListView1.Selected) then Exit;

  ID := StrToInt(ListView1.Selected.Caption);
  URL := 'http://localhost:4567/favorites/add';

  HTTP.CustomHeaders['Content-Type'] := 'application/json';
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('userId', TJSONNumber.Create(AppUser.UserID));
    JSON.AddPair('songId', TJSONNumber.Create(ID));

    Resp := HTTP.Post(URL, TStringStream.Create(JSON.ToString, TEncoding.UTF8));
    RespText := Resp.ContentAsString(TEncoding.UTF8);

    if Resp.StatusCode = 200 then
      ShowMessage('收藏成功')
    else
      ShowMessage('收藏失败：' + RespText);
  finally
    JSON.Free;
  end;
end;

procedure TFormMusicList.MenuItemRemoveFavoriteClick(Sender: TObject);
var
  ID: Integer;
  URL, RespText: string;
  JSON: TJSONObject;
  Resp: IHTTPResponse;
begin
  if not Assigned(ListView1.Selected) then Exit;

  ID := StrToInt(ListView1.Selected.Caption);
  URL := 'http://localhost:4567/favorites/remove';

  HTTP.CustomHeaders['Content-Type'] := 'application/json';
  JSON := TJSONObject.Create;
  try
    JSON.AddPair('userId', TJSONNumber.Create(AppUser.UserID));
    JSON.AddPair('songId', TJSONNumber.Create(ID));

    Resp := HTTP.Post(URL, TStringStream.Create(JSON.ToString, TEncoding.UTF8));
    RespText := Resp.ContentAsString(TEncoding.UTF8);

    if Resp.StatusCode = 200 then
    begin
      ShowMessage('已从收藏中移除');
      // 重新加载收藏列表
      if IsFavoritesMode then
        LoadMusicList;
    end
    else
      ShowMessage('移除失败：' + RespText);
  finally
    JSON.Free;
  end;
end;


end.

