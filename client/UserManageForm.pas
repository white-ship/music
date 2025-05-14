unit UserManageForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls,
  System.Net.HttpClient, System.JSON, Vcl.Dialogs, CurrentUser;

type
  TFormUserManage = class(TForm)
    ListViewUsers: TListView;
    BtnRefresh: TButton;
    BtnDelete: TButton;
    BtnSetAdmin: TButton;
    BtnRemoveAdmin: TButton;
    BtnSetArtist: TButton;
    BtnRemoveArtist: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure BtnDeleteClick(Sender: TObject);
    procedure BtnSetAdminClick(Sender: TObject);
    procedure BtnRemoveAdminClick(Sender: TObject);
    procedure BtnSetArtistClick(Sender: TObject);
    procedure BtnRemoveArtistClick(Sender: TObject);
  private
    HTTP: THttpClient;
    procedure LoadUsers;
    procedure UpdateRole(UserID: Integer; Add: Boolean);
  end;

var
  FormUserManage: TFormUserManage;

implementation

{$R *.dfm}

procedure TFormUserManage.FormCreate(Sender: TObject);
begin
  if not AppUser.IsAdmin then
  begin
    ShowMessage('仅管理员可访问此界面');
    Close;
    Exit;
  end;

  HTTP := THttpClient.Create;
  LoadUsers;
end;

procedure TFormUserManage.LoadUsers;
var
  Resp: IHTTPResponse;
  JsonArr: TJSONArray;
  Obj: TJSONObject;
  Item: TListItem;
  I: Integer;
begin
  ListViewUsers.Items.Clear;

  Resp := HTTP.Get('http://localhost:4567/users/list');
  if Resp.StatusCode <> 200 then
  begin
    ShowMessage('加载失败');
    Exit;
  end;

  JsonArr := TJSONObject.ParseJSONValue(Resp.ContentAsString(TEncoding.UTF8)) as TJSONArray;
  try
    for I := 0 to JsonArr.Count - 1 do
    begin
      Obj := JsonArr.Items[I] as TJSONObject;
      Item := ListViewUsers.Items.Add;
      Item.Caption := Obj.GetValue('id').Value;
      Item.SubItems.Add(Obj.GetValue('username').Value);
      if Obj.GetValue('is_admin').AsType<Boolean> then
        Item.SubItems.Add('是')
      else
        Item.SubItems.Add('否');
      if Obj.GetValue('is_artist').AsType<Boolean> then
        Item.SubItems.Add('是')
      else
        Item.SubItems.Add('否');
    end;
  finally
    JsonArr.Free;
  end;
end;

procedure TFormUserManage.BtnRefreshClick(Sender: TObject);
begin
  LoadUsers;
end;

procedure TFormUserManage.BtnDeleteClick(Sender: TObject);
var
  ID: string;
  Resp: IHTTPResponse;
begin
  if not Assigned(ListViewUsers.Selected) then Exit;

  ID := ListViewUsers.Selected.Caption;
  if MessageDlg('确认删除该用户？', mtConfirmation, [mbYes, mbNo], 0) = mrYes then
  begin
    Resp := HTTP.Delete('http://localhost:4567/users/delete?id=' + ID);
    if Resp.StatusCode = 200 then
      LoadUsers
    else
      ShowMessage('删除失败');
  end;
end;

procedure TFormUserManage.UpdateRole(UserID: Integer; Add: Boolean);
var
  URL: string;
  Resp: IHTTPResponse;
begin
  if Add then
    URL := Format('http://localhost:4567/users/set_admin?id=%d', [UserID])
  else
    URL := Format('http://localhost:4567/users/remove_admin?id=%d', [UserID]);

  Resp := HTTP.Post(URL, TStringStream.Create('', TEncoding.UTF8), nil);
  if Resp.StatusCode = 200 then
    LoadUsers
  else
    ShowMessage('操作失败');
end;

procedure TFormUserManage.BtnSetAdminClick(Sender: TObject);
begin
  if Assigned(ListViewUsers.Selected) then
    UpdateRole(StrToInt(ListViewUsers.Selected.Caption), True);
end;

procedure TFormUserManage.BtnRemoveAdminClick(Sender: TObject);
begin
  if Assigned(ListViewUsers.Selected) then
    UpdateRole(StrToInt(ListViewUsers.Selected.Caption), False);
end;

procedure TFormUserManage.BtnSetArtistClick(Sender: TObject);
var
  URL: string;
  Resp: IHTTPResponse;
  UserID: Integer;
begin
  if Assigned(ListViewUsers.Selected) then
  begin
      UserID := StrToInt(ListViewUsers.Selected.Caption);
      URL := Format('http://localhost:4567/users/set_artist?userId=%d', [UserID]);

      Resp := HTTP.Post(URL, TStringStream.Create('', TEncoding.UTF8), nil);
      if Resp.StatusCode = 200 then
        LoadUsers
      else
        ShowMessage('操作失败');
  end;
end;

procedure TFormUserManage.BtnRemoveArtistClick(Sender: TObject);
var
  URL: string;
  Resp: IHTTPResponse;
  UserID: Integer;
begin
  if Assigned(ListViewUsers.Selected) then
  begin
      UserID := StrToInt(ListViewUsers.Selected.Caption);
      URL := Format('http://localhost:4567/users/remove_artist?userId=%d', [UserID]);

      Resp := HTTP.Post(URL, TStringStream.Create('', TEncoding.UTF8), nil);
      if Resp.StatusCode = 200 then
        LoadUsers
      else
        ShowMessage('操作失败');
  end;
end;

end.
