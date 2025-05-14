unit HistoryForm;

interface

uses
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  System.Classes, System.SysUtils,
  System.Net.HttpClient, System.Net.URLClient, System.JSON, Vcl.Dialogs, CurrentUser;

type
  TFormHistory = class(TForm)
    ListViewHistory: TListView;
    BtnRefresh: TButton;
    procedure FormCreate(Sender: TObject);
    procedure BtnRefreshClick(Sender: TObject);
    procedure ListViewHistoryDblClick(Sender: TObject);
  private
    HTTP: THttpClient;
    procedure LoadHistory;
  end;

var
  FormHistory: TFormHistory;

implementation

{$R *.dfm}

procedure TFormHistory.FormCreate(Sender: TObject);
begin
  HTTP := THttpClient.Create;
  // 设置列
  with ListViewHistory.Columns do
  begin
    Clear;
    Add.Caption := 'ID';         // history id
    Add.Caption := 'songs';
    Add.Caption := 'album';
    Add.Caption := 'duration';
    Add.Caption := 'download at';
  end;
  ListViewHistory.ViewStyle := vsReport;
  LoadHistory;
end;

procedure TFormHistory.BtnRefreshClick(Sender: TObject);
begin
  LoadHistory;
end;

procedure TFormHistory.LoadHistory;
var
  Resp: IHTTPResponse;
  RespText: string;
  Arr: TJSONArray;
  Obj: TJSONObject;
  Item: TListItem;
  I: Integer;
  URL: string;
begin
  ListViewHistory.Items.Clear;
  URL := Format('http://localhost:4567/history?userId=%d', [AppUser.UserID]);
  Resp := HTTP.Get(URL);
  if Resp.StatusCode <> 200 then
  begin
    ShowMessage('加载失败，HTTP ' + Resp.StatusCode.ToString);
    Exit;
  end;
  RespText := Resp.ContentAsString(TEncoding.UTF8);
  Arr := TJSONObject.ParseJSONValue(RespText) as TJSONArray;
  if not Assigned(Arr) then
  begin
    ShowMessage('解析失败');
    Exit;
  end;
  try
    for I := 0 to Arr.Count - 1 do
    begin
      if not (Arr.Items[I] is TJSONObject) then Continue;
      Obj := Arr.Items[I] as TJSONObject;
      Item := ListViewHistory.Items.Add;
      Item.Caption := Obj.GetValue('historyId', '0');
      Item.SubItems.Add(Obj.GetValue('title', ''));
      Item.SubItems.Add(Obj.GetValue('album', ''));
      Item.SubItems.Add(Obj.GetValue('duration', ''));
      Item.SubItems.Add(Obj.GetValue('playedAt', ''));
    end;
  finally
    Arr.Free;
  end;
  // 自动列宽
  with ListViewHistory do
  begin
    Columns[0].Width := 50;
    Columns[1].Width := 150;
    Columns[2].Width := 100;
    Columns[3].Width := 80;
    Columns[4].Width := 150;
  end;
end;

procedure TFormHistory.ListViewHistoryDblClick(Sender: TObject);
begin
  // 双击可以实现再次播放或跳转到歌曲详情，留空或根据需求实现
  ShowMessage('双击播放历史项尚未实现');
end;

end.

