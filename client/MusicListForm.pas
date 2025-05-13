unit MusicListForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.ComCtrls, Vcl.StdCtrls,
  System.Net.HttpClient, System.Net.URLClient, System.JSON, Vcl.Dialogs;

type
  TFormMusicList = class(TForm)
    ListView1: TListView;
    procedure ListView1DblClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  public
    HTTP: THttpClient;
    procedure DownloadByID(const ID: Integer; const Title: string);
    procedure LoadMusicList;
  end;

var
  FormMusicList: TFormMusicList;

implementation

{$R *.dfm}

procedure TFormMusicList.LoadMusicList;
var
  Resp: IHTTPResponse;
  RespText: string;
  JsonArr: TJSONArray;
  JsonVal: TJSONValue;
  Obj: TJSONObject;
  Item: TListItem;
  I: Integer;
begin
  Resp := HTTP.Get('http://localhost:4567/music/list');
  RespText := Resp.ContentAsString(TEncoding.UTF8);
  if Resp.StatusCode <> 200 then
    Exit;
  JsonArr := TJSONObject.ParseJSONValue(RespText) as TJSONArray;
  try
    ListView1.Items.Clear;
    for I := 0 to JsonArr.Count - 1 do
    begin
      JsonVal := JsonArr.Items[I];
      if JsonVal is TJSONObject then
      begin
        Obj := JsonVal as TJSONObject;
        Item := ListView1.Items.Add;
        Item.Caption := Obj.GetValue('id').Value;
        Item.SubItems.Add(Obj.GetValue('title').Value);
        Item.SubItems.Add(Obj.GetValue('album').Value);
        Item.SubItems.Add(Obj.GetValue('duration').Value);
        Item.SubItems.Add(Obj.GetValue('uploaded by').Value);
      end;
    end;
  finally
    JsonArr.Free;
  end;
    with ListView1 do
  begin
    Columns[0].Width := -1;
    Columns[1].Width := -2;
    Columns[2].Width := -2;
    Columns[3].Width := -1;
    Columns[4].Width := -2;
  end;
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
    Resp := HTTP.Get(URL);
    if Resp.StatusCode = 200 then
    begin
      FileStream := TFileStream.Create(SaveDialog.FileName, fmCreate);
      try
        Resp.ContentStream.Position := 0;
        FileStream.CopyFrom(Resp.ContentStream, Resp.ContentStream.Size);
        ShowMessage('下载完成：' + SaveDialog.FileName);
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

end.

