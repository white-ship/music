unit LoginForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, System.JSON,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls
  , System.Net.HttpClient,System.Net.URLClient,MainForm,CurrentUser;

type
  TFormLogin = class(TForm)
    EditUsername: TEdit;
    EditPassword: TEdit;
    BtnRegister: TButton;
    BtnLogin: TButton;
    Label1: TLabel;
    Label2: TLabel;
    procedure BtnRegisterClick(Sender: TObject);
    procedure BtnLoginClick(Sender: TObject);
  private
    HTTP: THttpClient;
    procedure SendRequest(const Path: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  FormLogin: TFormLogin;

implementation

{$R *.dfm}

constructor TFormLogin.Create(AOwner: TComponent);
begin
  inherited;
  HTTP := THttpClient.Create;
end;

destructor TFormLogin.Destroy;
begin
  HTTP.Free;
  inherited;
end;

procedure TFormLogin.SendRequest(const Path: string);
var
  JsonResponse: TJSONObject;
  Status: string;
  UserId: Integer;
  URL, JsonBody, ResponseText: string;
  Response: IHTTPResponse;
  Stream: TStringStream;
  Headers: TNetHeaders;
begin
  URL := 'http://localhost:4567' + Path;
  JsonBody := Format('{"username":"%s","password":"%s"}', [
    EditUsername.Text, EditPassword.Text]);
  Stream := TStringStream.Create(JsonBody, TEncoding.UTF8);
  try
    SetLength(Headers, 1);
    Headers[0] := TNameValuePair.Create('Content-Type', 'application/json');
    Response := HTTP.Post(URL, Stream, nil, Headers);
    ResponseText := Response.ContentAsString();

    JsonResponse := TJSONObject.ParseJSONValue(ResponseText) as TJSONObject;

    if Path = '/login' then
    begin
      Status := JsonResponse.GetValue<string>('status');
      if Status = 'success' then
      begin
        AppUser.UserID := JsonResponse.GetValue<Integer>('userID');
        AppUser.Username := JsonResponse.GetValue<string>('username');
        AppUser.IsAdmin := JsonResponse.GetValue<Boolean>('isAdmin');
        ModalResult := mrOk;
      end
      else
      begin
        ShowMessage('µÇÂ¼Ê§°Ü£¬Çë¼ì²éÓÃ»§Ãû»òÃÜÂë');
      end;
    end
    else if Path = '/register' then
    begin
      if Pos('Registered', ResponseText) > 0 then
        ShowMessage('×¢²á³É¹¦£¡')
      else
        ShowMessage('×¢²áÊ§°Ü£º' + ResponseText);
    end;
  except
    on E: Exception do
    begin
      ShowMessage('ÇëÇóÊ§°Ü£º' + E.Message);
    end;
  end;
  Stream.Free;
end;



procedure TFormLogin.BtnRegisterClick(Sender: TObject);
begin
  SendRequest('/register');
end;

procedure TFormLogin.BtnLoginClick(Sender: TObject);
begin
  SendRequest('/login');
end;
end.
