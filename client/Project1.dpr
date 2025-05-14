program Project1;
uses
  Vcl.Forms,
  System.UITypes,
  MainForm in 'MainForm.pas' {FormMain},
  LoginForm in 'LoginForm.pas' {FormLogin},
  CurrentUser in 'CurrentUser.pas',
  MusicListForm in 'MusicListForm.pas' {FormMusicList},
  UploadDialog in 'UploadDialog.pas' {FormUploadDialog},
  UserManageForm in 'UserManageForm.pas' {FormUserManage};

{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;

  with TFormLogin.Create(nil) do
  try
    if ShowModal = mrOk then
    begin
      Application.CreateForm(TFormMain, FormMain);
      Application.CreateForm(TFormMusicList, FormMusicList);
      Application.CreateForm(TFormUploadDialog, FormUploadDialog);
      if AppUser.IsAdmin then
      begin
        Application.CreateForm(TFormUserManage, FormUserManage);
      end;
      Application.Run;
    end
    else
      Application.Terminate;
  finally
    Free;
  end;
end.

