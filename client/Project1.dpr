program Project1;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {TformMain},
  LoginForm in 'LoginForm.pas' {FormLogin},
  CurrentUser in 'CurrentUser.pas',
  UploadDialog in 'UploadDialog.pas' {FormUploadDialog};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormLogin, FormLogin);
  Application.CreateForm(TFormUploadDialog, FormUploadDialog);
  Application.Run;
end.
