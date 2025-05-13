program Project1;
uses
  Vcl.Forms,
  System.UITypes,
  MainForm in 'MainForm.pas' {FormMain},
  LoginForm in 'LoginForm.pas' {FormLogin};
{$R *.res}
begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  if TFormLogin.Create(nil).ShowModal = mrOk then
    Application.Run
  else
    Application.Terminate;
end.

