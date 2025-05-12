program Project1;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {TformMain},
  LoginForm in 'LoginForm.pas' {FormLogin};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormLogin, FormLogin);
  Application.Run;
end.
