unit UploadDialog;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,Vcl.FileCtrl;

type
  TFormUploadDialog = class(TForm)
    lblFile: TLabel;
    lblName: TLabel;
    lblAlbum: TLabel;
    lblDuration: TLabel;
    EditFile: TEdit;
    btnBrowse: TButton;
    EditName: TEdit;
    EditAlbum: TEdit;
    EditDuration: TEdit;
    btnOK: TButton;
    btnCancel: TButton;

    procedure btnBrowseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormUploadDialog: TFormUploadDialog;

implementation

{$R *.dfm}

procedure TFormUploadDialog.btnBrowseClick(Sender: TObject);
begin
  with TOpenDialog.Create(Self) do
  try
    Filter := '音乐文件 (*.mp3;*.wav)|*.mp3;*.wav|所有文件 (*.*)|*.*';
    if Execute then
      EditFile.Text := FileName;
  finally
    Free;
  end;
end;


end.
