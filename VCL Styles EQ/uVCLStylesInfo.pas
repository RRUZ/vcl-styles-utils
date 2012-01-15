unit uVCLStylesInfo;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Themes;

type
  TFrmVCLStyleInfoDialog = class(TForm)
    EditName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    EditAuthor: TEdit;
    Label3: TLabel;
    EditEMail: TEdit;
    Label4: TLabel;
    EditURL: TEdit;
    Label5: TLabel;
    EditVersion: TEdit;
    Button1: TButton;
    Button2: TButton;
    procedure Button2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FStatus   : Boolean;
    FStyleInfo: TStyleInfo;
    procedure SetStyleInfo(const Value: TStyleInfo);
    { Private declarations }
  public
    property  StyleInfo:  TStyleInfo read FStyleInfo write SetStyleInfo;
    function Execute : Boolean;
  end;

implementation

{$R *.dfm}

procedure TFrmVCLStyleInfoDialog.Button1Click(Sender: TObject);
begin
  FStyleInfo.Name       :=EditName.Text;
  FStyleInfo.Author     :=EditAuthor.Text;
  FStyleInfo.AuthorEMail:=EditEMail.Text;
  FStyleInfo.AuthorURL  :=EditURL.Text;
  FStyleInfo.Version    :=EditVersion.Text;
  FStatus:=True;
  Close;
end;

procedure TFrmVCLStyleInfoDialog.Button2Click(Sender: TObject);
begin
 FStatus:=False;
 Close;
end;

function TFrmVCLStyleInfoDialog.Execute: Boolean;
begin
  ShowModal;
  Result:=FStatus;
end;

procedure TFrmVCLStyleInfoDialog.FormCreate(Sender: TObject);
begin
  FStatus:=False;
end;

procedure TFrmVCLStyleInfoDialog.SetStyleInfo(const Value: TStyleInfo);
begin
  FStyleInfo := Value;
  EditName.Text:=FStyleInfo.Name;
  EditAuthor.Text:=FStyleInfo.Author;
  EditEMail.Text:=FStyleInfo.AuthorEMail;
  EditURL.Text:=FStyleInfo.AuthorURL;
  EditVersion.Text:=FStyleInfo.Version;
end;

end.
