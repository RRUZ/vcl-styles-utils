unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs;

type
  TForm25 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

uses
 Vcl.Styles.Utils.SystemMenu;

{$R *.dfm}

procedure TForm25.FormCreate(Sender: TObject);
begin
  TVclStylesSystemMenu.Create(Self);
  ReportMemoryLeaksOnShutdown:=True;
end;

end.
