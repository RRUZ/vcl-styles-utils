unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.Utils.SystemMenu;

type
  TForm25 = class(TForm)
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    VclStyleOptions : TVclStylesSystemMenu;
  public
    { Public declarations }
  end;

var
  Form25: TForm25;

implementation

{$R *.dfm}

procedure TForm25.FormCreate(Sender: TObject);
begin
  VclStyleOptions:=TVclStylesSystemMenu.Create(Self);
  ReportMemoryLeaksOnShutdown:=True;
end;

end.
