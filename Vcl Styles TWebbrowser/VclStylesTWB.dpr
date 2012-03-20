program VclStylesTWB;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.WebBrowser in '..\Common\Vcl.Styles.WebBrowser.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
