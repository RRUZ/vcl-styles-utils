program ColorTabs;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.ColorTabs in '..\Common\Vcl.Styles.ColorTabs.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Metro Grey');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
