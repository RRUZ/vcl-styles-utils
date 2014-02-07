program Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.Hooks in '..\Common\Vcl.Styles.Hooks.pas',
  KOLDetours in '..\Common\KOLDetours.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amakrits');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
