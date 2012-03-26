program VCLStylesSample;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.FormStyleHooks in '..\Common\Vcl.Styles.FormStyleHooks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Carbon');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
