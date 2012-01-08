program VCLStylePreview;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  uVCLStyleUtils in '..\Common\uVCLStyleUtils.pas',
  Vcl.Themes,
  Vcl.Styles;

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Aqua Graphite');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
