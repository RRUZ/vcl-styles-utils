program VCLStylePreview;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  uVCLStyleUtils in '..\Common\uVCLStyleUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
