program DemoActionMenu;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Form4},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.PlatformVclStylesActnCtrls in '..\Common\Vcl.PlatformVclStylesActnCtrls.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Auric');
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
