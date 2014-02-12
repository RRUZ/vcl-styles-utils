program Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas',
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.Utils.SystemMenu in '..\Common\Vcl.Styles.Utils.SystemMenu.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amakrits');
  Application.CreateForm(TForm25, Form25);
  Application.Run;
end.
