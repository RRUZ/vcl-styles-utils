program TTaskDialogsDemo;

uses
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmTaskDlgMain},
  Vcl.Styles.Utils.Forms in '..\Common\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  DDetours in '..\Common\delphi-detours-library\DDetours.pas',
  InstDecode in '..\Common\delphi-detours-library\InstDecode.pas',
  Vcl.Styles.Hooks in '..\Common\Vcl.Styles.Hooks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Auric');
  Application.CreateForm(TFrmTaskDlgMain, FrmTaskDlgMain);
  Application.Run;
end.
