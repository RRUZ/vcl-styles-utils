program ThemedSysControls;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {Main},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.ButtonWnd in '..\Common\Vcl.Styles.ButtonWnd.pas',
  Vcl.Styles.ComboBoxWnd in '..\Common\Vcl.Styles.ComboBoxWnd.pas',
  Vcl.Styles.ControlWnd in '..\Common\Vcl.Styles.ControlWnd.pas',
  Vcl.Styles.EditWnd in '..\Common\Vcl.Styles.EditWnd.pas',
  Vcl.Styles.NativeScrollBar in '..\Common\Vcl.Styles.NativeScrollBar.pas',
  Vcl.Styles.PopupWnd in '..\Common\Vcl.Styles.PopupWnd.pas',
  Vcl.Styles.ScrollBarWnd in '..\Common\Vcl.Styles.ScrollBarWnd.pas',
  Vcl.Styles.StaticWnd in '..\Common\Vcl.Styles.StaticWnd.pas',
  Vcl.Styles.SysControls in '..\Common\Vcl.Styles.SysControls.pas',
  Vcl.Styles.SysListView32Wnd in '..\Common\Vcl.Styles.SysListView32Wnd.pas',
  Vcl.Styles.ThemedDialog in '..\Common\Vcl.Styles.ThemedDialog.pas',
  Vcl.Styles.ToolbarWindow32Wnd in '..\Common\Vcl.Styles.ToolbarWindow32Wnd.pas',
  Vcl.Styles.ToolTipsWnd in '..\Common\Vcl.Styles.ToolTipsWnd.pas',
  Vcl.Styles.UnknownControlWnd in '..\Common\Vcl.Styles.UnknownControlWnd.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amethyst Kamri');
  Application.CreateForm(TMain, Main);
  Application.Run;
end.
