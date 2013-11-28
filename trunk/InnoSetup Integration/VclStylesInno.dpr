library VclStylesInno;

{$IFNDEF DEBUG}
  {$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

uses
  System.SysUtils,
  Vcl.Dialogs,
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
  Vcl.Styles.InnoSetup in '..\Common\Vcl.Styles.InnoSetup.pas',
  Vcl.Styles.ExtCtrls in '..\Common\Vcl.Styles.ExtCtrls.pas',
  Vcl.Styles.StdCtrls in '..\Common\Vcl.Styles.StdCtrls.pas',
  Vcl.Styles.ComCtrls in '..\Common\Vcl.Styles.ComCtrls.pas',
  Vcl.Styles.SysListView32Wnd in '..\Common\Vcl.Styles.SysListView32Wnd.pas',
  Vcl.Styles.Form in '..\Common\Vcl.Styles.Form.pas',
  Vcl.Styles.ThemedDialog in '..\Common\Vcl.Styles.ThemedDialog.pas',
  Vcl.Styles.ToolbarWindow32Wnd in '..\Common\Vcl.Styles.ToolbarWindow32Wnd.pas',
  Vcl.Styles.ToolTipsWnd in '..\Common\Vcl.Styles.ToolTipsWnd.pas',
  Vcl.Styles.UnknownControlWnd in '..\Common\Vcl.Styles.UnknownControlWnd.pas',
  Vcl.Styles.SysControls in '..\Common\Vcl.Styles.SysControls.pas';

{$R *.res}

 procedure LoadVCLStyleW(VClStyleFile: PChar); stdcall;
 begin
   if not StyleServices.Available then exit;

   if TStyleManager.IsValidStyle(VClStyleFile) then
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(VClStyleFile))
   else
   ShowMessage(Format('The Style File %s is not valid',[VClStyleFile]));
 end;

  procedure LoadVCLStyleA(VCLStyleFile: PAnsiChar); stdcall;
 begin
   if not StyleServices.Available then exit;

   if TStyleManager.IsValidStyle(String(VCLStyleFile)) then
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(String(VCLStyleFile)))
   else
   ShowMessage(Format('The Style File %s is not valid',[VCLStyleFile]));
 end;

 procedure UnLoadVCLStyles; stdcall;
 begin
   if not StyleServices.Available then exit;
   Vcl.Styles.InnoSetup.Done;
 end;

exports
  LoadVCLStyleW, LoadVCLStyleA, UnLoadVCLStyles;

begin
end.
