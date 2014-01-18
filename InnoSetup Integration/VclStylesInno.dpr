library VclStylesInno;

{$IFNDEF DEBUG}
  {$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{$SetPEFlags $2000}
uses
  System.SysUtils,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Dialogs,
  Vcl.Styles.Hooks in '..\Common\Vcl.Styles.Hooks.pas',
  KOLDetours in '..\Common\KOLDetours.pas',
  Vcl.Styles.InnoSetup in 'Vcl.Styles.InnoSetup.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.ComCtrls in '..\Common\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in '..\Common\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in '..\Common\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.StdCtrls in '..\Common\Vcl.Styles.Utils.StdCtrls.pas',
  Vcl.Styles.InnoSetup.StyleHooks in 'Vcl.Styles.InnoSetup.StyleHooks.pas';

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
