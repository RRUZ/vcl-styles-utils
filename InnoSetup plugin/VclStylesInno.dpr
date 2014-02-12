//**************************************************************************************************
//
// VclStylesInno  VCL Styles Plugin for Inno Setup
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
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
  WinApi.Windows,
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
   MessageBox(0, 'Error', PChar(Format('The Style File %s is not valid',[VCLStyleFile])), MB_OK);
 end;

 procedure LoadVCLStyleA(VCLStyleFile: PAnsiChar); stdcall;
 begin
   if not StyleServices.Available then exit;

   if TStyleManager.IsValidStyle(String(VCLStyleFile)) then
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(String(VCLStyleFile)))
   else
   MessageBox(0, 'Error', PChar(Format('The Style File %s is not valid',[VCLStyleFile])), MB_OK);
 end;

 procedure UnLoadVCLStyles; stdcall;
 begin
   if not StyleServices.Available then exit;
    Vcl.Styles.InnoSetup.Done;
 end;

exports
  LoadVCLStyleW, LoadVCLStyleA, UnLoadVCLStyles;

end.
