//**************************************************************************************************
//
// NSISVCLStyles VCL Styles Plugin for NSIS
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
// The Original Code is  NSISVCLStyles.dpr
//
// The Initial Developer of the Original Code is Rodrigo Ruz V. 
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
// Portions created by Mahdi Safsafi [SMP3]   e-mail SMP@LIVE.FR
//
// All Rights Reserved.
//
//**************************************************************************************************
library NSISVCLStyles;


{$IFNDEF DEBUG}
  {$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{$SetPEFlags $2000}
uses
  //IOUTILS,
  System.SysUtils,
  WinApi.Windows,
  Vcl.Themes,
  Vcl.Styles,
  //Vcl.Imaging.pngimage,
  Vcl.Styles.NSIS in 'Vcl.Styles.NSIS.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.ComCtrls in '..\Common\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in '..\Common\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in '..\Common\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.StdCtrls in '..\Common\Vcl.Styles.Utils.StdCtrls.pas',
  //KOLDetours in '..\Common\KOLDetours.pas',
  //Vcl.Styles.Hooks in '..\Common\Vcl.Styles.Hooks.pas',
  nsis in 'nsis.pas';

//NSIS Scripting Reference
  //http://nsis.sourceforge.net/Docs/Chapter4.html
{.$R *.res}

{$IFDEF NSIS_ANSI}
{$R VersionInfoANSI.res}
{$ENDIF}

{$IFDEF NSIS_UNICODE}
{$R VersionInfoUNICODE.res}
{$ENDIF}


var
 _NSISCallBack: TRegisterPluginCallback;


//procedure Addlog(const Msg: string);
//begin
//   TFile.AppendAllText('C:\Test\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
//end;

function NSISCallback(const NSPIM: Integer): Integer; cdecl;
begin
  Result := 0;
end;


 //procedure LoadVCLStyleA(VCLStyleFile: PAnsiChar); cdecl;

{$IFDEF NSIS_ANSI}
 procedure LoadVCLStyle(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 var
  VCLStyleFile : PAnsiChar;
 begin
   if not StyleServices.Available then exit;
   InitA(hwndParent, string_size, variables, stacktop, extraparameters);

   @_NSISCallBack := extrap.RegisterPluginCallback;
   _NSISCallBack(HInstance, @NSISCallback);

   VCLStyleFile:=PAnsiChar(PopStringA());
   if TStyleManager.IsValidStyle(String(VCLStyleFile)) then
   begin
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(String(VCLStyleFile)));

          {
     TSysDialogStyleHookBackground.MergeImages := True;
     TSysDialogStyleHookBackground.SharedImageLocation := 'C:\Delphi\google-code\vcl-styles-utils\NSIS plugin\background.png';
     TSysDialogStyleHookBackground.BackGroundSettings.UseImage := True;
     TSysDialogStyleHookBackground.BackGroundSettings.Enabled  := True;

     TSysStyleManager.UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
     TSysStyleManager.RegisterSysStyleHook('#32770', TSysDialogStyleHookBackground);

     TSysStyleManager.UnRegisterSysStyleHook('Static', TSysStaticStyleHook);
     TSysStyleManager.RegisterSysStyleHook('Static', TTransparentStaticNSIS);
         }
   end
   else
   MessageBox(hwndParent, 'Error', PChar(Format('The Style File %s is not valid',[VCLStyleFile])), MB_OK);
 end;

 procedure RemoveStyleNCArea(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 begin
   TSysStyleManager.UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
   TSysStyleManager.RegisterSysStyleHook('#32770', TSysDialogStyleHookNC);
 end;

 procedure RemoveStyleControl(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 var
  sHandle : PAnsiChar;
  Value: Integer;
 begin
   if not StyleServices.Available then exit;
   InitA(hwndParent, string_size, variables, stacktop, extraparameters);

   TSysStyleManager.UseStyleColorsChildControls:=False;
   sHandle:=PAnsiChar(PopStringA());
   //Addlog(Format('RemoveStyleControl (1) "%s"', [String(sHandle)]));
   if TryStrToInt(String(sHandle), Value) then
   begin

//     if TSysStyleManager.SysStyleHookList.ContainsKey(Value) then
//     begin
//        TSysStyleManager.SysStyleHookList.Items[Value].Free;
//        TSysStyleManager.SysStyleHookList.Remove(Value);
//     end;

     if TSysStyleManager.ChildRegSysStylesList.ContainsKey(Value) then
       TSysStyleManager.ChildRegSysStylesList.Remove(Value);

     NSIS_IgnoredControls.Add(Value);
     //Addlog(Format('RemoveStyleControl (2) "%s" %s', [String(sHandle), inttoHex(Value, 8)]));
   end;
 end;
{$ENDIF}


{$IFDEF NSIS_UNICODE}
 procedure LoadVCLStyle(const hwndParent: HWND; const string_size: integer; const variables: PChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 var
  VCLStyleFile : PChar;
 begin
   if not StyleServices.Available then exit;
   InitW(hwndParent, string_size, variables, stacktop, extraparameters);

   @_NSISCallBack := extrap.RegisterPluginCallback;
   _NSISCallBack(HInstance, @NSISCallback);

   VCLStyleFile:=PChar(PopStringW());

   if TStyleManager.IsValidStyle(String(VCLStyleFile)) then
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(String(VCLStyleFile)))
   else
   MessageBox(hwndParent,'Error', PChar(Format('The Style File %s is not valid',[VCLStyleFile])), MB_OK);
 end;

 procedure RemoveStyleNCArea(const hwndParent: HWND; const string_size: integer; const variables: PChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 begin
   TSysStyleManager.UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
   TSysStyleManager.RegisterSysStyleHook('#32770', TSysDialogStyleHookNC);
 end;

 procedure RemoveStyleControl(const hwndParent: HWND; const string_size: integer; const variables: PChar; const stacktop: pointer; const extraparameters: pointer = nil); cdecl;
 var
  sHandle : PChar;
  Value: Integer;
 begin
   if not StyleServices.Available then exit;
   InitW(hwndParent, string_size, variables, stacktop, extraparameters);

   TSysStyleManager.UseStyleColorsChildControls:=False;
   sHandle:=PChar(PopStringW());
   if TryStrToInt(String(sHandle), Value) then
   begin
     if TSysStyleManager.ChildRegSysStylesList.ContainsKey(Value) then
       TSysStyleManager.ChildRegSysStylesList.Remove(Value);

     NSIS_IgnoredControls.Add(Value);
   end;
 end;
{$ENDIF}

 procedure UnLoadVCLStyles; cdecl;
 begin
   if not StyleServices.Available then exit;
    //Vcl.Styles.NSIS.Done;
 end;

exports
  LoadVCLStyle, RemoveStyleNCArea, RemoveStyleControl;
begin
end.
