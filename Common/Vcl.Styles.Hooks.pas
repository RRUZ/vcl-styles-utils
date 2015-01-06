//**************************************************************************************************
//
// Unit Vcl.Styles.Hooks
// unit for the VCL Styles Utils
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
// The Original Code is Vcl.Styles.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.Hooks;

interface

implementation

{$DEFINE HOOK_UXTHEME}
{$DEFINE HOOK_TDateTimePicker}
{$DEFINE HOOK_TProgressBar}

uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Graphics,
{$IFDEF HOOK_UXTHEME}
  Vcl.Styles.UxTheme,
{$ENDIF}
  Vcl.Styles.Utils.SysControls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Themes;

type
  TListStyleBrush = TObjectDictionary<Integer, TBrush>;

var
  VCLStylesBrush   : TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock    : TCriticalSection = nil;

  TrampolineGetSysColor           : function (nIndex: Integer): DWORD; stdcall =  nil;
  TrampolineGetSysColorBrush      : function (nIndex: Integer): HBRUSH; stdcall=  nil;

function Detour_GetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled  then
    Result:= TrampolineGetSysColor(nIndex)
  else
  if nIndex= COLOR_HOTLIGHT then
    Result:= DWORD(StyleServices.GetSystemColor(clHighlight))
  else
    Result:= DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));

  //OutputDebugString(PChar('Detour_GetSysColor nIndex '+IntToStr(nIndex)) );
end;



function Detour_GetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;
var
  LCurrentStyleBrush : TListStyleBrush;
  LBrush : TBrush;
  LColor : TColor;
  //LogBrush: TLogBrush;
begin
  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled  then
     Exit(TrampolineGetSysColorBrush(nIndex))
    else
    begin
     if VCLStylesBrush.ContainsKey(StyleServices.Name) then
      LCurrentStyleBrush:=VCLStylesBrush.Items[StyleServices.Name]
     else
     begin
       VCLStylesBrush.Add(StyleServices.Name, TListStyleBrush.Create([doOwnsValues]));
       LCurrentStyleBrush:=VCLStylesBrush.Items[StyleServices.Name];
     end;

     if LCurrentStyleBrush.ContainsKey(nIndex) then
     begin
      LBrush:=LCurrentStyleBrush.Items[nIndex];
//      if GetObject(LBrush.Handle, SizeOf(TLogBrush), @LogBrush) <> 0 then
//        OutputDebugString(PChar(Format('nIndex %d Color %x RGB %x  GetObject %x', [nIndex, LBrush.Color, ColorToRGB(LBrush.Color), LogBrush.lbColor])));
      Exit(LBrush.Handle);
     end
     else
//     if LCurrentStyleBrush.ContainsKey(nIndex) then
//      LCurrentStyleBrush.Remove(nIndex);
     begin
       LBrush:=TBrush.Create;
       LCurrentStyleBrush.Add(nIndex, LBrush);
       if nIndex= COLOR_WINDOW then
       begin
         LBrush.Color:= StyleServices.GetSystemColor(clWindow);
         LColor := LBrush.Color;
         case LColor of
          $303030, $232323, $644239, $121212 : LBrush.Color:= LColor + 1;
         end;
       end
       else
       if nIndex= COLOR_HOTLIGHT then
         LBrush.Color:=StyleServices.GetSystemColor(clHighlight)
       else
         LBrush.Color:= StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000)));
       //OutputDebugString(PChar(Format('nIndex %d Color %x RGB %x', [nIndex, LBrush.Color, ColorToRGB(LBrush.Color)])));
       Exit(LBrush.Handle);
     end;
    end;
  finally
    VCLStylesLock.Leave;
  end;
end;


initialization
  VCLStylesLock := TCriticalSection.Create;
  VCLStylesBrush := TObjectDictionary<string, TListStyleBrush>.Create([doOwnsValues]);

 if StyleServices.Available then
 begin

   {$IFDEF  HOOK_TDateTimePicker}
   TCustomStyleEngine.RegisterStyleHook(TDateTimePicker, TStyleHook);
   {$ENDIF}
   {$IFDEF  HOOK_TProgressBar}
   TCustomStyleEngine.RegisterStyleHook(TProgressBar, TStyleHook);
   {$ENDIF}

   @TrampolineGetSysColor         :=  InterceptCreate(user32, 'GetSysColor', @Detour_GetSysColor);
   @TrampolineGetSysColorBrush    :=  InterceptCreate(user32, 'GetSysColorBrush', @Detour_GetSysColorBrush);
 end;

finalization
  InterceptRemove(@TrampolineGetSysColor);
  InterceptRemove(@TrampolineGetSysColorBrush);

  VCLStylesBrush.Free;
  VCLStylesLock.Free;
  VCLStylesLock := nil;
end.
