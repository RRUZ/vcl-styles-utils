{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Hooks                                                                            }
{ unit for the VCL Styles Utils                                                                    }
{ http://code.google.com/p/vcl-styles-utils/                                                       }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is Vcl.Styles.Hooks.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Hooks;

interface

implementation

uses
  {$IFDEF DEBUG}
  System.IOUtils,
  System.SysUtils,
  {$ENDIF}
  KOLDetours,
  Winapi.UxTheme,
  WinApi.Windows,
  Vcl.Themes;

var
  //ThemeLibrary: THandle;
  TrampolineOpenThemeData: function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
  TrampolineGetSysColor  : function (nIndex: Integer): DWORD; stdcall;
  TrampolineGetThemeSysColor :  function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;

  GetSysColorOrgPointer  : Pointer = nil;
  OpenThemeDataOrgPointer: Pointer = nil;
  GetThemeSysColorOrgPointer : Pointer = nil;


function InterceptOpenThemeData(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
begin
   Result:= TrampolineOpenThemeData(hwnd, pszClassList);
end;

function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle then
   Result:= TrampolineGetSysColor(nIndex)
  else
   Result:= StyleServices.GetSystemColor(nIndex or Integer($FF000000));
end;

function InterceptGetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
begin
  if StyleServices.IsSystemStyle then
   Result:= TrampolineGetThemeSysColor(hTheme, iColorId)
  else
   Result:= StyleServices.GetSystemColor(iColorId or Integer($FF000000));
end;


initialization
 if StyleServices.Available then
 begin
   //ThemeLibrary := LoadLibrary('uxtheme.dll');
   {$IFDEF DEBUG}
   Addlog(Format('ThemeLibrary %x %d',[Integer(ThemeLibrary), Integer(ThemeLibrary)]));
   {$ENDIF}

           //original
   GetSysColorOrgPointer  := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   @TrampolineGetSysColor := InterceptCreate(GetSysColorOrgPointer, @InterceptGetSysColor);

        // original
     {
   OpenThemeDataOrgPointer  := GetProcAddress(ThemeLibrary, 'OpenThemeData');
   @TrampolineOpenThemeData := InterceptCreate(OpenThemeDataOrgPointer, @InterceptOpenThemeData);

   GetThemeSysColorOrgPointer  := GetProcAddress(ThemeLibrary, 'GetThemeSysColor');
   @TrampolineGetThemeSysColor := InterceptCreate(GetThemeSysColorOrgPointer, @InterceptGetSysColor);
   }
 end;

finalization
 if GetSysColorOrgPointer<>nil then
  InterceptRemove(@TrampolineGetSysColor, @InterceptGetSysColor);
        {
 if OpenThemeDataOrgPointer<>nil then
  InterceptRemove(@TrampolineOpenThemeData, @InterceptOpenThemeData);
        }
end.
