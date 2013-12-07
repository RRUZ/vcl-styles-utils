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
  KOLDetours,
  WinApi.Windows,
  Vcl.Styles,
  Vcl.Themes;

var
  TrampolineGetSysColor:  function (nIndex: Integer): DWORD; stdcall;
  GetSysColorOrgPointer : Pointer = nil;

function InterceptGetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle then
   Result:= TrampolineGetSysColor(nIndex)
  else
   Result:= StyleServices.GetSystemColor(nIndex or Integer($FF000000));
end;

initialization
 if StyleServices.Available then
 begin
   GetSysColorOrgPointer  := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   @TrampolineGetSysColor := InterceptCreate(GetSysColorOrgPointer, @InterceptGetSysColor);
 end;
finalization
 if GetSysColorOrgPointer<>nil then
  InterceptRemove(@TrampolineGetSysColor, @InterceptGetSysColor);

end.
