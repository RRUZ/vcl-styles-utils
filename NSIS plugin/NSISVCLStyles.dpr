{**************************************************************************************************}
{                                                                                                  }
{ NSISVCLStyles VCL Styles Plugin for NSIS                                                         }
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
{ The Original Code is  NSISVCLStyles.dpr                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2014 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
library NSISVCLStyles;

{$IFNDEF DEBUG}
  {$IFOPT D-}{$WEAKLINKRTTI ON}{$ENDIF}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$ENDIF}

{$SetPEFlags $2000}
uses
  System.SysUtils,
  WinApi.Windows,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Dialogs,
  //Vcl.Styles.Hooks in '..\Common\Vcl.Styles.Hooks.pas',
  //KOLDetours in '..\Common\KOLDetours.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.ComCtrls in '..\Common\Vcl.Styles.Utils.ComCtrls.pas',
  Vcl.Styles.Utils.Forms in '..\Common\Vcl.Styles.Utils.Forms.pas',
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.ScreenTips in '..\Common\Vcl.Styles.Utils.ScreenTips.pas',
  Vcl.Styles.Utils.StdCtrls in '..\Common\Vcl.Styles.Utils.StdCtrls.pas';

  //NSIS Scripting Reference
  //http://nsis.sourceforge.net/Docs/Chapter4.html
{$R *.res}

type
  VarConstants = (
    INST_0,       // $0
    INST_1,       // $1
    INST_2,       // $2
    INST_3,       // $3
    INST_4,       // $4
    INST_5,       // $5
    INST_6,       // $6
    INST_7,       // $7
    INST_8,       // $8
    INST_9,       // $9
    INST_R0,      // $R0
    INST_R1,      // $R1
    INST_R2,      // $R2
    INST_R3,      // $R3
    INST_R4,      // $R4
    INST_R5,      // $R5
    INST_R6,      // $R6
    INST_R7,      // $R7
    INST_R8,      // $R8
    INST_R9,      // $R9
    INST_CMDLINE, // $CMDLINE
    INST_INSTDIR, // $INSTDIR
    INST_OUTDIR,  // $OUTDIR
    INST_EXEDIR,  // $EXEDIR
    INST_LANG,    // $LANGUAGE
    __INST_LAST
    );
  TVariableList = INST_0..__INST_LAST;
  pstack_t = ^stack_t;
  stack_t = record
    next: pstack_t;
    text: PAnsiChar;
  end;

var
  g_stringsize: integer;
  g_stacktop: ^pstack_t;
  g_variables: PAnsiChar;
  g_hwndParent: HWND;

procedure Init(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer);
begin
  g_stringsize := string_size;
  g_hwndParent := hwndParent;
  g_stacktop   := stacktop;
  g_variables  := variables;
end;

function PopString(): AnsiString;
var
  th: pstack_t;
begin
  if integer(g_stacktop^) <> 0 then
  begin
    th := g_stacktop^;
    Result :=PAnsiChar(@th.text);
    g_stacktop^ := th.next;
    GlobalFree(HGLOBAL(th));
  end;
end;

procedure PushString(const str: AnsiString='');
var
  th: pstack_t;
begin
  if integer(g_stacktop) <> 0 then
  begin
    th := pstack_t(GlobalAlloc(GPTR, SizeOf(stack_t) + g_stringsize));
    lstrcpynA(@th.text, PAnsiChar(str), g_stringsize);
    th.next := g_stacktop^;
    g_stacktop^ := th;
  end;
end;

function GetUserVariable(const varnum: TVariableList): AnsiString;
begin
  if (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    Result := g_variables + integer(varnum) * g_stringsize
  else
    Result := '';
end;

procedure SetUserVariable(const varnum: TVariableList; const value: AnsiString);
begin
  if (value <> '') and (integer(varnum) >= 0) and (integer(varnum) < integer(__INST_LAST)) then
    lstrcpyA(g_variables + integer(varnum) * g_stringsize, PAnsiChar(value));
end;

 //procedure LoadVCLStyleA(VCLStyleFile: PAnsiChar); cdecl;
 procedure LoadVCLStyleA(const hwndParent: HWND; const string_size: integer; const variables: PAnsiChar; const stacktop: pointer); cdecl;
 var
  VCLStyleFile : PAnsiChar;
 begin
   if not StyleServices.Available then exit;

   Init(hwndParent, string_size, variables, stacktop);
   VCLStyleFile:=PAnsiChar(PopString);

   //ShowMessage(VCLStyleFile);

   if TStyleManager.IsValidStyle(VCLStyleFile) then
     TStyleManager.SetStyle(TStyleManager.LoadFromFile(VCLStyleFile))
   else
   ShowMessage(Format('The Style File %s is not valid',[VCLStyleFile]));
 end;

 procedure UnLoadVCLStyles; cdecl;
 begin
   if not StyleServices.Available then exit;
    //Vcl.Styles.NSIS.Done;
 end;

exports
  LoadVCLStyleA, UnLoadVCLStyles;
begin
end.
