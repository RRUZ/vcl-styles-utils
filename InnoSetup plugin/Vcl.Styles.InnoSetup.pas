{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.InnoSetup                                                                        }
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
{ The Original Code is  Vcl.Styles.InnoSetup.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.InnoSetup;

interface

Procedure  Done;

implementation

{
  TNewEdit = class(TEdit)            ok
  TEdit                              ok
  TPasswordEdit                      ok
  TNewMemo = class(TMemo)            ow/ scrollbar
  TNewComboBox = class(TComboBox)    ok
  TNewListBox = class(TListBox)      ow/ scrollbar
  TListBox                           ow/ scrollbar
  TNewButton = class(TButton)        ok
  TNewCheckBox = class(TCheckBox)    ok
  TNewRadioButton = class(TRadioButton)
  TSelectFolderForm                  ok
  TFolderTreeView
  TStartMenuFolderTreeView
  TRichEditViewer                    ow/ scrollbar
  TNewStaticText                     ok
  TNewNotebook                       ok
  TNewNotebookPage                   ok
  TPanel                             ok
}

{$DEFINE USEGENERICS}   //-->Reduce the final exe/dll size

uses
  Winapi.Windows,
  Winapi.Messages,
  {$IFDEF USEGENERICS}
  System.Generics.Collections,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  {$IFDEF DEBUG}
  System.IOUtils,
  {$ENDIF}
  Vcl.Themes,
  Vcl.Styles.InnoSetup.StyleHooks,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.SysControls,
  Vcl.Styles.Utils.ComCtrls,
  Vcl.Styles.Utils.StdCtrls;

type
  TThemedInnoControls = class
  private
  class var
    FHook_WH_CALLWNDPROC: HHook;
  protected
    class function HookActionCallBackWndProc(nCode: Integer; wParam: wParam;
      lParam: lParam): LRESULT; stdcall; static;
    procedure InstallHook;
    procedure RemoveHook;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

{$IFNDEF USEGENERICS}
  TDictionary = class
  private
     FKeys, FValues : TList;
  public
    procedure Add(hwnd: HWND; Control : TControlWnd);
    function ContainsKey(hwnd: Winapi.Windows.HWND) : Boolean;
    constructor Create; overload;
    destructor Destroy; override;
  end;
{$ENDIF}

var
  {$IFDEF USEGENERICS}
  InnoSetupControlsList: TObjectDictionary<HWND, TSysStyleHook>;
  {$ELSE}
  InnoSetupControlsList: TDictionary;
  {$ENDIF}
  ClassesList : TStrings; //use a  TStrings to avoid the use of generics
  ThemedInnoControls: TThemedInnoControls;
  GetSysColorOrgPointer: Pointer = nil;
  //var TrampolineGetSysColor : function (nIndex: Integer): DWORD; stdcall;

{$IFDEF DEBUG}
procedure Addlog(const msg : string);
begin
   TFile.AppendAllText('C:\Delphi\google-code\vcl-styles-utils\log.txt',Format('%s %s %s',[FormatDateTime('hh:nn:ss.zzz', Now),  msg, sLineBreak]));
end;
{$ENDIF}

{$IFNDEF USEGENERICS}

{ TDictionary }

procedure TDictionary.Add(hwnd: HWND; Control: TControlWnd);
begin
 FKeys.Add(Pointer(hwnd));
 FValues.Add(Control);
end;

function TDictionary.ContainsKey(hwnd: Winapi.Windows.HWND): Boolean;
var
  i : integer;
begin
 Result:=False;
  for i := 0 to FKeys.Count-1 do
   if  Winapi.Windows.HWND(FKeys[i])=hwnd then
     Exit(True);
end;

constructor TDictionary.Create;
begin
 FKeys:=TList.Create;
 FValues:=TList.Create;
end;

destructor TDictionary.Destroy;
var
  i : integer;
begin
  FKeys.Free;
   for i := 0 to FValues.Count-1 do
      TControlWnd(FValues[i]).Free;

  FValues.Free;
  inherited;
end;
{$ENDIF}


{ TThemedSysControls }

constructor TThemedInnoControls.Create;
begin
  inherited;
  FHook_WH_CALLWNDPROC := 0;
  InstallHook;
  {$IFDEF USEGENERICS}
  InnoSetupControlsList := TObjectDictionary<HWND, TSysStyleHook>.Create([doOwnsValues]);
  {$ELSE}
  InnoSetupControlsList := TDictionary.Create;
  {$ENDIF}
  ClassesList := TStringList.Create;

//  with TSysStyleManager do
//    RegisterSysStyleHook('TNewEdit', TSysEditStyleHook);

end;

destructor TThemedInnoControls.Destroy;
begin
  RemoveHook;
  InnoSetupControlsList.Free;
  ClassesList.Free;
  inherited;
end;


class function TThemedInnoControls.HookActionCallBackWndProc(nCode: Integer;
  wParam: wParam; lParam: lParam): LRESULT;
var
  C: array [0 .. 256] of Char;
  sClassName : string;
begin
    Result := CallNextHookEx(FHook_WH_CALLWNDPROC, nCode, wParam, lParam);
    if (nCode < 0) then
     Exit;

    if (StyleServices.Enabled) and not (StyleServices.IsSystemStyle) then
    begin

      if ClassesList.IndexOfName(IntToStr(PCWPStruct(lParam)^.hwnd))=-1 then
      begin
        GetClassName(PCWPStruct(lParam)^.hwnd, C, 256);
        //Addlog('GetClassName ' + C);
        ClassesList.Add(Format('%d=%s',[PCWPStruct(lParam)^.hwnd, C]));
      end;

      if ClassesList.IndexOfName(IntToStr(PCWPStruct(lParam)^.hwnd))>=0 then
      begin
        sClassName:=ClassesList.Values[IntToStr(PCWPStruct(lParam)^.hwnd)]; //ClassesList[PCWPStruct(lParam)^.hwnd];

        {$IFDEF DEBUG}
        if (SameText(sClassName,'TNewMemo')) then
        Addlog(sClassName+' '+WM_To_String(PCWPStruct(lParam)^.message)+
        ' WParam '+IntToHex(PCWPStruct(lParam)^.wParam, 8) +
        ' lParam '+IntToHex(PCWPStruct(lParam)^.lParam, 8) +
        ' hwnd : '+ IntToHex(PCWPStruct(lParam)^.hwnd, 8) +
        ' WNDPROC : ' + IntToHex(GetWindowLongPtr(PCWPStruct(lParam)^.hwnd, GWL_WNDPROC), 8 )
        );
        {$ENDIF}

        if SameText(sClassName,'TNewButton') then
        begin
           //Addlog('TNewButton');
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNewButtonStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TWizardForm') or SameText(sClassName,'TSetupForm') or SameText(sClassName,'TSelectFolderForm') then
        begin
           if (PCWPStruct(lParam)^.message=WM_NCCALCSIZE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysDialogStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewComboBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysComboBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewCheckBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysCheckBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewRadioButton') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysRadioButtonStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TEdit') or SameText(sClassName,'TNewEdit')  or SameText(sClassName,'TPasswordEdit')  then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysEditStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else        //TSysScrollingStyleHook.PaintNC<>TScrollingStyleHook.PaintNC
        if SameText(sClassName,'TNewMemo') or SameText(sClassName,'TMemo')  then
        begin
           if (PCWPStruct(lParam)^.message=WM_NCCALCSIZE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysMemoStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewListBox') or SameText(sClassName,'TListBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_NCCALCSIZE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysListBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TNewCheckListBox') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNewCheckListBoxStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if SameText(sClassName,'TRichEditViewer') then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TRichEditViewerStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
//        if SameText(sClassName,'TNewStaticText') then
//        begin
//           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
//               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TStaticTextWnd.Create(PCWPStruct(lParam)^.hwnd));
//        end
//        else
        if (SameText(sClassName,'TNewProgressBar')) then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysProgressBarStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
        if (SameText(sClassName,'TStartMenuFolderTreeView')) or (SameText(sClassName,'TFolderTreeView'))  then
        begin
           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysTreeViewStyleHook.Create(PCWPStruct(lParam)^.hwnd));
        end
        else
//        if (SameText(sClassName,'TNewNotebook')) then     //TNewNotebook is handled by the Getsyscolors hook
//        begin
//           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
//               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TNotebookWnd.Create(PCWPStruct(lParam)^.hwnd));
//        end
//        else
//        if (SameText(sClassName,'TNewNotebookPage')) then   //TNewNotebookPage is handled by the Getsyscolors hook
//        begin
//           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
//               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TSysStyleHook.Create(PCWPStruct(lParam)^.hwnd));
//        end
//        else
//        if (SameText(sClassName,'TPanel')) then   //TPanel is handled by the Getsyscolors hook
//        begin
//           if (PCWPStruct(lParam)^.message=WM_CREATE) and not (InnoSetupControlsList.ContainsKey(PCWPStruct(lParam)^.hwnd)) then
//               InnoSetupControlsList.Add(PCWPStruct(lParam)^.hwnd, TPanelWnd.Create(PCWPStruct(lParam)^.hwnd));
//        end;
      end;
    end;
end;

procedure TThemedInnoControls.InstallHook;
begin
  FHook_WH_CALLWNDPROC := SetWindowsHookEx(WH_CALLWNDPROC, @TThemedInnoControls.HookActionCallBackWndProc, 0, GetCurrentThreadId);
end;

procedure TThemedInnoControls.RemoveHook;
begin
  if FHook_WH_CALLWNDPROC <> 0 then
    UnhookWindowsHookEx(FHook_WH_CALLWNDPROC);
end;

{

function ColorToRGB(Color: TColor): Longint;
begin
  if Color < 0 then
    Result := GetSysColor(Color and $000000FF) else
    Result := Color;
end;
}

//function GetSysColorHook(nIndex: Integer): DWORD; stdcall;
//begin
////  if nIndex = COLOR_BTNFACE then
////  begin
////   Result:= StyleServices.GetSystemColor(clBtnFace);
////   Addlog(IntToHex(Result, 8));
////  end
////  else
//   Result:= StyleServices.GetSystemColor(nIndex or Integer($FF000000));
//end;

Procedure  Done;
begin
if Assigned(ThemedInnoControls) then
  begin
    ThemedInnoControls.Free;
    ThemedInnoControls:=nil;
  end;

  //InterceptRemove(@TrampolineGetSysColor,@GetSysColorHook);
  //RedirectProcedure(@GetSysColorHook, @Winapi.Windows.GetSysColor);
end;


initialization

  ThemedInnoControls:=nil;
  if StyleServices.Available then
  begin
   ThemedInnoControls := TThemedInnoControls.Create;
   GetSysColorOrgPointer  := GetProcAddress(GetModuleHandle('user32.dll'), 'GetSysColor');
   //@TrampolineGetSysColor := InterceptCreate(GetSysColorOrgPointer, @GetSysColorHook);
   //RedirectProcedure(GetSysColorOrgPointer, @GetSysColorHook);
  end;
  TSysStyleManager.HookVclControls:=True;
finalization
   Done;
end.
