{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.SysControls                                                                      }
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
{ The Original Code is uThemedSysControls.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.SysControls;

interface

implementation

uses
  Winapi.Windows,
  System.Generics.Collections,
  System.SysUtils,
  Vcl.Controls,
  Vcl.Dialogs,
  Vcl.Themes,
  Vcl.Styles.PopupWnd,
  Vcl.Styles.EditWnd,
  Vcl.Styles.StaticWnd,
  Vcl.Styles.ThemedDialog,
  Vcl.Styles.ToolbarWindow32Wnd,
  Vcl.Styles.SysListView32Wnd,
  Vcl.Styles.ButtonWnd,
  Vcl.Styles.UnknownControlWnd,
  Vcl.Styles.ControlWnd,
  Vcl.Styles.ComboBoxWnd,
  Vcl.Styles.ToolTipsWnd;

type
  TThemedSysControls = class
  private
  class var
    FBalloonHint: TBalloonHint;
    FPreviousSysBtn: Integer;
    FPreviousHandle: THandle;
    FHook: HHook;
  protected
    class function HookActionCallBack(Code: Integer; wParam: wParam;
      lParam: lParam): LRESULT; stdcall; static;
    procedure InstallHook;
    procedure RemoveHook;
  public
    constructor Create; overload;
    destructor Destroy; override;
  end;

  PInstruction = ^TInstruction;

  TInstruction = packed record
    Opcode: Byte;
    Offset: Integer;
  end;

var
  InsertMenuItemOrgPointer: Pointer = nil;
  MenuItemInfoArray: array of TMenuItemInfo;
  SysControlsWndList: TObjectDictionary<HWND, TControlWnd>;
  PopupWndList: TObjectDictionary<HWND, TPopupWnd>;
  ThemedSysControls: TThemedSysControls;

{ the Original InsertMenuItem function }
function InsertMenuItemOrg(p1: HMENU; p2: UINT; p3: BOOL;
  const p4: TMenuItemInfo): BOOL; stdcall;
  external user32 name 'InsertMenuItemW';

procedure PatchCode(Address: Pointer; const NewCode; Size: Integer);
var
  OldProtect: DWORD;
begin
  if VirtualProtect(Address, Size, PAGE_EXECUTE_READWRITE, OldProtect) then
    begin
      Move(NewCode, Address^, Size);
      FlushInstructionCache(GetCurrentProcess, Address, Size);
      VirtualProtect(Address, Size, OldProtect, @OldProtect);
    end;
end;

procedure RedirectProcedure(OldAddress, NewAddress: Pointer);
var
  NewCode: TInstruction;
begin
  NewCode.Opcode := $E9; // jump relative
  NewCode.Offset := NativeInt(NewAddress) - NativeInt(OldAddress) -
    SizeOf(NewCode);
  PatchCode(OldAddress, NewCode, SizeOf(NewCode));
end;

function InsertMenuItemHook(p1: HMENU; p2: UINT; p3: BOOL;
  const p4: TMenuItemInfo): BOOL; stdcall;
var
  L: Integer;
begin
  { If Menu is Owner Draw , the dwTypeData will be destroyed
    So the GetHMenuItemText function will fails  .
    We need to copy the Text String to a Pointer that hold the Text (dwItemData).
  }
  if p4.fType and MFT_OWNERDRAW = MFT_OWNERDRAW then
    begin
      L := Length(MenuItemInfoArray) + 1;
      SetLength(MenuItemInfoArray, L);
      MenuItemInfoArray[L - 1] := p4;
      MenuItemInfoArray[L - 1].dwItemData :=
        NativeInt(MenuItemInfoArray[L - 1].dwTypeData);
      Result := InsertMenuItemOrg(p1, p2, p3, MenuItemInfoArray[L - 1]);
    end
  else
    { Call the Original InsertMenuItem function }
    Result := InsertMenuItemOrg(p1, p2, p3, p4);
end;



{ TThemedSysControls }

constructor TThemedSysControls.Create;
begin
  inherited;
  FBalloonHint := TBalloonHint.Create(nil);
  FBalloonHint.Style := bhsStandard;
  FBalloonHint.Delay := 1500;
  FBalloonHint.HideAfter := 3000;
  FPreviousHandle := 0;
  FHook := 0;
  InstallHook;
  SysControlsWndList:= TObjectDictionary<HWND, TControlWnd>.Create([doOwnsValues]);
  PopupWndList:= TObjectDictionary<HWND, TPopupWnd>.Create([doOwnsValues]);
end;

destructor TThemedSysControls.Destroy;
begin
  RemoveHook;
  SysControlsWndList.Free;
  PopupWndList.Free;
  FBalloonHint.Free;
  inherited;
end;

class function TThemedSysControls.HookActionCallBack(Code: Integer;
  wParam: wParam; lParam: lParam): LRESULT;
var
  Msg: TMOUSEHOOKSTRUCT;
  C: array [0 .. 256] of Char;

  { Note :  there is only one window that desplay
    Max,Min,Help,Close hint in the System Windows .
    => the handle of this window is fixed !!
    Yes it's fixed .. that's means : there is only one
    Handle : the handle value is :  [65550,65600] in windows (98,XP,Win7).
    Not sure about Win8...
  }
  procedure HideSysToolTip;
  var
    hSysToolTip: THandle;
  begin
    For hSysToolTip := 65550 To 65600 do
      begin
        If IsWindowVisible(hSysToolTip) then
          begin
            GetClassName(hSysToolTip, C, 256);
            ShowWindow(hSysToolTip, SW_HIDE);
          end;
      end;
  end;

  procedure ShowToolTip(HintTitle: String);
  begin
    HideSysToolTip;
    if FPreviousSysBtn <> Integer(Msg.wHitTestCode) then
      begin
        FBalloonHint.HideHint;
        FBalloonHint.Title := HintTitle;
        FPreviousSysBtn := Msg.wHitTestCode;
        FBalloonHint.ShowHint(Msg.pt);
      end;
  end;

var
  CBTSturct: TCBTCreateWnd;
  sClassName : string;
begin
    if (StyleServices.Enabled) and not (StyleServices.IsSystemStyle) then
    begin
      if Code = HCBT_SYSCOMMAND then
        begin
          FBalloonHint.HideHint;
          FPreviousSysBtn := 0;
        end
      else
      if Code = HCBT_DESTROYWND then
      begin
        if SysControlsWndList.ContainsKey(wParam) then
          SysControlsWndList.Remove(wParam);
      end
      else
      if Code = HCBT_CREATEWND then
        begin
          CBTSturct := PCBTCreateWnd(lParam)^;
          sClassName := GetWindowClassName(wParam);
          //PopupMenu
          if Integer(CBTSturct.lpcs.lpszClass) = 32768 then
              PopupWndList.Add(wParam, TPopupWnd.Create(wParam))
          else
          //Dialog
          if (Integer(CBTSturct.lpcs.lpszClass) = 32770) then
            begin
              if (CBTSturct.lpcs.cx <> 0) and (CBTSturct.lpcs.cy <> 0) then
              begin
                SysControlsWndList.Add(wParam, TDialogWnd.Create(wParam));
              end;
            end
          else
          if (sClassName = 'Button')  then
              SysControlsWndList.Add(wParam, TButtonWnd.Create(wParam))
          else
          if (sClassName = 'ScrollBar') or (sClassName = 'ReBarWindow32') {or (sClassName = 'ToolbarWindow32')} then
              SysControlsWndList.Add(wParam, TUnknownControlWnd.Create(wParam))
          else
          if sClassName = 'SysListView32' then
              SysControlsWndList.Add(wParam, TSysListView32Wnd.Create(wParam))
          else
          if sClassName = 'ToolbarWindow32' then
            begin
              if not UseLatestCommonDialogs then
                SysControlsWndList.Add(wParam, TToolbarWindow32Wnd.Create(wParam));
            end
          else
          if (sClassName = 'Edit')  then
              SysControlsWndList.Add(wParam, TEditWnd.Create(wParam))
          else
          if (sClassName = 'Static') then
            begin
              { This condition can solve the Edit animated cursor : see ColorDialog !! }
              if (CBTSturct.lpcs.Style and SS_ICON <> SS_ICON) and
                (CBTSturct.lpcs.Style and SS_BITMAP <> SS_BITMAP) and
                (CBTSturct.lpcs.Style and SS_GRAYRECT <> SS_GRAYRECT) and
                (CBTSturct.lpcs.Style and SS_GRAYFRAME <> SS_GRAYFRAME) then
                  SysControlsWndList.Add(wParam, TStaticWnd.Create(wParam));
            end
          else
          if (sClassName = 'ComboBox') then
            SysControlsWndList.Add(wParam, TComboBoxWnd.Create(wParam))
          else
          if (sClassName = 'tooltips_class32') then
            SysControlsWndList.Add(wParam, TooltipsWnd.Create(wParam))
        end
    end;
  Result := CallNextHookEx(FHook, Code, wParam, lParam);
end;

procedure TThemedSysControls.InstallHook;
begin
  FHook := SetWindowsHookEx(WH_CBT, @TThemedSysControls.HookActionCallBack, 0, GetCurrentThreadId);
end;

procedure TThemedSysControls.RemoveHook;
begin
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
end;

initialization

  ThemedSysControls:=nil;
  if StyleServices.Available then
  begin
    ThemedSysControls := TThemedSysControls.Create;
    InsertMenuItemOrgPointer := @Winapi.Windows.InsertMenuItem;
    RedirectProcedure(@Winapi.Windows.InsertMenuItem, @InsertMenuItemHook);
  end;

finalization

if Assigned(ThemedSysControls) then
  begin
    ThemedSysControls.Free;
    RedirectProcedure(@Winapi.Windows.InsertMenuItem, InsertMenuItemOrgPointer);
  end;

end.
