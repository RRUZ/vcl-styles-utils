{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Utils.SysControls                                                                }
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
{                                                                                                  }
{ Portions created by Safafi Mahdi [SMP3]   e-mail SMP@LIVE.FR                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Utils.SysControls;
{$DEFINE EventLog}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Styles,
  Vcl.Themes,
  System.Types,
  System.SysUtils,
  Vcl.Dialogs,
  Vcl.Styles.Utils.SysStyleHook,
  Generics.Collections;

type
  TSysStyleManager = class(TComponent)
  private
  class var
    FEnabled: Boolean;
    FHook: HHook;
    FRegSysStylesList: TObjectDictionary<String, TSysStyleHookClass>;
    FSysStyleHookList: TObjectDictionary<HWND, TSysStyleHook>;
    FChildRegSysStylesList: TObjectDictionary<HWND, TSysStyleHookClass>;
    FHookVclControls: Boolean;
  protected
    class procedure InstallHook;
    class procedure RemoveHook;
    class function HookCBProc(nCode: Integer; wParam: wParam; lParam: lParam)
      : LRESULT; stdcall; static;
  public
    class procedure RegisterSysStyleHook(SysControlClass: String;
      SysStyleHookClass: TSysStyleHookClass);
    class procedure UnRegisterSysStyleHook(SysControlClass: String;
      SysStyleHookClass: TSysStyleHookClass);
    class constructor Create;
    class destructor Destroy;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    class property Enabled: Boolean read FEnabled write FEnabled;
    class property HookVclControls: Boolean read FHookVclControls
      write FHookVclControls;
  end;

function GetWindowText(Window: HWND): String;
function GetWindowClassName(Window: HWND): String;
function GET_X_LPARAM(Value: DWORD): Longint;
function GET_Y_LPARAM(Value: DWORD): Longint;
function RectVCenter(var R: TRect; Bounds: TRect): TRect;
procedure MoveWindowOrg(DC: HDC; DX, DY: Integer);
function GetBmpInfo(hBmp: HBITMAP): BITMAP;
function GetBitmapHeight(hBmp: HBITMAP): Integer;
function GetBitmapWidth(hBmp: HBITMAP): Integer;
function BmpToIcon(hBmp: HBITMAP): HICON;
procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);
function GetMenuItemPos(Menu: HMENU; ID: Integer): Integer;
function IsItemDisabled(Menu: HMENU; Index: Integer): Boolean;
procedure FillDC(DC: HDC; R: TRect; Color: TColor);
function IsWindowMsgBox(Handle: HWND): Boolean;

{$IFDEF EventLog}
procedure AddToLog(Msg: TMessage); overload;
procedure AddToLog(S: string; Value: Integer); overload;
procedure AddToLog(Msg: string); overload;
{$ENDIF }

implementation

//uses
//  Vcl.Styles.Utils.Forms,
//  Vcl.Styles.Utils.Menus,
//  Vcl.Styles.Utils.ComCtrls,
//  Vcl.Styles.Utils.ScreenTips,
//  Vcl.Styles.Utils.StdCtrls;

{ TSysStyleManager }

constructor TSysStyleManager.Create(AOwner: TComponent);
begin
  inherited;

end;

class destructor TSysStyleManager.Destroy;
var
  SysStyleHook: TSysStyleHook;
begin
  RemoveHook;
  FRegSysStylesList.Free;

  for SysStyleHook in FSysStyleHookList.Values do
    if Assigned(SysStyleHook) then
      SysStyleHook.Free;
  FSysStyleHookList.Free;
  FChildRegSysStylesList.Free;
  inherited;
end;

class constructor TSysStyleManager.Create;
begin
  FEnabled := True;
  FHookVclControls := False;
  FSysStyleHookList := TObjectDictionary<HWND, TSysStyleHook>.Create;
  FRegSysStylesList := TObjectDictionary<String, TSysStyleHookClass>.Create;
  FChildRegSysStylesList := TObjectDictionary<HWND, TSysStyleHookClass>.Create;
  InstallHook;
end;

destructor TSysStyleManager.Destroy;
begin

end;

class function TSysStyleManager.HookCBProc(nCode: Integer; wParam: wParam;
  lParam: lParam): LRESULT;
var
  CBTSturct: TCBTCreateWnd;
  sClassName: string;
  Parent: TWinControl;
  Control: TWinControl;
begin
  if not FEnabled then
  begin
    Result := CallNextHookEx(FHook, nCode, wParam, lParam);
    Exit;
  end;
  if (nCode = HCBT_CREATEWND) and not(StyleServices.IsSystemStyle) then
  begin
    CBTSturct := PCBTCreateWnd(lParam)^;
    sClassName := GetWindowClassName(wParam);
    sClassName := LowerCase(sClassName);

    if FRegSysStylesList.ContainsKey(sClassName) then
    begin
      if not HookVclControls then
      begin
        Control := FindControl(wParam);
        { If control was created by the VCL ==> Skip . }
        if Control <> nil then
        begin
          Result := CallNextHookEx(FHook, nCode, wParam, lParam);
          Exit;
        end;
      end;

      if CBTSturct.lpcs.style and WS_POPUP = WS_POPUP then
      begin
        { If Control is parent . }
        FSysStyleHookList.Add(wParam, FRegSysStylesList[sClassName].Create(wParam));
      end
      else if CBTSturct.lpcs.style and WS_CHILD = WS_CHILD then
      begin
        { If Control is child . }
        Parent := FindControl(CBTSturct.lpcs.hwndParent);
        {
          If the Parent is a VCL control
          => Do not add the child control to the child list .
        }
        if Parent <> nil then
          FSysStyleHookList.Add(wParam, FRegSysStylesList[sClassName]
            .Create(wParam))
        else
        begin
          { If parent control of child control is registered . }
          if FSysStyleHookList.ContainsKey(CBTSturct.lpcs.hwndParent) then
            FChildRegSysStylesList.Add(wParam, FRegSysStylesList[sClassName])
          else
            { Parent not found . }
            FSysStyleHookList.Add(wParam, FRegSysStylesList[sClassName]
              .Create(wParam));
        end;
      end
      else
        { Not WS_POPUP & not WS_CHILD !!! }
        FSysStyleHookList.Add(wParam, FRegSysStylesList[sClassName]
          .Create(wParam));
    end;

    if nCode = HCBT_DESTROYWND then
    begin
      if FSysStyleHookList.ContainsKey(wParam) then
      begin
        FSysStyleHookList[wParam].Free;
        FSysStyleHookList.Remove(wParam);
      end;
    end;
  end;
  Result := CallNextHookEx(FHook, nCode, wParam, lParam);
end;

class procedure TSysStyleManager.InstallHook;
begin
  FHook := SetWindowsHookEx(WH_CBT, @HookCBProc, 0, GetCurrentThreadId);
end;

class procedure TSysStyleManager.RegisterSysStyleHook(SysControlClass: String;
  SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
  FRegSysStylesList.Add(LowerCase(SysControlClass), SysStyleHookClass);
end;

class procedure TSysStyleManager.RemoveHook;
begin
  if FHook <> 0 then
    UnhookWindowsHookEx(FHook);
end;

class procedure TSysStyleManager.UnRegisterSysStyleHook(SysControlClass: String;
  SysStyleHookClass: TSysStyleHookClass);
begin
  if FRegSysStylesList.ContainsKey(LowerCase(SysControlClass)) then
    FRegSysStylesList.Remove(LowerCase(SysControlClass));
end;

{$IFDEF EventLog}

procedure AddToLog(Msg: TMessage);
begin
  with Msg do
    OutputDebugString(PChar('Msg = ' + IntToStr(Msg) + ' wParam = ' +
      IntToStr(wParam) + ' LParam = ' + IntToStr(lParam)));
end;

procedure AddToLog(S: string; Value: Integer);
begin
  OutputDebugString(PChar((S) + ' = ' + IntToStr(Value)));
end;

procedure AddToLog(Msg: string);
begin
  OutputDebugString(PChar(Msg));
end;
{$ENDIF}

function GetWindowText(Window: HWND): String;
var
  Text: PChar;
begin
  GetMem(Text, 256);
  try
    Winapi.Windows.GetWindowText(Window, Text, 256);
    Result := String(Text);
  finally
    FreeMem(Text, 256);
  end;
end;

function GetWindowClassName(Window: HWND): String;
var
  sClassName: PChar;
begin
  GetMem(sClassName, 256);
  try
    GetClassName(Window, sClassName, 256);
    Result := String(sClassName);
  finally
    FreeMem(sClassName, 256);
  end;
end;

{$IFDEF PUREPASCAL}

function GET_X_LPARAM(Value: DWORD): Longint;
begin
  Result := Value and $FFFF;
end;

function GET_Y_LPARAM(Value: DWORD): Longint;
begin
  Result := Value SHR 16;
end;

{$ELSE}

function GET_X_LPARAM(Value: DWORD): Longint;
asm
  {$IFDEF CPUX64}
  MOV EAX,ECX
  {$ENDIF}
  AND EAX,$FFFF
end;

function GET_Y_LPARAM(Value: DWORD): Longint;
asm
  {$IFDEF CPUX64}
  MOV EAX,ECX
  {$ENDIF}
  SHR EAX,16
end;
{$ENDIF}

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

procedure MoveWindowOrg(DC: HDC; DX, DY: Integer);
var
  P: TPoint;
begin
  GetWindowOrgEx(DC, P);
  SetWindowOrgEx(DC, P.X - DX, P.Y - DY, nil);
end;

function GetBmpInfo(hBmp: HBITMAP): BITMAP;
begin
  ZeroMemory(@Result, sizeof(BITMAP));
  GetObject(hBmp, sizeof(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: BITMAP;
  hbmMask: HBITMAP;
  DC: HDC;
  ii: ICONINFO;
  Icon: HICON;
begin
  FillChar(Bmp, sizeof(BITMAP), Char(0));
  GetObject(hBmp, sizeof(BITMAP), @Bmp);
  DC := GetDC(0);
  hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
  ii.fIcon := True;
  ii.hbmColor := hBmp;
  ii.hbmMask := hbmMask;
  Icon := CreateIconIndirect(ii);
  DeleteObject(hbmMask);
  ReleaseDC(0, DC);
  Result := Icon;
end;

procedure RotateBitmap(Bmp: TBitmap; Rads: Single; AdjustSize: Boolean;
  BkColor: TColor = clNone);
var
  C: Single;
  S: Single;
  XForm: tagXFORM;
  Tmp: TBitmap;
begin
  C := Cos(Rads);
  S := Sin(Rads);
  XForm.eM11 := C;
  XForm.eM12 := S;
  XForm.eM21 := -S;
  XForm.eM22 := C;
  Tmp := TBitmap.Create;
  try
    Tmp.TransparentColor := Bmp.TransparentColor;
    Tmp.TransparentMode := Bmp.TransparentMode;
    Tmp.Transparent := Bmp.Transparent;
    Tmp.Canvas.Brush.Color := BkColor;
    if AdjustSize then
    begin
      Tmp.Width := Round(Bmp.Width * Abs(C) + Bmp.Height * Abs(S));
      Tmp.Height := Round(Bmp.Width * Abs(S) + Bmp.Height * Abs(C));
      XForm.eDx := (Tmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Tmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end
    else
    begin
      Tmp.Width := Bmp.Width;
      Tmp.Height := Bmp.Height;
      XForm.eDx := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
      XForm.eDy := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;
    end;
    SetGraphicsMode(Tmp.Canvas.Handle, GM_ADVANCED);
    SetWorldTransform(Tmp.Canvas.Handle, XForm);
    BitBlt(Tmp.Canvas.Handle, 0, 0, Tmp.Width, Tmp.Height, Bmp.Canvas.Handle, 0,
      0, SRCCOPY);
    Bmp.Assign(Tmp);
  finally
    Tmp.Free;
  end;
end;

function GetMenuItemPos(Menu: HMENU; ID: Integer): Integer;
var
  i: Integer;
  mii: MENUITEMINFO;
begin
  Result := -1;
  if Menu = 0 then
    Exit;
  for i := 0 to GetMenuItemCount(Menu) do
  begin
    FillChar(mii, sizeof(mii), Char(0));
    mii.cbSize := sizeof(mii);
    mii.fMask := MIIM_ID;
    if (GetMenuItemInfo(Menu, i, True, mii)) then
      if mii.wID = Cardinal(ID) then
        Exit(i);
  end;
end;

function IsItemDisabled(Menu: HMENU; Index: Integer): Boolean;
var
  info: TMenuItemInfo;
begin
  Result := False;
  if (Menu = 0) or (Index < 0) then
    Exit;

  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(Menu, Index, True, info);
  Result := (info.fState and MFS_DISABLED = MFS_DISABLED) or
    (info.fState and MF_DISABLED = MF_DISABLED) or
    (info.fState and MF_GRAYED = MF_GRAYED);
end;

procedure FillDC(DC: HDC; R: TRect; Color: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(Color);
  FillRect(DC, R, Brush);
  DeleteObject(Brush);
end;

function IsWindowMsgBox(Handle: HWND): Boolean;
begin
  Result := ((FindWindowEx(Handle, 0, 'Edit', nil) = 0) and
    (GetDlgItem(Handle, $FFFF) <> 0)) and
    (GetWindowLongPtr(Handle, GWL_USERDATA) <> 0);
end;

initialization
UseLatestCommonDialogs := False;

finalization


end.
