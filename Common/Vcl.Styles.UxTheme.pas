//**************************************************************************************************
//
// Unit Vcl.Styles.UxTheme
// unit for the VCL Styles Utils
// https://github.com/RRUZ/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is Vcl.Styles.UxTheme.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
// All Rights Reserved.
//
//**************************************************************************************************
unit Vcl.Styles.UxTheme;

interface

implementation

{
   W8  Check Color text disabled on listview hot item
   Test on W10
   Test Task Dialogs
   fix preview windows background color when no elements are shown
   fix background of homegroup folder (and related)
}




{$DEFINE HOOK_Button}
{$DEFINE HOOK_Scrollbar}
{$DEFINE HOOK_TaskDialog}
{$DEFINE HOOK_ProgressBar}
{$DEFINE HOOK_DateTimePicker}
{$DEFINE HOOK_TreeView}
{$DEFINE HOOK_ListView}
{$DEFINE HOOK_ListBox}
{$DEFINE HOOK_ComboBox}
{$DEFINE HOOK_Spin}
{$DEFINE HOOK_EDIT}
{$DEFINE HOOK_Rebar}
{$DEFINE HOOK_ToolBar}
{.$DEFINE HOOK_Menu}


//Undocumented
{$DEFINE HOOK_CommandModule}
{$DEFINE HOOK_SearchBox}
{$DEFINE HOOK_AddressBand}



uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.Classes,
  System.UITypes,
  System.Generics.Collections,
  WinApi.Windows,
  Winapi.UxTheme,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.GraphUtil,
  Vcl.Themes,
  Vcl.Styles.Hooks,
  Vcl.Styles.Utils.Graphics,
  Vcl.Styles.Utils.SysControls;

type
 TDrawThemeBackground  = function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; Foo: Pointer): HRESULT; stdcall;


{$IFDEF HOOK_ProgressBar}
const
  VSCLASS_PROGRESS_INDERTERMINATE        = 'Indeterminate::Progress';
{$ENDIF}

{$IFDEF HOOK_ListView}
const
  VSCLASS_ITEMSVIEW_LISTVIEW             = 'ItemsView::ListView';
  VSCLASS_ITEMSVIEW_HEADER               = 'ItemsView::Header';
  VSCLASS_EXPLORER_LISTVIEW              = 'Explorer::ListView';
  VSCLASS_ITEMSVIEW                      = 'ItemsView';
{$ENDIF}

{$IFDEF HOOK_CommandModule}
const
  VSCLASS_COMMANDMODULE                  = 'CommandModule';
{$ENDIF}

{$IFDEF HOOK_SearchBox}
const
  VSCLASS_SEARCHBOX                     = 'SearchBoxCompositedSearchBox::SearchBox';
{$ENDIF}

{$IFDEF HOOK_AddressBand}
const
  VSCLASS_ADDRESSBAND                   = 'AddressBand';
{$ENDIF}


const
  VSCLASS_PREVIEWPANE                  = 'PreviewPane';

{$IFDEF HOOK_TreeView}
const
  VSCLASS_PROPERTREE                   = 'PROPERTREE';
{$ENDIF}


var
  TrampolineOpenThemeData         : function(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall =  nil;
  TrampolineOpenThemeDataEx       : function(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME; stdcall = nil;

  TrampolineCloseThemeData        : function(hTheme: HTHEME): HRESULT; stdcall =  nil;
  TrampolineDrawThemeBackground   : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pClipRect: Pointer): HRESULT; stdcall =  nil;
  TrampolineDrawThemeBackgroundEx : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pRect: TRect; pOptions: Pointer): HResult; stdcall =  nil;
  TrampolineGetThemeColor         : function(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT; stdcall = nil;
  TrampolineGetThemeSysColor      : function(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall =  nil;
  TrampolineGetThemeSysColorBrush : function(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall =  nil;
  TrampolineDrawThemeText         : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall = nil;
  TrampolineDrawThemeTextEx       : function(hTheme: HTHEME; hdc: HDC; iPartId: Integer; iStateId: Integer; pszText: LPCWSTR; cchText: Integer; dwTextFlags: DWORD; pRect: PRect; var pOptions: TDTTOpts): HResult; stdcall = nil;
  TrampolineDrawThemeEdge         : function(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge, uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall = nil;


  THThemesClasses  : TDictionary<HTHEME, string>;
  THThemesHWND     : TDictionary<HTHEME, HWND>;
  VCLStylesLock    : TCriticalSection = nil;


function Detour_UxTheme_OpenThemeData(hwnd: HWND; pszClassList: LPCWSTR): HTHEME; stdcall;
begin
  VCLStylesLock.Enter;
  try
    Result:=TrampolineOpenThemeData(hwnd, pszClassList);
    if THThemesClasses.ContainsKey(Result) then
      THThemesClasses.Remove(Result);
    THThemesClasses.Add(Result, pszClassList);

    if THThemesHWND.ContainsKey(Result) then
      THThemesHWND.Remove(Result);
    THThemesHWND.Add(Result, hwnd);
  finally
    VCLStylesLock.Leave;
  end;
  //OutputDebugString(PChar('Detour_UxTheme_OpenThemeData '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8)));
  //OutputDebugString2('Detour_UxTheme_OpenThemeData '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8));
end;

function Detour_UxTheme_OpenThemeDataEx(hwnd: HWND; pszClassList: LPCWSTR; dwFlags: DWORD): HTHEME; stdcall;
begin
  VCLStylesLock.Enter;
  try
    Result:=TrampolineOpenThemeDataEx(hwnd, pszClassList, dwFlags);
    if THThemesClasses.ContainsKey(Result) then
      THThemesClasses.Remove(Result);
    THThemesClasses.Add(Result, pszClassList);

    if THThemesHWND.ContainsKey(Result) then
      THThemesHWND.Remove(Result);
    THThemesHWND.Add(Result, hwnd);
  finally
    VCLStylesLock.Leave;
  end;
  //OutputDebugString(PChar('Detour_UxTheme_OpenThemeDataEx '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8)));
  //OutputDebugString2('Detour_UxTheme_OpenThemeDataEx '+pszClassList+' hTheme '+IntToStr(Result)+' Handle '+IntToHex(hwnd, 8));
end;

procedure DrawParentBackground(Handle : THandle; DC: HDC; const ARect: TRect);
var
  LBuffer: TBitmap;
  LPoint: TPoint;
  ParentHandle : THandle;
begin
  if Handle=0 then exit;
  LPoint := Point(ARect.Left, ARect.Top);
  LBuffer := TBitmap.Create;
  try
    ParentHandle:=GetParent(Handle);
    if ParentHandle<>0 then
    begin
      LBuffer.SetSize(ARect.Width, ARect.Height);
      SendMessage(ParentHandle , WM_ERASEBKGND, LBuffer.Canvas.Handle, 0);
      ClientToScreen(Handle, LPoint);
      ScreenToClient(ParentHandle, LPoint);
      //BitBlt(DC, ARect.Left, ARect.Top, ARect.Width, ARect.Height, Bmp.Canvas.Handle, P.X, P.Y, SRCCOPY)
    end;
  finally
    LBuffer.Free;
  end;
end;

function AreBitmapsEqual(Bitmap1, Bitmap2: TBitmap): Boolean;
var
 i, ScanBytes : Integer;
begin
  Result:= (Bitmap1<>nil) and (Bitmap2<>nil);
  if not Result then exit;
  Result:=(bitmap1.Width=bitmap2.Width) and (bitmap1.Height=bitmap2.Height) and (bitmap1.PixelFormat=bitmap2.PixelFormat);

  if not Result then exit;

  ScanBytes := Abs(Integer(Bitmap1.Scanline[1]) - Integer(Bitmap1.Scanline[0]));
  for i:=0 to Bitmap1.Height-1 do
  Begin
    Result:=CompareMem(Bitmap1.ScanLine[i], Bitmap2.ScanLine[i], ScanBytes);
    if not Result then exit;
  End;

end;

function _GetThemeClass(hTheme: HTHEME ; iPartId, iStateId: Integer) : string;
var
  LBuffer, LBufferNew  : TBitmap;
  hThemeNew : WinApi.UxTheme.HTHEME;
  LRect   : TRect;
begin
  Result:='';

  //Check for VSCLASS_SCROLLBAR
  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_SCROLLBAR);
  if hThemeNew=hTheme then
   Exit(VSCLASS_SCROLLBAR)
  else
  try
    LBuffer:=TBitmap.Create;
    LBufferNew:=TBitmap.Create;
    try
      LBuffer.SetSize(25, 25);
      lRect:=Rect(0, 0 , LBuffer.Width, LBuffer.Height);
      TrampolineDrawThemeBackground(hTheme, LBuffer.Canvas.Handle, SBP_ARROWBTN, ABS_UPHOT, LRect, nil);

      LBufferNew.SetSize(25, 25);
        TrampolineDrawThemeBackground(hThemeNew, LBufferNew.Canvas.Handle, SBP_ARROWBTN, ABS_UPHOT, LRect, nil);

        if AreBitmapsEqual(LBuffer, LBufferNew) then
          Exit(VSCLASS_SCROLLBAR);
    finally
      LBuffer.Free;
      LBufferNew.Free;
    end;
  finally
     CloseThemeData(hThemeNew);
  end;
end;

function GetThemeClass(hTheme: HTHEME ; iPartId, iStateId: Integer) : string;
var
  hThemeNew : WinApi.UxTheme.HTHEME;
begin
  Result:='';
  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_SCROLLBAR);
  if hThemeNew=hTheme then
    Exit(VSCLASS_SCROLLBAR)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_LISTVIEW);
  if hThemeNew=hTheme then
    Exit(VSCLASS_LISTVIEW)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_EDIT);
  if hThemeNew=hTheme then
    Exit(VSCLASS_EDIT)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_LISTBOX);
  if hThemeNew=hTheme then
    Exit(VSCLASS_LISTBOX)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_BUTTON);
  if hThemeNew=hTheme then
    Exit(VSCLASS_BUTTON)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_MENU);
  if hThemeNew=hTheme then
    Exit(VSCLASS_MENU)
  else
    CloseThemeData(hThemeNew);

  hThemeNew := TrampolineOpenThemeData(0, VSCLASS_MENUBAND);
  if hThemeNew=hTheme then
    Exit(VSCLASS_MENUBAND)
  else
    CloseThemeData(hThemeNew);
end;




function Detour_UxTheme_DrawThemeMain(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; Foo: Pointer; Trampoline : TDrawThemeBackground): HRESULT; stdcall;
var
  LBuffer   : TBitmap;
  SaveIndex : integer;
  LDetails  : TThemedElementDetails;
  LScrollDetails: TThemedScrollBar;
  LRect     : TRect;
  LSize     : TSize;
  LColor, LStartColor, LEndColor  : TColor;
  LCanvas   : TCanvas;
  LThemeClass : string;
  LThemeClasses : TStrings;
  LHWND       : HWND;
begin
 LThemeClasses:=TStringList.Create;
 try
  VCLStylesLock.Enter;
  try

    if not THThemesClasses.ContainsKey(hTheme)  then
    begin
      LThemeClass:=GetThemeClass(hTheme, iPartId, iStateId);
      if LThemeClass<>'' then
      begin
       THThemesClasses.Add(hTheme, LThemeClass);
       THThemesHWND.Add(hTheme, 0);
      end;
    end
    else
    LThemeClass := THThemesClasses.Items[hTheme];

//    if (LThemeClass<>'')  then
//    begin
//      //if SameText(LThemeClass, VSCLASS_MENU) then
//        OutputDebugString2(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId]))
//    end
//    else
//       OutputDebugString2(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId]));

    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or not THThemesClasses.ContainsKey(hTheme) then
      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));


//
//    if (LThemeClass<>'')  then
//    begin
//        OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
//    end
//    else
//      OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
//

//    DrawStyleFillRect(hdc, pRect, clRed);
//    Exit(S_OK);


    ExtractStrings([';'], [], PChar(LThemeClass), LThemeClasses);
    LHWND := THThemesHWND.Items[hTheme];
  finally
    VCLStylesLock.Leave;
  end;


   if SameText(LThemeClass, VSCLASS_PREVIEWPANE) then
   begin
             case iPartId of
               1:
                                       begin
                                          case iStateId of
                                           //background
                                           1 :
                                               begin
                                                 //StyleServices.GetElementColor(=StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
                                                 //DrawStyleFillRect(hdc, pRect, LColor);

                                                 //DrawStyleFillRect(hdc, pRect, clRed);

                                                 //DrawStyleElement(hdc, StyleServices.GetElementDetails(tpPanelBackground), pRect);
                                                 DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
                                                 Exit(S_OK);
                                               end;
                                          end;
                                       end;

               3:
                                       begin
                                          case iStateId of
                                           //left border of listview
                                           0 :
                                               begin
                                                 //DrawStyleFillRect(hdc, pRect, clBlue);
                                                 StyleServices.GetElementColor(StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
                                                 DrawStyleFillRect(hdc, pRect, LColor);
                                                 //DrawStyleElement(hdc, StyleServices.GetElementDetails(tpPanelBackground), pRect);
                                                 Exit(S_OK);
                                               end;
                                          end;
                                       end;


               4:
                                       begin
                                          case iStateId of
                                           //left border of preview pane
                                           0 :
                                               begin
                                                 //DrawStyleFillRect(hdc, pRect, clGreen);
                                                 StyleServices.GetElementColor(StyleServices.GetElementDetails(tpPanelBackground), ecFillColor, LColor);
                                                 DrawStyleFillRect(hdc, pRect, LColor);
                                                 //DrawStyleElement(hdc, StyleServices.GetElementDetails(tpPanelBackground), pRect);
                                                 Exit(S_OK);
                                               end;
                                          end;
                                       end;
             end;

    OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
    Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));

   end
   else
   {$IFDEF HOOK_ToolBar}
   if SameText(LThemeClass, VSCLASS_TOOLBAR) then
   begin
             case iPartId of
               TP_BUTTON :
                                       begin
                                          case iStateId of

                                           TS_NORMAL :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNormal), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_HOT :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonHot), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_PRESSED :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonPressed), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_NEARHOT :
                                                       begin
                                                         //DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonNearHot), pRect);
                                                         //DrawStyleFillRect(hdc, pRect, clRed);
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbButtonHot), pRect);
                                                         Exit(S_OK);
                                                       end;
                                          end;
                                       end;

               TP_SPLITBUTTON :
                                       begin
                                          case iStateId of

                                           TS_NORMAL :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonNormal), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_HOT :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_PRESSED :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonPressed), pRect);
                                                         Exit(S_OK);
                                                       end;


                                           TS_NEARHOT :
                                                       begin
                                                         //DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonNearHot), pRect);
                                                         //DrawStyleFillRect(hdc, pRect, clRed);
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_OTHERSIDEHOT :
                                                       begin
                                                         //DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonOtherSideHot), pRect);
                                                         //DrawStyleFillRect(hdc, pRect, clBlue);
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonHot), pRect);
                                                         Exit(S_OK);
                                                       end;

                                          end;
                                       end;
               TP_SPLITBUTTONDROPDOWN :
                                       begin
                                          case iStateId of

                                           TS_NORMAL :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_HOT :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownHot), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_PRESSED :
                                                       begin
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownPressed), pRect);
                                                         Exit(S_OK);
                                                       end;

                                           TS_OTHERSIDEHOT :
                                                       begin
                                                         //DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownOtherSideHot), pRect);
                                                         DrawStyleElement(hdc, StyleServices.GetElementDetails(ttbSplitButtonDropDownHot), pRect);
                                                         Exit(S_OK);
                                                       end;
                                          end;
                                       end;



             end;
      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_AddressBand}
   if SameText(LThemeClass, VSCLASS_ADDRESSBAND) then
   begin
             case iPartId of
                  //address bar control
                    1 :
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollNormal), pRect);
                                       Exit(S_OK);
                                   end;
                              //hot
                              2:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollHot), pRect);
                                       Exit(S_OK);
                                   end;
                              //editing
                              4:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollFocused), pRect);
                                       Exit(S_OK);
                                   end;

                             end;

                          end;
             end;

      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_SearchBox}
   if SameText(LThemeClass, VSCLASS_SEARCHBOX) then  //OK
   begin
             case iPartId of
                  //searchbox control
                    1 :
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollNormal), pRect);
                                       Exit(S_OK);
                                   end;
                              //hot
                              2:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollHot), pRect);
                                       Exit(S_OK);
                                   end;
                              //editing
                              4:
                                   begin
                                       DrawStyleElement(hdc, StyleServices.GetElementDetails(teEditBorderNoScrollFocused), pRect);
                                       Exit(S_OK);
                                   end;

                             end;

                          end;
             end;

      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_CommandModule}
   if  SameText(LThemeClass, VSCLASS_COMMANDMODULE) then
   begin

      case iPartId of
                      //Top Bar
                      1 :
                          begin
                             case iStateId of
                              0:
                                   begin
                                        DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
                                        Exit(S_OK);
                                   end;

                             end;

                          end;

                      //Buttons background
                      3 :
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                      DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
                                      Exit(S_OK);
                                   end;
                              //Hot
                              2:
                                   begin

                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      Exit(S_OK);
                                   end;

                              //pressed
                              3:
                                   begin
                                      //awStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      Exit(S_OK);
                                   end;

                              //focused
                              4:
                                   begin
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
                                      Exit(S_OK);
                                   end;
                             end;

                          end;

                      //button with dropdown
                      4 :
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                      DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
                                      Exit(S_OK);
                                   end;

                              //hot
                              2:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //awStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      Exit(S_OK);
                                   end;

                              //pressed
                              3:
                                   begin
                                     LColor:=StyleServices.GetSystemColor(clHighlight);
                                     AlphaBlendFillCanvas(hdc, LColor, pRect, 96);

                                   //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
                                      Exit(S_OK);
                                   end;

                              //focused
                              4:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
                                      Exit(S_OK);
                                   end;


                              5: // hot arrow button
                                   begin
                                     LColor:=StyleServices.GetSystemColor(clHighlight);
                                     AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                    //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      Exit(S_OK);
                                   end;
                             end;


                          end;

                      //narrow button with dropdown - background
                      5 :
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                      DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
                                      Exit(S_OK);
                                   end;
                              //hot on arrow
                              2:
                                   begin
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      Exit(S_OK);
                                   end;

                              //pressed arrow (button down)
                              3:
                                   begin
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      Exit(S_OK);
                                   end;

                              //selected
                              4:
                                   begin
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
                                      Exit(S_OK);
                                   end;

                              //hot on button
                              5:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      Exit(S_OK);
                                   end;

                             end;

                          end;

                      //dropdown arrow
                      6 :
                          begin
                             case iStateId of
                              0:
                                   begin
                                      LRect:=pRect;
                                      LRect.Left  := LRect.Left  + 5;
                                      LRect.Width := LRect.Width + 5;
                                      DrawStyleDownArrow(hdc, LRect, StyleServices.GetSystemColor(clHighlight));
                                      Exit(S_OK);
                                   end;

                              //down arrow normal
                              1:
                                   begin
                                      LRect:=pRect;
                                      LRect.Left  := LRect.Left  + 2;
                                      LRect.Width := LRect.Width + 2;
                                      DrawStyleDownArrow(hdc, LRect, StyleServices.GetSystemColor(clHighlight));
                                      Exit(S_OK);
                                   end;
                             end;

                          end;


                      8 ://arrow button - Top Bar of listview
                          begin
                             case iStateId of
                              //normal
                              1:
                                   begin
                                      LRect:=pRect;
                                      LRect.Left  := LRect.Left  + 5;
                                      LRect.Width := LRect.Width + 5;
                                      DrawStyleDownArrow(hdc, LRect, StyleServices.GetSystemColor(clHighlight));
                                      Exit(S_OK);
                                   end;
                              //hot
                              2:
                                   begin
                                      LRect:=pRect;
                                      LRect.Left  := LRect.Left  + 5;
                                      LRect.Width := LRect.Width + 5;
                                      DrawStyleDownArrow(hdc, LRect, StyleServices.GetSystemColor(clHighlight));
                                      Exit(S_OK);
                                   end;
                             end;

                          end;

                      9 : //button -Top Bar of listview
                          begin

                             case iStateId of
                              //normal
                              1:
                                   begin
                                      DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonNormal), pRect);
                                      Exit(S_OK);
                                   end;

                              //hot
                              2:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonHot), pRect);
                                      Exit(S_OK);
                                   end;

                              3: //pressed arrow (button down)
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
                                      Exit(S_OK);
                                   end;

                              //selected
                              4:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 50);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonPressed), pRect);
                                      Exit(S_OK);
                                   end;


                              //hot on button
                              5:
                                   begin
                                      LColor:=StyleServices.GetSystemColor(clHighlight);
                                      AlphaBlendFillCanvas(hdc, LColor, pRect, 96);
                                      //DrawStyleElement(hdc, StyleServices.GetElementDetails(tbPushButtonDefaulted), pRect);
                                      Exit(S_OK);
                                   end;
                             end;

                          end;


                      //Top Bar of listview - background  solid color
                      11 :
                          begin
                             case iStateId of
                              0:
                                   begin
                                      LDetails:=StyleServices.GetElementDetails(tpPanelBackground);
                                      StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
                                      DrawStyleFillRect(hdc, pRect, LColor);
                                      Exit(S_OK);
                                   end;
                             end;

                          end;

                      //Top Bar of listview - backgroundimage
                      12 :
                          begin
                             case iStateId of
                              0:
                                   begin
                                      LDetails:=StyleServices.GetElementDetails(tpPanelBackground);
                                      StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
                                      DrawStyleFillRect(hdc, pRect, LColor);
                                      Exit(S_OK);
                                   end;
                             end;


                          end;

      end;

      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Menu}
   if  SameText(LThemeClass, VSCLASS_MENU) then
   begin
      case iPartId of

         MENU_POPUPBORDERS   :
                                 begin
                                    DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBorders), pRect);
                                    Exit(S_OK);
                                 end;

         MENU_POPUPITEM      :
                                 begin
                                    case iStateId of
                                      MPI_NORMAL         : LDetails:=StyleServices.GetElementDetails(tmPopupItemNormal);
                                      MPI_HOT            : LDetails:=StyleServices.GetElementDetails(tmPopupItemHot);
                                      //MBI_PUSHED         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemPushed);
                                      MPI_DISABLED       : LDetails:=StyleServices.GetElementDetails(tmPopupItemDisabled);
                                      MPI_DISABLEDHOT    : LDetails:=StyleServices.GetElementDetails(tmPopupItemHot);
                                      //MPI_DISABLEDPUSHED : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                                    end;

                                    DrawStyleElement(hdc, LDetails, pRect);
                                    Exit(S_OK);
                                 end;

         MENU_BARBACKGROUND :
                                 begin
                                    case iStateId of
                                      MB_ACTIVE      :LDetails:=StyleServices.GetElementDetails(tmMenuBarBackgroundActive);
                                      MB_INACTIVE    :LDetails:=StyleServices.GetElementDetails(tmMenuBarBackgroundInactive);
                                    else
                                      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                                    end;

                                    DrawStyleElement(hdc, LDetails, pRect);
                                    Exit(S_OK);
                                 end;

         MENU_BARITEM      :
                                 begin
                                    case iStateId of
                                      MBI_NORMAL         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemNormal);
                                      MBI_HOT            : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemHot);
                                      MBI_PUSHED         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemPushed);
                                      MBI_DISABLED       : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabled);
                                      MBI_DISABLEDHOT    : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledHot);
                                      MBI_DISABLEDPUSHED : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                                    end;

                                    DrawStyleElement(hdc, LDetails, pRect);
                                    Exit(S_OK);
                                 end;


        MENU_POPUPSEPARATOR   :

                                 begin
                                   DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupSeparator), pRect);
                                   //DrawStyleFillRect(hdc, pRect, clRed);
                                   Exit(S_OK);
                                 end;

         MENU_POPUPGUTTER       :
                                 begin
                                    DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupGutter), pRect);
                                    Exit(S_OK);
                                 end;

         MENU_POPUPBACKGROUND :
                                 begin
                                    DrawStyleElement(hdc, StyleServices.GetElementDetails(tmPopupBackground), pRect);
                                    Exit(S_OK);
                                 end;
       else
       begin
          //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));
          Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
      end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Rebar}
   if  SameText(LThemeClass, VSCLASS_REBAR) then
   begin
      case iPartId of
        RP_BAND       :
                        begin
                                DrawStyleElement(hdc, StyleServices.GetElementDetails(trBand), pRect);
                                Exit(S_OK);
                        end;

        RP_BACKGROUND :
                        begin
                                DrawStyleElement(hdc, StyleServices.GetElementDetails(trBackground), pRect);
                                Exit(S_OK);
                        end
      else
          begin
            //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
            Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
          end;
      end;

   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Edit}
   if  SameText(LThemeClass, VSCLASS_EDIT) then
   begin
      case iPartId of

         EP_BACKGROUNDWITHBORDER,
         EP_EDITBORDER_NOSCROLL :
                                 begin
                                    case iStateId of
                                      EPSN_NORMAL   :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
                                      EPSN_HOT      :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollHot);
                                      EPSN_FOCUSED  :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
                                      EPSN_DISABLED :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
                                    end;

                                   DrawStyleElement(hdc, LDetails, pRect);
                                   Exit(S_OK);
                                 end;

         EP_EDITBORDER_HSCROLL :
                                 begin
                                    case iStateId of
                                      EPSH_NORMAL   :LDetails:=StyleServices.GetElementDetails(teEditBorderHScrollNormal);
                                      EPSH_HOT      :LDetails:=StyleServices.GetElementDetails(teEditBorderHScrollHot);
                                      EPSH_FOCUSED  :LDetails:=StyleServices.GetElementDetails(teEditBorderHScrollFocused);
                                      EPSH_DISABLED :LDetails:=StyleServices.GetElementDetails(teEditBorderHScrollDisabled);
                                    end;

                                   DrawStyleElement(hdc, LDetails, pRect);
                                   Exit(S_OK);
                                 end;

         EP_EDITBORDER_VSCROLL :
                                 begin
                                    case iStateId of
                                      EPSV_NORMAL   :LDetails:=StyleServices.GetElementDetails(teEditBorderVScrollNormal);
                                      EPSV_HOT      :LDetails:=StyleServices.GetElementDetails(teEditBorderVScrollHot);
                                      EPSV_FOCUSED  :LDetails:=StyleServices.GetElementDetails(teEditBorderVScrollFocused);
                                      EPSV_DISABLED :LDetails:=StyleServices.GetElementDetails(teEditBorderVScrollDisabled);
                                    end;

                                   DrawStyleElement(hdc, LDetails, pRect);
                                   Exit(S_OK);
                                 end;

         EP_EDITBORDER_HVSCROLL :
                                 begin
                                    case iStateId of
                                      EPSHV_NORMAL   :LDetails:=StyleServices.GetElementDetails(teEditBorderHVScrollNormal);
                                      EPSHV_HOT      :LDetails:=StyleServices.GetElementDetails(teEditBorderHVScrollHot);
                                      EPSHV_FOCUSED  :LDetails:=StyleServices.GetElementDetails(teEditBorderHVScrollFocused);
                                      EPSHV_DISABLED :LDetails:=StyleServices.GetElementDetails(teEditBorderHVScrollDisabled);
                                    end;


                                   DrawStyleElement(hdc, LDetails, pRect);
                                   Exit(S_OK);
                                 end

       else
       begin
          //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
          Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
      end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_ListBox}
   if SameText(LThemeClass, VSCLASS_LISTBOX) then
   begin
      case iPartId of
       LBCP_BORDER_NOSCROLL :
                              begin
                                  case iStateId of
                                    LBPSN_NORMAL   : LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
                                    LBPSN_FOCUSED  : LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
                                    LBPSN_HOT      : LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollHot);
                                    LBPSN_DISABLED : LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
                                  end;

                                  DrawStyleElement(hdc, LDetails, pRect);
                                  Exit(S_OK);
                              end;
      else
        begin
          //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
          Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
        end;
      end;

   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Spin}
   if SameText(LThemeClass, VSCLASS_SPIN) then
   begin
     case iPartId of
        SPNP_UP :
                        begin
                          case iStateId of
                            UPS_NORMAL   :LDetails:=StyleServices.GetElementDetails(tsUpNormal);
                            UPS_HOT      :LDetails:=StyleServices.GetElementDetails(tsUpHot);
                            UPS_PRESSED  :LDetails:=StyleServices.GetElementDetails(tsUpPressed);
                            UPS_DISABLED :LDetails:=StyleServices.GetElementDetails(tsUpDisabled);
                          end;

                          SaveIndex := SaveDC(hdc);
                          try
                           if LHWND<>0  then
                             DrawParentBackground(LHWND, hdc, pRect);

                           StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                          finally
                            RestoreDC(hdc, SaveIndex);
                          end;
                          Result:=S_OK;
                        end;

        SPNP_DOWN :
                        begin
                          case iStateId of
                            DNS_NORMAL   :LDetails:=StyleServices.GetElementDetails(tsDownNormal);
                            DNS_HOT      :LDetails:=StyleServices.GetElementDetails(tsDownHot);
                            DNS_PRESSED  :LDetails:=StyleServices.GetElementDetails(tsDownPressed);
                            DNS_DISABLED :LDetails:=StyleServices.GetElementDetails(tsDownDisabled);
                          end;

                          SaveIndex := SaveDC(hdc);
                          try
                           if LHWND<>0  then
                             DrawParentBackground(LHWND, hdc, pRect);

                           StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                          finally
                            RestoreDC(hdc, SaveIndex);
                          end;
                          Result:=S_OK;
                        end;
     else
       begin
         //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
         Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
     end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_ComboBox}
   if  SameText(LThemeClass, VSCLASS_COMBOBOX) then
   begin

     case iPartId of
        CP_BORDER              :
                                  begin
                                    case iStateId of
                                      CBB_NORMAL   : LDetails:=StyleServices.GetElementDetails(tcBorderNormal);
                                      CBB_HOT      : LDetails:=StyleServices.GetElementDetails(tcBorderHot);
                                      CBB_FOCUSED  : LDetails:=StyleServices.GetElementDetails(tcBorderFocused);
                                      CBB_DISABLED : LDetails:=StyleServices.GetElementDetails(tcBorderDisabled);
                                    end;

                                    SaveIndex := SaveDC(hdc);
                                    try
                                     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                    finally
                                      RestoreDC(hdc, SaveIndex);
                                    end;
                                    Result:=S_OK;
                                  end;

        CP_DROPDOWNBUTTONRIGHT :
                                  begin
                                    case iStateId of
                                      CBXSR_NORMAL   :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonNormal);
                                      CBXSR_HOT      :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonHot);
                                      CBXSR_PRESSED  :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonPressed);
                                      CBXSR_DISABLED :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonDisabled);
                                    end;

                                    SaveIndex := SaveDC(hdc);
                                    try
                                     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                    finally
                                      RestoreDC(hdc, SaveIndex);
                                    end;
                                    Result:=S_OK;
                                  end;


     else
       begin
         //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
         Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
     end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_ListView}
   if (SameText(LThemeClass, VSCLASS_HEADER) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_HEADER)) then
   begin
        case iPartId of
          0                :
                              begin
                                  DrawStyleElement(hdc, StyleServices.GetElementDetails(tcpThemedHeader), pRect);
                                  Exit(S_OK);
                              end;


          HP_HEADERITEM    :
                              begin
                                  case iStateId of
                                      HIS_NORMAL   : LDetails:=StyleServices.GetElementDetails(thHeaderItemNormal);
                                      HIS_HOT      : LDetails:=StyleServices.GetElementDetails(thHeaderItemHot);
                                      HIS_PRESSED  : LDetails:=StyleServices.GetElementDetails(thHeaderItemPressed);
                                      
                                      HIS_SORTEDNORMAL   : LDetails:=StyleServices.GetElementDetails(thHeaderItemNormal);
                                      HIS_SORTEDHOT      : LDetails:=StyleServices.GetElementDetails(thHeaderItemHot);
                                      HIS_SORTEDPRESSED  : LDetails:=StyleServices.GetElementDetails(thHeaderItemPressed);

                                      HIS_ICONNORMAL   : LDetails:=StyleServices.GetElementDetails(thHeaderItemNormal);
                                      HIS_ICONHOT      : LDetails:=StyleServices.GetElementDetails(thHeaderItemHot);
                                      HIS_ICONPRESSED  : LDetails:=StyleServices.GetElementDetails(thHeaderItemPressed);
                                      
                                      HIS_ICONSORTEDNORMAL   : LDetails:=StyleServices.GetElementDetails(thHeaderItemNormal);
                                      HIS_ICONSORTEDHOT      : LDetails:=StyleServices.GetElementDetails(thHeaderItemHot);
                                      HIS_ICONSORTEDPRESSED  : LDetails:=StyleServices.GetElementDetails(thHeaderItemPressed);
                                  else
                                      LDetails:=StyleServices.GetElementDetails(thHeaderItemNormal);
                                  end;

                                  SaveIndex := SaveDC(hdc);
                                  try
                                     if LHWND<>0  then
                                       DrawParentBackground(LHWND, hdc, pRect);
                                     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                  finally
                                    RestoreDC(hdc, SaveIndex);
                                  end;

                                  Exit(S_OK);
                              end;   

          HP_HEADERSORTARROW :
                              begin
//                                  case iStateId of
//                                      HSAS_SORTEDUP    : LDetails:=StyleServices.GetElementDetails(thHeaderSortArrowSortedUp);
//                                      HSAS_SORTEDDOWN  : LDetails:=StyleServices.GetElementDetails(thHeaderSortArrowSortedDown);
//                                  end;

                                  SaveIndex := SaveDC(hdc);
                                  LCanvas:=TCanvas.Create;
                                  try
                                    LCanvas.Handle:=hdc;
                                    LStartColor:= StyleServices.GetSystemColor(clHighlight);

                                    LCanvas.Pen.Color:=LStartColor;
                                    LCanvas.Brush.Style:=bsClear;

                                    LRect:= pRect;
                                    LRect.Top :=  LRect.Top + 3;

                                    if (iStateId=HSAS_SORTEDUP) then
                                      DrawArrow(LCanvas, TScrollDirection.sdUp, LRect.Location, 3)
                                    else
                                      DrawArrow(LCanvas, TScrollDirection.sdDown, LRect.Location, 3);
                                  finally
                                    LCanvas.Handle:=0;
                                    LCanvas.Free;
                                    RestoreDC(hdc, SaveIndex);
                                  end;

                                  Exit(S_OK);
                              end;

         HP_HEADERDROPDOWN   :
                              begin
                                  case iStateId of
                                      HDDS_NORMAL   : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal); //tcDropDownButtonNormal, thHeaderDropDownNormal
                                      HDDS_SOFTHOT  : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownHot); //tcDropDownButtonHot, thHeaderDropDownSoftHot
                                      HDDS_HOT      : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);  //tcDropDownButtonHot, thHeaderDropDownHot
                                  end;

                                  DrawStyleElement(hdc, LDetails, pRect);
                                  Exit(S_OK);
                              end;

         HP_HEADERDROPDOWNFILTER   :
                              begin
                                  case iStateId of
                                      HDDFS_NORMAL   : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownNormal); //tcDropDownButtonNormal, thHeaderDropDownNormal
                                      HDDFS_SOFTHOT  : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownHot); //tcDropDownButtonHot, thHeaderDropDownSoftHot
                                      HDDFS_HOT      : LDetails:=StyleServices.GetElementDetails(ttbSplitButtonDropDownHot);  //tcDropDownButtonHot, thHeaderDropDownHot
                                  end;

                                  DrawStyleElement(hdc, LDetails, pRect);
                                  Exit(S_OK);
                              end;

        else
           begin
             //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
             Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
             //DrawStyleFillRect(hdc, pRect, clRed);
             //Exit(S_OK);
           end;
        end;

   end
   else
   if  (SameText(LThemeClass, VSCLASS_ITEMSVIEW) or SameText(LThemeClass, VSCLASS_LISTVIEW) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) or SameText(LThemeClass, VSCLASS_EXPLORER_LISTVIEW)) then
   begin
        case iPartId of
          LVP_LISTITEM       :
                              begin
                                  case iStateId of
                                      LIS_HOT,
                                      LISS_HOTSELECTED,
                                      LIS_SELECTEDNOTFOCUS,
                                      LIS_SELECTED          :
                                                              begin
                                                                LColor :=StyleServices.GetSystemColor(clHighlight);
                                                                LCanvas:=TCanvas.Create;
                                                                SaveIndex := SaveDC(hdc);
                                                                try
                                                                  LCanvas.Handle:=hdc;
                                                                  LStartColor:= LCanvas.Pen.Color;
                                                                  LRect:=pRect;


                                                                  if iStateId=LISS_HOTSELECTED then
                                                                    AlphaBlendFillCanvas(LCanvas, LColor, LRect, 96)
                                                                  else
                                                                    AlphaBlendFillCanvas(LCanvas, LColor, LRect, 50);

                                                                  if SameText(LThemeClass, VSCLASS_LISTVIEW) then
                                                                  begin
                                                                    LCanvas.Pen.Color:=LColor;
                                                                    LCanvas.Brush.Style:=bsClear;
                                                                    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
                                                                    LCanvas.Pen.Color:=LStartColor;
                                                                  end;

                                                                finally
                                                                  LCanvas.Handle:=0;
                                                                  LCanvas.Free;
                                                                  RestoreDC(hdc, SaveIndex);
                                                                end;
                                                                Result:=S_OK;
                                                              end;
                                  else
                                  begin
                                      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
                                      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                                  end;
                                  end;
                              end;

          LVP_LISTDETAIL     :
                              begin
                                  case iStateId of

                                      LIS_NORMAL,
                                      LIS_HOT             :
                                                              begin
                                                                LColor :=StyleServices.GetSystemColor(clHighlight);
                                                                LCanvas:=TCanvas.Create;
                                                                SaveIndex := SaveDC(hdc);
                                                                try
                                                                  LCanvas.Handle:=hdc;
                                                                  LStartColor:= LCanvas.Pen.Color;
                                                                  LRect:=pRect;

                                                                  if iStateId=LIS_HOT then
                                                                    AlphaBlendFillCanvas(LCanvas, LColor, LRect, 96)
                                                                  else
                                                                    AlphaBlendFillCanvas(LCanvas, LColor, LRect, 50);

                                                                  if SameText(LThemeClass, VSCLASS_LISTVIEW) then
                                                                  begin
                                                                    LCanvas.Pen.Color:=LColor;
                                                                    LCanvas.Brush.Style:=bsClear;
                                                                    LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
                                                                    LCanvas.Pen.Color:=LStartColor;
                                                                  end;

                                                                finally
                                                                  LCanvas.Handle:=0;
                                                                  LCanvas.Free;
                                                                  RestoreDC(hdc, SaveIndex);
                                                                end;
                                                                Result:=S_OK;
                                                              end;
                                  else
                                  begin
                                      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
                                      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                                  end;

                                  end;

                              end;
          LVP_EXPANDBUTTON   :
                              begin
                                  case iStateId of
                                      LVEB_NORMAL  : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
                                      LVEB_HOVER   : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedHot);
                                      LVEB_PUSHED  : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedPressed);
                                  else
                                      LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
                                  end;

                                  SaveIndex := SaveDC(hdc);
                                  try
                                     if LHWND<>0  then
                                       DrawParentBackground(LHWND, hdc, pRect);
                                     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                  finally
                                    RestoreDC(hdc, SaveIndex);
                                  end;
                                  Exit(S_OK);
                              end;

          LVP_COLLAPSEBUTTON  :
                              begin
                                  case iStateId of
                                      LVCB_NORMAL  : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
                                      LVCB_HOVER   : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedHot);
                                      LVCB_PUSHED  : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedPressed);
                                  else
                                      LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
                                  end;

                                  SaveIndex := SaveDC(hdc);
                                  try
                                     if LHWND<>0  then
                                       DrawParentBackground(LHWND, hdc, pRect);
                                     StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                  finally
                                    RestoreDC(hdc, SaveIndex);
                                  end;
                                  Exit(S_OK);
                              end;
          LVP_GROUPHEADER     :
                               begin
                                  case iStateId of
                                   LVGH_OPENMIXEDSELECTIONHOT,
                                   LVGH_OPENSELECTED,
                                   LVGH_OPENSELECTEDNOTFOCUSEDHOT,
                                   LVGH_OPENSELECTEDHOT,
                                   LVGH_CLOSEHOT,
                                   LVGH_CLOSESELECTEDHOT,
                                   LVGH_CLOSESELECTEDNOTFOCUSEDHOT,
                                   LVGHL_CLOSESELECTED,
                                   LVGH_CLOSESELECTEDNOTFOCUSED,
                                   LVGH_CLOSEMIXEDSELECTION,
                                   LVGH_CLOSEMIXEDSELECTIONHOT,
                                   LVGH_OPENHOT         :
                                                         begin
                                                            LColor :=StyleServices.GetSystemColor(clHighlight);
                                                            LCanvas:=TCanvas.Create;
                                                            SaveIndex := SaveDC(hdc);
                                                            try
                                                              LCanvas.Handle:=hdc;
                                                              AlphaBlendFillCanvas(LCanvas, LColor, pRect, 96);
                                                              LCanvas.Pen.Color:=LColor;
                                                              LCanvas.Brush.Style:=bsClear;
                                                              LRect:=pRect;
                                                              LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
                                                            finally
                                                              LCanvas.Handle:=0;
                                                              LCanvas.Free;
                                                              RestoreDC(hdc, SaveIndex);
                                                            end;
                                                            Exit(S_OK);
                                                         end;
                                  else
                                   begin
                                     //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
                                     Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                                   end;
                                  end;

                               end;
          LVP_GROUPHEADERLINE :
                               begin

                                  case iStateId of
                                   LVGHL_CLOSEHOT,
                                   LVGHL_OPENHOT,
                                   LVGHL_OPENMIXEDSELECTIONHOT,
                                   LVGHL_OPENSELECTEDNOTFOCUSEDHOT,
                                   LVGHL_CLOSESELECTEDNOTFOCUSEDHOT,
                                   LVGHL_OPENSELECTED,
                                   LVGHL_CLOSESELECTED,
                                   LVGHL_CLOSESELECTEDHOT,
                                   LVGHL_CLOSEMIXEDSELECTION,
                                   LVGHL_CLOSEMIXEDSELECTIONHOT,
                                   LVGHL_OPENSELECTEDHOT :
                                                          begin
                                                              //LColor := StyleServices.GetSystemColor(clHighlight);
                                                              LColor := StyleServices.GetSystemColor(clWindowText);
                                                              LCanvas:=TCanvas.Create;
                                                              SaveIndex := SaveDC(hdc);
                                                              try
                                                                LCanvas.Handle:=hdc;
                                                                LCanvas.Pen.Color:=LColor;
                                                                LCanvas.MoveTo(pRect.Left, pRect.Top);
                                                                LCanvas.LineTo(pRect.Right, pRect.Top);
                                                              finally
                                                                LCanvas.Handle:=0;
                                                                LCanvas.Free;
                                                                RestoreDC(hdc, SaveIndex);
                                                              end;

                                                            Exit(S_OK);
                                                          end;

                                   LVGHL_CLOSE,
                                   LVGHL_OPENSELECTEDNOTFOCUSED,
                                   LVGHL_OPEN,
                                   LVGHL_OPENMIXEDSELECTION :
                                                          begin
                                                              LColor := StyleServices.GetSystemColor(clWindowText);
                                                              LCanvas:=TCanvas.Create;
                                                              SaveIndex := SaveDC(hdc);
                                                              try
                                                                LCanvas.Handle:=hdc;
                                                                LCanvas.Pen.Color:=LColor;
                                                                LCanvas.MoveTo(pRect.Left, pRect.Top);
                                                                LCanvas.LineTo(pRect.Right, pRect.Top);
                                                              finally
                                                                LCanvas.Handle:=0;
                                                                LCanvas.Free;
                                                                RestoreDC(hdc, SaveIndex);
                                                              end;

                                                            Exit(S_OK);
                                                          end;
                                  else
                                   begin
                                     //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
                                     Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                                   end;
                                  end;

                               end;

        else
          begin
            //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
            Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
          end;
        end;

   end
   else
   {$ENDIF}
   {$IFDEF HOOK_DateTimePicker}
   if SameText(LThemeClass, VSCLASS_DATEPICKER) then
   begin
     case iPartId of
        DP_DATEBORDER :
                        begin
                          case iStateId of
                            DPDB_NORMAL   :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
                            DPDB_HOT      :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollHot);
                            DPDB_FOCUSED  :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollFocused);
                            DPDB_DISABLED :LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollDisabled);
                          end;

                          SaveIndex := SaveDC(hdc);
                          try
                           StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                          finally
                            RestoreDC(hdc, SaveIndex);
                          end;
                          Exit(S_OK);
                        end;

        DP_SHOWCALENDARBUTTONRIGHT :
                        begin
                          case iStateId of
                            DPSCBR_NORMAL   :LDetails:=StyleServices.GetElementDetails(tcBorderNormal);
                            DPSCBR_HOT      :LDetails:=StyleServices.GetElementDetails(tcBorderHot);
                            DPSCBR_PRESSED  :LDetails:=StyleServices.GetElementDetails(tcBorderHot);
                            DPSCBR_DISABLED :LDetails:=StyleServices.GetElementDetails(tcBorderDisabled);
                          end;

                          SaveIndex := SaveDC(hdc);
                          try
                           StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                          finally
                            RestoreDC(hdc, SaveIndex);
                          end;

                          case iStateId of
                            DPSCBR_NORMAL   :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonNormal);
                            DPSCBR_HOT      :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonHot);
                            DPSCBR_PRESSED  :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonPressed);
                            DPSCBR_DISABLED :LDetails:=StyleServices.GetElementDetails(tcDropDownButtonDisabled);
                          end;

                          SaveIndex := SaveDC(hdc);
                          try
                           LRect:=pRect;
                           InflateRect(LRect, -1, -1);

                           StyleServices.DrawElement(hdc, LDetails, LRect, nil);
                          finally
                            RestoreDC(hdc, SaveIndex);
                          end;
                          Exit(S_OK);
                        end;
     else
       begin
         //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
         Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
     end;
   end
   else
   if  SameText(LThemeClass, VSCLASS_MONTHCAL) then
   begin
     case iPartId of
       MC_BORDERS        :
                           begin
                              LDetails:=StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
                              SaveIndex := SaveDC(hdc);
                              try
                                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                              finally
                                RestoreDC(hdc, SaveIndex);
                              end;
                              Exit(S_OK);
                           end;

       MC_BACKGROUND,
       MC_GRIDBACKGROUND :
                           begin
                            SaveIndex := SaveDC(hdc);
                            LCanvas := TCanvas.Create;
                            try
                             LCanvas.Handle := hdc;
                             LCanvas.Brush.Color := StyleServices.GetStyleColor(scGrid);
                             LCanvas.FillRect(pRect);

                             //LDetails:=StyleServices.GetElementDetails(teBackgroundNormal);
                             //StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                            finally
                              LCanvas.Handle := 0;
                              LCanvas.Free;
                              RestoreDC(hdc, SaveIndex);
                            end;
                            Exit(S_OK);
                           end;

       MC_COLHEADERSPLITTER  : begin
                                  LStartColor := StyleServices.GetSystemColor(clBtnShadow);
                                  LEndColor   := StyleServices.GetSystemColor(clBtnHighlight);

                                  LCanvas:=TCanvas.Create;
                                  SaveIndex := SaveDC(hdc);
                                  try
                                    LCanvas.Handle:=hdc;
                                    LCanvas.Pen.Color:=LStartColor;
                                    LCanvas.MoveTo(pRect.Left, pRect.Top);
                                    LCanvas.LineTo(pRect.Right, pRect.Top);
                                    LCanvas.Pen.Color:=LEndColor;
                                    LCanvas.MoveTo(pRect.Left, pRect.Top+1);
                                    LCanvas.LineTo(pRect.Right, pRect.Top+1);
                                  finally
                                    LCanvas.Handle:=0;
                                    LCanvas.Free;
                                    RestoreDC(hdc, SaveIndex);
                                  end;

                                  Exit(S_OK);
                               end;

       MC_GRIDCELLBACKGROUND : begin

//                                   case iStateId of
//                                    MCGCB_SELECTED : LDetails:=StyleServices.GetElementDetails(tgCellSelected);
//                                    MCGCB_HOT :LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
//                                    MCGCB_SELECTEDHOT :LDetails:=StyleServices.GetElementDetails(tgCellSelected);
//                                    MCGCB_SELECTEDNOTFOCUSED :LDetails:=StyleServices.GetElementDetails(tgCellSelected);
//                                    MCGCB_TODAY :LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
//                                    else
//                                      Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
//                                   end;
//
//                                    SaveIndex := SaveDC(hdc);
//                                    try
//                                       StyleServices.DrawElement(hdc, LDetails, pRect, nil);
//                                    finally
//                                      RestoreDC(hdc, SaveIndex);
//                                    end;

                                      SaveIndex := SaveDC(hdc);
                                      LCanvas:=TCanvas.Create;
                                      try
                                        LCanvas.Handle:=hdc;
                                        LStartColor:= StyleServices.GetSystemColor(clHighlight);
                                        //GradientRoundedFillCanvas(LCanvas, LStartColor, LEndColor, pRect, gdVertical, 4);
                                        if iStateId=MCGCB_TODAY then
                                         AlphaBlendFillCanvas(LCanvas, LStartColor, pRect, 200)
                                        else
                                         AlphaBlendFillCanvas(LCanvas, LStartColor, pRect, 96);

                                        LCanvas.Pen.Color:=LStartColor;
                                        LCanvas.Brush.Style:=bsClear;
                                        LRect:=pRect;
                                        //LCanvas.RoundRect(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height, 6, 6);
                                        LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
                                      finally
                                        LCanvas.Handle:=0;
                                        LCanvas.Free;
                                        RestoreDC(hdc, SaveIndex);
                                      end;

                                    Exit(S_OK);
                               end;

        MC_NAVNEXT             : begin
                                    case iStateId of
                                      MCNN_NORMAL    :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnRightNormal);
                                      MCNN_HOT       :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnRightHot);
                                      MCNN_PRESSED   :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnRightPressed);
                                      MCNN_DISABLED  :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnRightDisabled);
                                    end;

                                    SaveIndex := SaveDC(hdc);
                                    try
                                       StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                    finally
                                      RestoreDC(hdc, SaveIndex);
                                    end;
                                    Exit(S_OK);
                                 end;

        MC_NAVPREV             : begin
                                    case iStateId of
                                      MCNP_NORMAL :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnLeftNormal);
                                      MCNP_HOT :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnLeftHot);
                                      MCNP_PRESSED :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnLeftPressed);
                                      MCNP_DISABLED :  LDetails:=StyleServices.GetElementDetails(tsArrowBtnLeftDisabled);
                                    end;

                                    SaveIndex := SaveDC(hdc);
                                    try
                                       StyleServices.DrawElement(hdc, LDetails, pRect, nil);
                                    finally
                                      RestoreDC(hdc, SaveIndex);
                                    end;
                                    Exit(S_OK);
                                 end;

     else
       begin
         //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
         Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
       end;
     end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Scrollbar}
   if SameText(LThemeClass, VSCLASS_SCROLLBAR) then
   begin

      LScrollDetails := tsScrollBarRoot;
      LDetails.Element := TThemedElement.teScrollBar;
      LDetails.Part := iPartId;
      LDetails.State := iStateId;
      LDetails := StyleServices.GetElementDetails(TThemedScrollBar.tsThumbBtnHorzNormal);

      case iPartId  of
        SBP_ARROWBTN :
        begin
          case iStateId of
            ABS_UPNORMAL      : LScrollDetails := tsArrowBtnUpNormal;
            ABS_UPHOT         : LScrollDetails := tsArrowBtnUpHot;
            ABS_UPPRESSED     : LScrollDetails := tsArrowBtnUpPressed;
            ABS_UPDISABLED    : LScrollDetails := tsArrowBtnUpDisabled;
            ABS_DOWNNORMAL    : LScrollDetails := tsArrowBtnDownNormal;
            ABS_DOWNHOT       : LScrollDetails := tsArrowBtnDownHot;
            ABS_DOWNPRESSED   : LScrollDetails := tsArrowBtnDownPressed;
            ABS_DOWNDISABLED  : LScrollDetails := tsArrowBtnDownDisabled;
            ABS_LEFTNORMAL    : LScrollDetails := tsArrowBtnLeftNormal;
            ABS_LEFTHOT       : LScrollDetails := tsArrowBtnLeftHot;
            ABS_LEFTPRESSED   : LScrollDetails := tsArrowBtnLeftPressed;
            ABS_LEFTDISABLED  : LScrollDetails := tsArrowBtnLeftDisabled;
            ABS_RIGHTNORMAL   : LScrollDetails := tsArrowBtnRightNormal;
            ABS_RIGHTHOT      : LScrollDetails := tsArrowBtnRightHot;
            ABS_RIGHTPRESSED  : LScrollDetails := tsArrowBtnRightPressed;
            ABS_RIGHTDISABLED : LScrollDetails := tsArrowBtnRightDisabled;
            ABS_UPHOVER       : LScrollDetails := tsArrowBtnUpNormal;//tsArrowBtnUpHover;
            ABS_DOWNHOVER     : LScrollDetails := tsArrowBtnDownNormal;//tsArrowBtnDownHover;
            ABS_LEFTHOVER     : LScrollDetails := tsArrowBtnLeftNormal;//tsArrowBtnLeftHover;
            ABS_RIGHTHOVER    : LScrollDetails := tsArrowBtnRightNormal;//tsArrowBtnRightHover;
          end;
        end;

        SBP_THUMBBTNHORZ:
        begin
          case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsThumbBtnHorzNormal;
           SCRBS_HOT      : LScrollDetails := tsThumbBtnHorzHot;
           SCRBS_PRESSED  : LScrollDetails := tsThumbBtnHorzPressed;
           SCRBS_DISABLED : LScrollDetails := tsThumbBtnHorzDisabled;
           SCRBS_HOVER    : LScrollDetails := tsThumbBtnHorzNormal;
          end;
        end;

        SBP_THUMBBTNVERT:
        begin
          case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsThumbBtnVertNormal;
           SCRBS_HOT      : LScrollDetails := tsThumbBtnVertHot;
           SCRBS_PRESSED  : LScrollDetails := tsThumbBtnVertPressed;
           SCRBS_DISABLED : LScrollDetails := tsThumbBtnVertDisabled;
           SCRBS_HOVER    : LScrollDetails := tsThumbBtnVertNormal;
          end;
        end;

        SBP_LOWERTRACKHORZ:
        begin
          case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsLowerTrackHorzNormal;
           SCRBS_HOT      : LScrollDetails := tsLowerTrackHorzHot;
           SCRBS_PRESSED  : LScrollDetails := tsLowerTrackHorzPressed;
           SCRBS_DISABLED : LScrollDetails := tsLowerTrackHorzDisabled;
           SCRBS_HOVER    : LScrollDetails := tsLowerTrackHorzNormal;//tsLowerTrackHorzHover; //no support for hover
          end;
        end;

        SBP_UPPERTRACKHORZ :
        begin
         case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsUpperTrackHorzNormal;
           SCRBS_HOT      : LScrollDetails := tsUpperTrackHorzHot;
           SCRBS_PRESSED  : LScrollDetails := tsUpperTrackHorzPressed;
           SCRBS_DISABLED : LScrollDetails := tsUpperTrackHorzDisabled;
           SCRBS_HOVER    : LScrollDetails := tsUpperTrackHorzNormal;//tsUpperTrackHorzHover; //no support for hover
         end;
        end;

        SBP_LOWERTRACKVERT:
        begin
         case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsLowerTrackVertNormal;
           SCRBS_HOT      : LScrollDetails := tsLowerTrackVertHot;
           SCRBS_PRESSED  : LScrollDetails := tsLowerTrackVertPressed;
           SCRBS_DISABLED : LScrollDetails := tsLowerTrackVertDisabled;
           SCRBS_HOVER    : LScrollDetails := tsLowerTrackVertNormal;//tsLowerTrackVertHover; //no support for hover
         end;
        end;

        SBP_UPPERTRACKVERT:
        begin
         case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsUpperTrackVertNormal;
           SCRBS_HOT      : LScrollDetails := tsUpperTrackVertHot;
           SCRBS_PRESSED  : LScrollDetails := tsUpperTrackVertPressed;
           SCRBS_DISABLED : LScrollDetails := tsUpperTrackVertDisabled;
           SCRBS_HOVER    : LScrollDetails := tsUpperTrackVertNormal;//tsUpperTrackVertHover; //no support for hover
         end;
        end;

        SBP_SIZEBOX :
        begin
         case iStateId of
          SZB_RIGHTALIGN            : LScrollDetails := tsSizeBoxRightAlign;
          SZB_LEFTALIGN             : LScrollDetails := tsSizeBoxLeftAlign;
          SZB_TOPRIGHTALIGN         : LScrollDetails := tsSizeBoxTopRightAlign;
          SZB_TOPLEFTALIGN          : LScrollDetails := tsSizeBoxTopLeftAlign;
          SZB_HALFBOTTOMRIGHTALIGN  : LScrollDetails := tsSizeBoxHalfBottomRightAlign;
          SZB_HALFBOTTOMLEFTALIGN   : LScrollDetails := tsSizeBoxHalfBottomLeftAlign;
          SZB_HALFTOPRIGHTALIGN     : LScrollDetails := tsSizeBoxHalfTopRightAlign;
          SZB_HALFTOPLEFTALIGN      : LScrollDetails := tsSizeBoxHalfTopLeftAlign;
         end;
        end;

        SBP_GRIPPERHORZ :
        begin
         case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsGripperHorzNormal;
           SCRBS_HOT      : LScrollDetails := tsGripperHorzHot;
           SCRBS_PRESSED  : LScrollDetails := tsGripperHorzPressed;
           SCRBS_DISABLED : LScrollDetails := tsGripperHorzDisabled;
           SCRBS_HOVER    : LScrollDetails := tsGripperHorzHover;//tsGripperHorzHover; //no support for hover
         end;
        end;

        SBP_GRIPPERVERT :
        begin
         case iStateId of
           SCRBS_NORMAL   : LScrollDetails := tsGripperVertNormal;
           SCRBS_HOT      : LScrollDetails := tsGripperVertHot;
           SCRBS_PRESSED  : LScrollDetails := tsGripperVertPressed;
           SCRBS_DISABLED : LScrollDetails := tsGripperVertDisabled;
           SCRBS_HOVER    : LScrollDetails := tsGripperVertNormal;//tsGripperVertHover; //no support for hover
         end;
        end;

      end;
      LDetails := StyleServices.GetElementDetails(LScrollDetails);

      if (iPartId=SBP_THUMBBTNHORZ) then
        StyleServices.DrawElement(hdc, StyleServices.GetElementDetails(tsUpperTrackHorzNormal), pRect, nil)
      else
      if (iPartId=SBP_THUMBBTNVERT) then
        StyleServices.DrawElement(hdc, StyleServices.GetElementDetails(tsUpperTrackVertNormal), pRect, nil);

      StyleServices.DrawElement(hdc, LDetails, pRect, nil);
      Exit(S_OK);
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Progressbar}
   if  (SameText(LThemeClass, VSCLASS_PROGRESS) or SameText(LThemeClass, VSCLASS_PROGRESS_INDERTERMINATE)) then
   begin
        case iPartId of
         PP_BAR         :   LDetails:=StyleServices.GetElementDetails(tpBar);
         PP_BARVERT     :   LDetails:=StyleServices.GetElementDetails(tpBarVert);
         PP_CHUNK       :   LDetails:=StyleServices.GetElementDetails(tpChunk);
         PP_CHUNKVERT   :   LDetails:=StyleServices.GetElementDetails(tpChunkVert);

         PP_FILL        :   if SameText(LThemeClass, VSCLASS_PROGRESS) then
                             LDetails:=StyleServices.GetElementDetails(tpChunk)//GetElementDetails(tpChunk);//GetElementDetails(tpFill);   not defined
                            else
                             LDetails:=StyleServices.GetElementDetails(tpBar);
         PP_FILLVERT    :    LDetails:=StyleServices.GetElementDetails(tpChunkVert);//GetElementDetails(tpFillVert); not defined

//      Use the Native PP_PULSEOVERLAY part to get better results.
//      PP_PULSEOVERLAY : if SameText(THThemesClasses.Items[hTheme], VSCLASS_PROGRESS) then
//                         LDetails:=StyleServices.GetElementDetails(tpChunk)//GetElementDetails(tpPulseOverlay);
//                        else
//                         LDetails:=StyleServices.GetElementDetails(tpBar);

         PP_MOVEOVERLAY      : if SameText(LThemeClass, VSCLASS_PROGRESS) then
                                LDetails:=StyleServices.GetElementDetails(tpMoveOverlay)
                               else
                                LDetails:=StyleServices.GetElementDetails(tpChunk);

//       PP_PULSEOVERLAYVERT :   LDetails:=StyleServices.GetElementDetails(tpPulseOverlayVert);
//       PP_MOVEOVERLAYVERT  :   LDetails:=StyleServices.GetElementDetails(tpMoveOverlayVert);

         PP_TRANSPARENTBAR       :   LDetails:=StyleServices.GetElementDetails(tpBar);//GetElementDetails(tpTransparentBarNormal); not defined
         PP_TRANSPARENTBARVERT   :   LDetails:=StyleServices.GetElementDetails(tpBarVert);//GetElementDetails(tpTransparentBarVertNormal); not defined
        else
        begin
          //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain hTheme %d iPartId %d iStateId %d', [hTheme, iPartId, iStateId])));
          Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
        end;
        end;

        SaveIndex := SaveDC(hdc);
        try
           if LHWND<>0  then
             DrawParentBackground(LHWND, hdc, pRect);
           StyleServices.DrawElement(hdc, LDetails, pRect, nil);
        finally
          RestoreDC(hdc, SaveIndex);
        end;
        Result:=S_OK;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_TaskDialog}
   if  SameText(LThemeClass, VSCLASS_TASKDIALOG) then
   begin

        case iPartId of

            TDLG_PRIMARYPANEL  :
            begin
              //LDetails:=StyleServices.GetElementDetails(ttdPrimaryPanel);   //ttdPrimaryPanel  this element is not included in the VCL Styles yet
              LColor:=StyleServices.GetStyleColor(scEdit);
              LCanvas:=TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle:=hdc;
                LCanvas.Brush.Color:=LColor;
                LCanvas.FillRect(pRect);
              finally
                LCanvas.Handle:=0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;
              Result:=S_OK;
            end;

            TDLG_FOOTNOTEPANE,
            TDLG_SECONDARYPANEL  :
            begin
              LDetails:=StyleServices.GetElementDetails(tpPanelBackground);   //ttdSecondaryPanel  this element is not included in the VCL Styles yet
              StyleServices.GetElementColor(LDetails, ecFillColor, LColor);
              LCanvas:=TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle:=hdc;
                LCanvas.Brush.Color:=LColor;
                LCanvas.FillRect(pRect);
              finally
                LCanvas.Handle:=0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;


              Result:=S_OK;
            end;

            TDLG_EXPANDOBUTTON :
            begin
              case iStateId of
                  TDLGEBS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedNormal);
                  TDLGEBS_HOVER      : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedHot);
                  TDLGEBS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronClosedPressed);

                  TDLGEBS_EXPANDEDNORMAL : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedNormal);
                  TDLGEBS_EXPANDEDHOVER     : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedHot);
                  TDLGEBS_EXPANDEDPRESSED        : LDetails:=StyleServices.GetElementDetails(tcpThemedChevronOpenedPressed);
              end;

              SaveIndex := SaveDC(hdc);
              try
                 if LHWND<>0  then
                   DrawParentBackground(LHWND, hdc, pRect);
                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Result:=S_OK;
            end;

            TDLG_FOOTNOTESEPARATOR :
            begin
              LStartColor := StyleServices.GetSystemColor(clBtnShadow);
              LEndColor   := StyleServices.GetSystemColor(clBtnHighlight);

              LCanvas:=TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle:=hdc;
                LCanvas.Pen.Color:=LStartColor;
                LCanvas.MoveTo(pRect.Left, pRect.Top);
                LCanvas.LineTo(pRect.Right, pRect.Top);
                LCanvas.Pen.Color:=LEndColor;
                LCanvas.MoveTo(pRect.Left, pRect.Top+1);
                LCanvas.LineTo(pRect.Right, pRect.Top+1);
              finally
                LCanvas.Handle:=0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              Result:=S_OK;
            end
        else
          begin
           //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
           Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
          end;
        end;

   end
  else
  {$ENDIF}
  {$IFDEF HOOK_Button}
  if  LThemeClasses.IndexOf(VSCLASS_BUTTON)>=0 then //SameText(LThemeClass, VSCLASS_BUTTON)  then
  begin
        case iPartId of

            BP_PUSHBUTTON  :
            begin
              case iStateId of
                  PBS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbPushButtonNormal);
                  PBS_HOT      : LDetails:=StyleServices.GetElementDetails(tbPushButtonHot);
                  PBS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbPushButtonPressed);
                  PBS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbPushButtonDisabled);
                  PBS_DEFAULTED     : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaulted);
                  PBS_DEFAULTED_ANIMATING        : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
              end;

              SaveIndex := SaveDC(hdc);
              try
                 if LHWND<>0  then
                   DrawParentBackground(LHWND, hdc, pRect);
                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Exit(S_OK);
            end;

            BP_COMMANDLINK  :
            begin

              case iStateId of
                  CMDLS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbPushButtonNormal);
                  CMDLS_HOT      : LDetails:=StyleServices.GetElementDetails(tbPushButtonHot);
                  CMDLS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbPushButtonPressed);
                  CMDLS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbPushButtonDisabled);
                  CMDLS_DEFAULTED     : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaulted);
                  CMDLS_DEFAULTED_ANIMATING        : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
              end;

//              case iStateId of
//                  CMDLS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbCommandLinkNormal);
//                  CMDLS_HOT      : LDetails:=StyleServices.GetElementDetails(tbCommandLinkHot);
//                  CMDLS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbCommandLinkPressed);
//                  CMDLS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbCommandLinkDisabled);
//                  CMDLS_DEFAULTED     : LDetails:=StyleServices.GetElementDetails(tbCommandLinkDefaulted);
//                  CMDLS_DEFAULTED_ANIMATING        : LDetails:=StyleServices.GetElementDetails(tbCommandLinkDefaultedAnimating);
//              end;

              SaveIndex := SaveDC(hdc);
              try
                 if LHWND<>0  then
                   DrawParentBackground(LHWND, hdc, pRect);
                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Exit(S_OK);
            end;

            BP_COMMANDLINKGLYPH  :
            begin
              case iStateId of
                  CMDLGS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbCommandLinkGlyphNormal);
                  CMDLGS_HOT      : LDetails:=StyleServices.GetElementDetails(tbCommandLinkGlyphHot);
                  CMDLGS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbCommandLinkGlyphPressed);
                  CMDLGS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled);
                  CMDLGS_DEFAULTED: LDetails:=StyleServices.GetElementDetails(tbCommandLinkGlyphDefaulted);
              end;

              SaveIndex := SaveDC(hdc);
              try
                 if LHWND<>0  then
                   DrawParentBackground(LHWND, hdc, pRect);
                 StyleServices.DrawElement(hdc, LDetails, pRect, nil);
              finally
                RestoreDC(hdc, SaveIndex);
              end;

              Exit(S_OK);
            end;

            BP_RADIOBUTTON  :
            begin
              case iStateId of
                  RBS_UNCHECKEDNORMAL   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                  RBS_UNCHECKEDHOT      : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                  RBS_UNCHECKEDPRESSED  : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                  RBS_UNCHECKEDDISABLED : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                  RBS_CHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                  RBS_CHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                  RBS_CHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                  RBS_CHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
              end;

              DrawStyleElement(hdc, LDetails, pRect);
              Exit(S_OK);
            end;

            BP_CHECKBOX  :
            begin
              case iStateId of
                  CBS_UNCHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
                  CBS_UNCHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
                  CBS_UNCHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedPressed);
                  CBS_UNCHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled);
                  CBS_CHECKEDNORMAL       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedNormal);
                  CBS_CHECKEDHOT          : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedHot);
                  CBS_CHECKEDPRESSED      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedPressed);
                  CBS_CHECKEDDISABLED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled);
                  CBS_MIXEDNORMAL         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedNormal);
                  CBS_MIXEDHOT            : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedHot);
                  CBS_MIXEDPRESSED        : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedPressed);
                  CBS_MIXEDDISABLED       : LDetails:=StyleServices.GetElementDetails(tbCheckBoxMixedDisabled);
                  { For Windows >= Vista }
                  CBS_IMPLICITNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitNormal);
                  CBS_IMPLICITHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitHot);
                  CBS_IMPLICITPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitPressed);
                  CBS_IMPLICITDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxImplicitDisabled);
                  CBS_EXCLUDEDNORMAL      : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedNormal);
                  CBS_EXCLUDEDHOT         : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedHot);
                  CBS_EXCLUDEDPRESSED     : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedPressed);
                  CBS_EXCLUDEDDISABLED    : LDetails:=StyleServices.GetElementDetails(tbCheckBoxExcludedDisabled);
              end;

              DrawStyleElement(hdc, LDetails, pRect);
              Exit(S_OK);
            end
        else
         begin
          //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
          Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
         end;
        end;
  end
  else
  {$ENDIF}
  {$IFDEF HOOK_TreeView}
  if  SameText(LThemeClass, VSCLASS_TREEVIEW) then
  begin

    case iPartId of
      TVP_GLYPH  :
                   begin
//                     case iStateId of
//                       GLPS_CLOSED : LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphClosed);
//                       GLPS_OPENED : LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphOpened);
//                     end;

//                     LBuffer:=TBitmap.Create;
//                     try
//                       LSize.cx:=11;
//                       LSize.cy:=11;
//                       LRect := Rect(0, 0, LSize.Width, LSize.Height);
//                       LBuffer.SetSize(LRect.Width, LRect.Height);
//
//                       LBuffer.Canvas.Brush.Color:=StyleServices.GetSystemColor(clWindow);
//                       LBuffer.Canvas.FillRect(LRect);
//
////                       if LHWND<>0  then
////                         DrawParentBackground(LHWND, LBuffer.Canvas.Handle, LRect);
//
//
//                       StyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
//                       BitBlt(hdc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
//                     finally
//                       LBuffer.Free;
//                     end;

                      SaveIndex := SaveDC(hdc);
                      LCanvas:=TCanvas.Create;
                      try
                        LCanvas.Handle:=hdc;
                        LStartColor:= StyleServices.GetSystemColor(clWindowText);

                        LCanvas.Pen.Color:=LStartColor;
                        LCanvas.Brush.Style:=bsClear;

                        LRect:= pRect;
                        LRect.Top :=  LRect.Top + 5;

                        if (iStateId=GLPS_OPENED) or (iStateId=HGLPS_OPENED) then
                          DrawArrow(LCanvas, TScrollDirection.sdDown, LRect.Location, 3)
                        else
                          DrawArrow(LCanvas, TScrollDirection.sdRight, LRect.Location, 3);
                      finally
                        LCanvas.Handle:=0;
                        LCanvas.Free;
                        RestoreDC(hdc, SaveIndex);
                      end;


                     Exit(S_OK);
                   end;


      TVP_HOTGLYPH :
                   begin
//                     case iStateId of
//                       HGLPS_CLOSED : LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphClosed);
//                       HGLPS_OPENED : LDetails := StyleServices.GetElementDetails(tcbCategoryGlyphOpened);
//                     end;
//
//                     LBuffer:=TBitmap.Create;
//                     try
//                       LSize.cx:=11;
//                       LSize.cy:=11;
//                       LRect := Rect(0, 0, LSize.Width, LSize.Height);
//                       LBuffer.SetSize(LRect.Width, LRect.Height);
//
//                       LBuffer.Canvas.Brush.Color:=StyleServices.GetSystemColor(clWindow);
//                       LBuffer.Canvas.FillRect(LRect);
//
//                       StyleServices.DrawElement(LBuffer.Canvas.Handle, LDetails, LRect);
//                       BitBlt(hdc, pRect.Left, pRect.Top, LRect.Width, LRect.Height, LBuffer.Canvas.Handle, 0, 0, SRCCOPY);
//                     finally
//                       LBuffer.Free;
//                     end;

                      SaveIndex := SaveDC(hdc);
                      LCanvas:=TCanvas.Create;
                      try
                        LCanvas.Handle:=hdc;
                        LStartColor:= StyleServices.GetSystemColor(clHighlightText);

                        LCanvas.Pen.Color:=LStartColor;
                        LCanvas.Brush.Style:=bsClear;

                        LRect:= pRect;
                        LRect.Top :=  LRect.Top + 5;

                        if (iStateId=HGLPS_OPENED) then
                          DrawArrow(LCanvas, TScrollDirection.sdDown, LRect.Location, 3)
                        else
                          DrawArrow(LCanvas, TScrollDirection.sdRight, LRect.Location, 3);
                      finally
                        LCanvas.Handle:=0;
                        LCanvas.Free;
                        RestoreDC(hdc, SaveIndex);
                      end;

                     Exit(S_OK);
                   end;




      TVP_TREEITEM  :
                      begin
                            case iStateId of
                              TREIS_HOT,
                              TREIS_SELECTED,
                              TREIS_SELECTEDNOTFOCUS,
                              TREIS_HOTSELECTED
                                                :
                                                 begin
                                                    SaveIndex := SaveDC(hdc);
                                                    LCanvas:=TCanvas.Create;
                                                    try
                                                      LCanvas.Handle:=hdc;
                                                      LStartColor:= StyleServices.GetSystemColor(clHighlight);
                                                      AlphaBlendFillCanvas(LCanvas, LStartColor, pRect, 96);
                                                      LCanvas.Pen.Color:=LStartColor;
                                                      LCanvas.Brush.Style:=bsClear;
                                                      LRect:=pRect;
                                                      LCanvas.Rectangle(LRect.Left, LRect.Top, LRect.Left +  LRect.Width,  LRect.Top + LRect.Height);
                                                    finally
                                                      LCanvas.Handle:=0;
                                                      LCanvas.Free;
                                                      RestoreDC(hdc, SaveIndex);
                                                    end;
                                                    Result:=S_OK;
                                                 end;
                            else
                                Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
                            end;
                      end;

    else
      begin
       //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
       Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
      end;
    end
  end
  else
  {$ENDIF}
  begin
    //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeBackgroundEx  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
    Exit(Trampoline(hTheme, hdc, iPartId, iStateId, pRect, Foo));
  end;

//  if Result<>S_OK then
//   OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeMain class %s hTheme %d iPartId %d iStateId %d', [LThemeClass, hTheme, iPartId, iStateId])));

 finally
  LThemeClasses.Free;
 end;

end;

function Detour_UxTheme_DrawThemeBackgroundEx(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pOptions: Pointer): HRESULT; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(TrampolineDrawThemeBackgroundEx(hTheme, hdc, iPartId, iStateId, pRect, pOptions))
  else
    Exit(Detour_UxTheme_DrawThemeMain(hTheme, hdc, iPartId, iStateId, pRect, pOptions, TrampolineDrawThemeBackgroundEx));
end;

function Detour_UxTheme_DrawThemeBackground(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  const pRect: TRect; pClipRect: Pointer): HRESULT; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(TrampolineDrawThemeBackground(hTheme, hdc, iPartId, iStateId, pRect, pClipRect))
  else
    Exit(Detour_UxTheme_DrawThemeMain(hTheme, hdc, iPartId, iStateId, pRect, pClipRect, TrampolineDrawThemeBackground));
end;


function Detour_UxTheme_DrawThemeEdge(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer; const pDestRect: TRect; uEdge, uFlags: UINT; pContentRect: PRECT): HRESULT; stdcall;
begin
 //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeEdge  class %s hTheme %d iPartId %d iStateId %d', [THThemesClasses.Items[hTheme],hTheme, iPartId, iStateId])));
 Exit(TrampolineDrawThemeEdge(hTheme, hdc, iPartId, iStateId, pDestRect, uEdge, uFlags, pContentRect));
end;

function Detour_GetThemeSysColor(hTheme: HTHEME; iColorId: Integer): COLORREF; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
   Result:= TrampolineGetThemeSysColor(hTheme, iColorId)
  else
   Result:= StyleServices.GetSystemColor(iColorId or Integer($FF000000));
end;

function Detour_GetThemeSysColorBrush(hTheme: HTHEME; iColorId: Integer): HBRUSH; stdcall;
begin
  //OutputDebugString(PChar(Format('GetThemeSysColorBrush hTheme %d iColorId %d', [hTheme, iColorId])));
  Exit(TrampolineGetThemeSysColorBrush(hTheme, iColorId));
end;


function Detour_GetThemeColor(hTheme: HTHEME; iPartId, iStateId, iPropId: Integer; var pColor: COLORREF): HRESULT;  stdcall;
var
  LThemeClass : string;
begin

 VCLStylesLock.Enter;
 try
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or not THThemesClasses.ContainsKey(hTheme) then
   Exit(TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor));
   LThemeClass:=THThemesClasses.Items[hTheme];
 finally
   VCLStylesLock.Leave;
 end;



      //if SameText('PROPERTREE', LThemeClass)   then
      //begin
       if iPropId = TMT_TEXTCOLOR then
       begin
         Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
         if (Result=S_OK) and  (pColor=TrampolineGetSysColor(COLOR_WINDOWTEXT)) then
         begin
           //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
           pColor:=ColorToRGB(StyleServices.GetSystemColor(clWindowText));
           Exit(S_OK);
         end;
       end;

     //    pColor:=StyleServices.GetSystemColor(clWindow);
     //    Exit;
     // end;


 //    pColor:=ColorToRGB(clYellow);
//     Exit(s_ok);
//              TMT_HEADING1TEXTCOLOR

    {$IFDEF HOOK_ListView}
    if (SameText(LThemeClass, VSCLASS_HEADER) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_HEADER)) then
    begin
        pColor:=clNone;
        case iPartId of

          HP_HEADERITEM :
              case iStateId  of
               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clBtnText));
              end;

        end;

       if pColor=clNone then
       begin
         Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
         //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
       end
       else
         Result:=S_OK;
    end
    else
    {$ENDIF}
    if LThemeClass<>'' then
    begin
      if  SameText(LThemeClass, VSCLASS_PREVIEWPANE) then
      begin

        pColor:=clNone;
        case iPartId of

          5 :
              case iStateId  of
               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));
              end;

          6 :
              case iStateId  of
               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clBtnText));
              end;

          7 :
              case iStateId  of
               1 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));
               //2 :  pColor:= clGreen;// StyleServices.GetSystemColor(clHighlight);
              end;
        end;

       if pColor=clNone then
       begin
         Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
         //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
       end
       else
         Result:=S_OK;

      end
      else
      {$IFDEF HOOK_TaskDialog}
      if SameText(LThemeClass, VSCLASS_TASKDIALOGSTYLE) then
      begin
         case iPartId of
           TDLG_MAININSTRUCTIONPANE : begin
                                       pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));
                                       Result:= S_OK;
                                      end;

           TDLG_CONTENTPANE         : begin
                                       pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                                       Result:= S_OK;
                                      end;
          TDLG_EXPANDOTEXT,
          TDLG_EXPANDEDFOOTERAREA,
          TDLG_FOOTNOTEPANE,
          TDLG_VERIFICATIONTEXT,
          //TDLG_RADIOBUTTONPANE,
          TDLG_EXPANDEDCONTENT      : begin
                                       pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                                       Result:= S_OK;
                                      end;
         else
          begin
           Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
           //pColor:=clRed;
           //Result:=S_OK;
           //OutputDebugString(PChar(Format('Detour_GetThemeColor Theme Class %LThemeClass hTheme %d iPartId %d iStateId %d  Color %8.x', [hTheme, iPartId, iStateId, pColor])));
          end;
         end;
      end
      else
      {$ENDIF}
     {$IFDEF HOOK_TreeView}
      if  SameText(LThemeClass, VSCLASS_TREEVIEW) or SameText(LThemeClass, VSCLASS_PROPERTREE) then
      begin

        pColor:=clNone;
        case iPartId of


          0, 2 :
              case iStateId  of
               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindow));//OK

              end;
//
//          TVP_BRANCH :
//              case iStateId  of
//               0 :  pColor:= StyleServices.GetSystemColor(clHighlight);//OK
//              end;
        end;

       if pColor=clNone then
       begin
         Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
         //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
       end
       else
         Result:=S_OK;
      end
      else
     {$ENDIF}
     {$IFDEF HOOK_ListView}
      if   (SameText(LThemeClass, VSCLASS_ITEMSVIEW) or  SameText(LThemeClass, VSCLASS_LISTVIEW) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) or SameText(LThemeClass, VSCLASS_EXPLORER_LISTVIEW))  then
      begin

        pColor:=clNone;
        case iPartId of
          0 :
                              case iStateId  of
                               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindow));
                              end;

          LVP_LISTITEM :
                              case iStateId  of
                               0 :  pColor:= ColorToRGB(ClRed);
                              end;

          LVP_LISTSORTEDDETAIL :
                              case iStateId  of
                               1 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                              end;


          LVP_EMPTYTEXT :
                              case iStateId  of
                               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));
                              end;

          LVP_GROUPHEADER :
                              case iStateId  of
                               0 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clWindowText));
                              end;
        end;

       //Result:=S_OK;
       if pColor=clNone then
       begin
         //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
         Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
       end
       else
         Result:= S_OK;
      end
      else
     {$ENDIF}
     {$IFDEF HOOK_CommandModule}
      if  SameText(LThemeClass, VSCLASS_COMMANDMODULE) then
      begin

        pColor:=clNone;
        case iPartId of

          //button with dropdown
          3 :
              case iStateId  of
               1 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));//OK
               6 :  pColor:= clYellow;//StyleServices.GetSystemColor(clBtnShadow);
              end;

         9 :
              case iStateId  of
               1 :  pColor:=ColorToRGB(StyleServices.GetSystemColor(clBtnText));
               //Highlight
               2 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clBtnText)); //OK
               3 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clBtnText)); //OK
               6 :  pColor:= clLime;//StyleServices.GetSystemColor(clBtnShadow);
              end;

          10 :
              case iStateId  of
               1 :  pColor:= ColorToRGB(StyleServices.GetSystemColor(clHighlight));
              end;
        end;

       Result:=S_OK;
       //if pColor=clNone then
         //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end
      else
     {$ENDIF}
      begin
       Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
      // pColor:=clRed;
       //Result:=S_OK;
       //OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
      end;
    end
    else
    begin
     Result:=TrampolineGetThemeColor(hTheme, iPartId, iStateId, iPropId, pColor);
     //pColor:=clFuchsia;
     //Result:=S_OK;
     //OutputDebugString(PChar(Format('Detour_GetThemeColor hTheme %d iPartId %d iStateId %d  Color %8.x', [hTheme, iPartId, iStateId, pColor])));
     //OutputDebugString2(Format('Detour_GetThemeColor hTheme %d iPartId %d iStateId %d  Color %8.x', [hTheme, iPartId, iStateId, pColor]));
    end;

//    if pColor=7104329 then
//      OutputDebugString(PChar(Format('Detour_GetThemeColor Class %s hTheme %d iPartId %d iStateId %d  iPropId %d Color %8.x', [LThemeClass, hTheme, iPartId, iStateId, iPropId, pColor])));
end;


procedure _DrawControlText(Canvas: TCanvas; const S: string; var R: TRect; Flags: Cardinal; ThemeTextColor: TColor);
var
  TextFormat: TTextFormatFlags;
begin
  //Canvas.Font := TWinControlClass(Control).Font;
  Canvas.Font.Color := ThemeTextColor;
  TextFormat := TTextFormatFlags(Flags);
   DrawText(Canvas.Handle, S,  Length(S), R, TTextFormatFlags(TextFormat));
end;


function Detour_UxTheme_DrawThemeText(hTheme: HTHEME; hdc: HDC; iPartId, iStateId: Integer;  pszText: LPCWSTR; iCharCount: Integer; dwTextFlags, dwTextFlags2: DWORD; const pRect: TRect): HRESULT; stdcall;
var
  LThemeClass : string;
  LThemeClasses : TStrings;
  LDetails: TThemedElementDetails;
  ThemeTextColor : TColor;
  p, SaveIndex : Integer;
  LCanvas : TCanvas;
  plf: LOGFONTW;
  LText : string;
  LRect : TRect;
begin
 LThemeClasses:=TStringList.Create;
 try
   VCLStylesLock.Enter;
   try
     if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or (dwTextFlags and DT_CALCRECT <> 0) or not THThemesClasses.ContainsKey(hTheme) then
      Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));

     LThemeClass:=THThemesClasses.Items[hTheme];
     ExtractStrings([';'], [], PChar(LThemeClass), LThemeClasses);
     //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s %s', [hTheme, iPartId, iStateId, pszText, LThemeClass])));

//     if Pos('Name', pszText)>0 then
//       OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d class %s iPartId %d iStateId %d  text %s', [hTheme, LThemeClass, iPartId, iStateId, pszText])));


   finally
     VCLStylesLock.Leave;
   end;

   if LThemeClass<>'' then
   begin

     {$IFDEF HOOK_ToolBar}
     if SameText(LThemeClass, VSCLASS_TOOLBAR) then //OK
     begin
       case iPartId of
          0  :
                          begin
                            case iStateId of
                              TS_NORMAL    :
                                            begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clBtnText);
                                               LDetails:=StyleServices.GetElementDetails(ttbButtonNormal);
                                            end;

                              TS_HOT    :
                                            begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clHighlight);
                                               LDetails:=StyleServices.GetElementDetails(ttbButtonHot);
                                            end;

                              TS_PRESSED    :
                                            begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clHighlight);
                                               LDetails:=StyleServices.GetElementDetails(ttbButtonPressed);
                                            end;

                              TS_NEARHOT    :
                                            begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clBtnText);
                                               LDetails:=StyleServices.GetElementDetails(ttbButtonHot);
                                            end;

                              TS_OTHERSIDEHOT    :
                                            begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clHighlight);
                                               LDetails:=StyleServices.GetElementDetails(ttbButtonHot);
                                            end;

//                            else
//                                OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                            end;

                            LRect:=pRect;
                            StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                            Exit(S_OK);
                          end;
       else
           Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
       end;
     end
     else
     {$ENDIF}
     {$IFDEF HOOK_CommandModule}
     if  SameText(LThemeClass, VSCLASS_COMMANDMODULE) then
     begin
        //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
        Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
     end
     else
     {$ENDIF}
     {$IFDEF HOOK_Menu}
     if  SameText(LThemeClass, VSCLASS_MENU) then
     begin
        case iPartId of

           MENU_POPUPITEM      :
                                   begin
                                     SaveIndex := SaveDC(hdc);
                                     try
                                      case iStateId of
                                        MPI_NORMAL         : LDetails:=StyleServices.GetElementDetails(tmPopupItemNormal);
                                        MPI_HOT            : LDetails:=StyleServices.GetElementDetails(tmPopupItemHot);
                                        //MPI_PUSHED         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemPushed);
                                        MPI_DISABLED       : LDetails:=StyleServices.GetElementDetails(tmPopupItemDisabled);
                                        MPI_DISABLEDHOT    : LDetails:=StyleServices.GetElementDetails(tmPopupItemDisabledHot);
                                        //MPI_DISABLEDPUSHED : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                                      end;

                                      LRect:=pRect;
                                      StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
                                      LText :=  string(pszText);

                                      //SetTextColor(hdc, ColorToRGB(clred));

//                                      LCanvas:= TCanvas.Create;
//                                      try
//                                        LCanvas.Handle:=hdc;
//                                        LCanvas.Brush.Style:=bsClear;
//                                        _DrawControlText(LCanvas, LText, LRect, TTextFormatFlags(dwTextFlags), clred);
//                                      finally
//                                        LCanvas.Handle:=0;
//                                        LCanvas.Free;
//                                      end;
                                      StyleServices.DrawText(hdc, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                      Exit(S_OK);
                                     finally
                                       RestoreDC(hdc, SaveIndex);
                                     end;
                                   end;

           MENU_BARITEM      :
                                   begin
                                     SaveIndex := SaveDC(hdc);
                                     try
                                      case iStateId of
                                        MBI_NORMAL         : LDetails:=StyleServices.GetElementDetails(tmPopupItemNormal);
                                        MBI_HOT            : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemHot);
                                        MBI_PUSHED         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemPushed);
                                        MBI_DISABLED       : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabled);
                                        MBI_DISABLEDHOT    : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledHot);
                                        MBI_DISABLEDPUSHED : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                                      end;

                                      LRect:=pRect;
                                      StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                      Exit(S_OK);
                                     finally
                                       RestoreDC(hdc, SaveIndex);
                                     end;
                                   end;
         else
         begin
            Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
         end;
        end;
     end
     else
     {$ENDIF}
     {$IFDEF  HOOK_DateTimePicker}
     if SameText(LThemeClass, VSCLASS_DATEPICKER) then
     begin
       case iPartId of
          DP_DATETEXT :
                          begin
                            case iStateId of
                              DPDT_NORMAL    :begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clWindowText);
                                               LDetails:=StyleServices.GetElementDetails(teEditTextNormal);
                                              end;
                              DPDT_DISABLED  :begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clGrayText);
                                               LDetails:=StyleServices.GetElementDetails(teEditTextDisabled);
                                              end;
                              DPDT_SELECTED  :begin
                                               ThemeTextColor:=StyleServices.GetSystemColor(clHighlightText);
                                               LDetails:=StyleServices.GetElementDetails(tgCellSelected); //Fix issue with selected text color
                                              end;
                            end;

                            LRect:=pRect;
                            StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                            Exit(S_OK);
                          end;
       else
           Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
       end;
     end
     else
     if SameText(LThemeClass, VSCLASS_MONTHCAL) then
     begin
       case iPartId of
          MC_GRIDCELL  : begin
  //                          case iStateId of
  //                           MCGCB_SELECTED           :   LDetails:=StyleServices.GetElementDetails(tgCellSelected);
  //                           MCGCB_HOT                :   LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
  //                           MCGCB_SELECTEDHOT        :   LDetails:=StyleServices.GetElementDetails(tgCellSelected);
  //                           MCGCB_SELECTEDNOTFOCUSED :   LDetails:=StyleServices.GetElementDetails(tgCellSelected);
  //                           MCGCB_TODAY              :   LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
  //                          else
  //                              LDetails:=StyleServices.GetElementDetails(tgCellNormal);
  //                          end;

                            LDetails:=StyleServices.GetElementDetails(tgCellNormal);

                            if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                              ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

                              LCanvas:=TCanvas.Create;
                              SaveIndex := SaveDC(hdc);
                              try
                                LCanvas.Handle:=hdc;
                                ZeroMemory(@plf, SizeOf(plf));
                                plf.lfHeight := 13;
                                plf.lfCharSet := DEFAULT_CHARSET;
                                StrCopy(plf.lfFaceName, 'Tahoma');
                                LCanvas.Font.Handle := CreateFontIndirect(plf);

                                LText :=  string(pszText);
                                p:=Pos(Chr($A), LText);
                                if p>1 then
                                   LText:=Copy(LText, 1, p-1);

                                LRect:=pRect;
                                StyleServices.DrawText(LCanvas.Handle, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                              finally
                                DeleteObject(LCanvas.Font.Handle);
                                LCanvas.Handle:=0;
                                LCanvas.Free;
                                RestoreDC(hdc, SaveIndex);
                              end;

                            //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                            Exit(S_OK);
                         end;

         MC_TRAILINGGRIDCELL :
                         begin
                            case iStateId of
                             MCTGC_HOT                :   LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
                             MCTGC_HASSTATE           :   LDetails:=StyleServices.GetElementDetails(tgCellSelected);
                             MCTGC_HASSTATEHOT        :   LDetails:=StyleServices.GetElementDetails(tgCellSelected);
                             MCTGC_TODAY              :   LDetails:=StyleServices.GetElementDetails(tgFixedCellHot);
                            else
                                LDetails:=StyleServices.GetElementDetails(teEditTextDisabled);
                            end;

                            if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                              ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

                              LCanvas:=TCanvas.Create;
                              SaveIndex := SaveDC(hdc);
                              try
                                LCanvas.Handle:=hdc;
                                ZeroMemory(@plf, SizeOf(plf));
                                plf.lfHeight := 13;
                                plf.lfCharSet := DEFAULT_CHARSET;
                                StrCopy(plf.lfFaceName, 'Tahoma');
                                LCanvas.Font.Handle := CreateFontIndirect(plf);

                                LText :=  string(pszText);
                                p:=Pos(Chr($A), LText);
                                if p>1 then
                                   LText:=Copy(LText, 1, p-1);

                                LRect:=pRect;
                                StyleServices.DrawText(LCanvas.Handle, LDetails, LText, LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                              finally
                                DeleteObject(LCanvas.Font.Handle);
                                LCanvas.Handle:=0;
                                LCanvas.Free;
                                RestoreDC(hdc, SaveIndex);
                              end;
                            Exit(S_OK);
                         end;
       else
       begin
         //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
         Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
       end;
       end;
     end
     else
     {$ENDIF}
     {$IFDEF HOOK_Button}
     if LThemeClasses.IndexOf(VSCLASS_BUTTON)>=0 then //SameText(LThemeClass, VSCLASS_BUTTON) then
     begin
          case iPartId of
              BP_PUSHBUTTON  :
                                begin
                                  case iStateId of
                                      PBS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbPushButtonNormal);
                                      PBS_HOT      : LDetails:=StyleServices.GetElementDetails(tbPushButtonHot);
                                      PBS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbPushButtonPressed);
                                      PBS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbPushButtonDisabled);
                                      PBS_DEFAULTED            : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaulted);
                                      PBS_DEFAULTED_ANIMATING  : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
                                  end;

                                  //StyleServices.DrawText(hdc,  LDetails, string(pszText), pRect, dwTextFlags, dwTextFlags2);

                                  if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                                   ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                                  LRect:=pRect;
                                  StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                  Exit(S_OK);
                                end;

              BP_RADIOBUTTON  : begin
                                  case iStateId of
                                      RBS_UNCHECKEDNORMAL   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedNormal);
                                      RBS_UNCHECKEDHOT      : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedHot);
                                      RBS_UNCHECKEDPRESSED  : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedPressed);
                                      RBS_UNCHECKEDDISABLED : LDetails:=StyleServices.GetElementDetails(tbRadioButtonUncheckedDisabled);
                                      RBS_CHECKEDNORMAL     : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedNormal);
                                      RBS_CHECKEDHOT        : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedHot);
                                      RBS_CHECKEDPRESSED    : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedPressed);
                                      RBS_CHECKEDDISABLED   : LDetails:=StyleServices.GetElementDetails(tbRadioButtonCheckedDisabled);
                                  end;

                                  if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                                   ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                                  LRect:=pRect;
                                  StyleServices.DrawText(hdc, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                  Exit(S_OK);
                                end;

              BP_COMMANDLINK :
                                begin

                                  case iStateId of
                                      CMDLS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbPushButtonNormal);
                                      CMDLS_HOT      : LDetails:=StyleServices.GetElementDetails(tbPushButtonHot);
                                      CMDLS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbPushButtonPressed);
                                      CMDLS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbPushButtonDisabled);
                                      CMDLS_DEFAULTED     : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaulted);
                                      CMDLS_DEFAULTED_ANIMATING        : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
                                  end;

                                  LCanvas:=TCanvas.Create;
                                  SaveIndex := SaveDC(hdc);
                                  try
                                    LCanvas.Handle:=hdc;
                                    ZeroMemory(@plf, SizeOf(plf));
                                    plf.lfHeight := 14;
                                    plf.lfCharSet := DEFAULT_CHARSET;
                                    StrCopy(plf.lfFaceName, 'Tahoma');
                                    LCanvas.Font.Handle := CreateFontIndirect(plf);
                                    if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                                     ThemeTextColor := StyleServices.GetSystemColor(clBtnText);
                                    LRect:=pRect;
                                    StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), LRect, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                  finally
                                    DeleteObject(LCanvas.Font.Handle);
                                    LCanvas.Handle:=0;
                                    LCanvas.Free;
                                    RestoreDC(hdc, SaveIndex);
                                  end;

                                  Exit(S_OK);
                                end
              else
              begin
                 //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                 Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
              end;
          end;
     end
     else
     {$ENDIF}
     begin
      //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
      Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
     end;
   end
   else
     Exit(TrampolineDrawThemeText(hTheme, hdc, iPartId, iStateId, pszText, iCharCount, dwTextFlags, dwTextFlags2, pRect));
 finally
   LThemeClasses.Free;
 end;
end;




function  Detour_UxTheme_DrawThemeTextEx(hTheme: HTHEME; hdc: HDC; iPartId: Integer; iStateId: Integer; pszText: LPCWSTR; cchText: Integer; dwTextFlags: DWORD; pRect: PRect; var pOptions: TDTTOpts): HResult; stdcall;
var
  LDetails: TThemedElementDetails;
  ThemeTextColor : TColor;
  SaveIndex : Integer;
  LCanvas : TCanvas;
  LThemeClass : string;
  plf: LOGFONTW;
begin

 //Result:=TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions);
 VCLStylesLock.Enter;
 try
   if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled or (dwTextFlags and DT_CALCRECT <> 0) or not THThemesClasses.ContainsKey(hTheme) then
    Exit(TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions));

   LThemeClass:= THThemesClasses.Items[hTheme];
 finally
   VCLStylesLock.Leave;
 end;

//  if SameText(pszText, 'Name') then
//   OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));

//   if SameText(LThemeClass, VSCLASS_PREVIEWPANE) then
//       OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeText hTheme %d class %s iPartId %d iStateId %d  text %s', [hTheme, LThemeClass, iPartId, iStateId, pszText])));

 if LThemeClass<>'' then
 begin
     {$IFDEF HOOK_EDIT}
//     if  SameText(LThemeClass, VSCLASS_EDIT) then  //TODO
//     begin
//        ThemeTextColor := clNone;
//
//        case iPartId of
//              0 :
//                   case  iStateId of
//                       0 :
//                           begin
//                             ThemeTextColor:=clRed;//StyleServices.GetSystemColor(clWindowText);
//                             LDetails:=StyleServices.GetElementDetails(teEditTextNormal);
//                           end;
//                   end;
//
//
//            EP_EDITTEXT :
//                   case  iStateId of
//                       ETS_DISABLED :
//                           begin
//                             ThemeTextColor:=clBlue;//StyleServices.GetSystemColor(clWindowText);
//                             LDetails:=StyleServices.GetElementDetails(teEditTextNormal);
//                           end;
//                   end;
//
//
//        end;
//
//        if ThemeTextColor<>clNone then
//        begin
//           StyleServices.DrawText(hdc, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags), ThemeTextColor);
//           Exit(S_OK);
//        end
//        else
//         Exit(TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions));
//
//     end
//     else
     {$ENDIF}
     {$IFDEF HOOK_PopMenu}
     if  SameText(LThemeClass, VSCLASS_MENU) then
     begin
        case iPartId of

           MENU_POPUPITEM      :
                                   begin
                                     SaveIndex := SaveDC(hdc);
                                     try

                                      case iStateId of
                                        MPI_NORMAL         : LDetails:=StyleServices.GetElementDetails(tmPopupItemNormal);
                                        MPI_HOT            : LDetails:=StyleServices.GetElementDetails(tmPopupItemHot);
                                        //MPI_PUSHED         : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemPushed);
                                        MPI_DISABLED       : LDetails:=StyleServices.GetElementDetails(tmPopupItemDisabled);
                                        MPI_DISABLEDHOT    : LDetails:=StyleServices.GetElementDetails(tmPopupItemDisabledHot);
                                        //MPI_DISABLEDPUSHED : LDetails:=StyleServices.GetElementDetails(tmMenuBarItemDisabledPushed);
                                      end;


                                      StyleServices.DrawText(hdc, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                      Result:=S_OK;
                                     finally
                                       RestoreDC(hdc, SaveIndex);
                                     end;
                                   end;
         else
         begin
             //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
             Exit(TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions));
         end;
        end;
     end
     else
     {$ENDIF}
   {$IFDEF HOOK_ListView}
   if SameText(LThemeClass, VSCLASS_LISTVIEW) or SameText(LThemeClass, VSCLASS_ITEMSVIEW_LISTVIEW) then
   begin
        case iPartId of
          LVP_GROUPHEADER :
                           begin
                             if iStateId=0 then
                             begin
                                  LCanvas:=TCanvas.Create;
                                  SaveIndex := SaveDC(hdc);
                                  try
                                    LCanvas.Handle:=hdc;
                                    if pOptions.dwFlags AND DTT_FONTPROP <> 0  then
                                    begin
                                      ZeroMemory(@plf, SizeOf(plf));
                                      plf.lfHeight := 13;
                                      plf.lfCharSet := DEFAULT_CHARSET;
                                      StrCopy(plf.lfFaceName, 'Tahoma');
                                      LCanvas.Font.Handle := CreateFontIndirect(plf);
                                    end;
                                    LDetails:=StyleServices.GetElementDetails(tlListItemNormal);
                                    ThemeTextColor:=StyleServices.GetStyleFontColor(sfListItemTextNormal);
                                    StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags), ThemeTextColor);
                                  finally
                                    if pOptions.dwFlags AND DTT_FONTPROP <> 0  then
                                      DeleteObject(LCanvas.Font.Handle);
                                    LCanvas.Handle:=0;
                                    LCanvas.Free;
                                    RestoreDC(hdc, SaveIndex);
                                  end;

                                  Result:=S_OK;
                             end
                             else
                             begin
                                 //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
                                 Exit(TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions));
                             end;
                           end;

        else
          begin
             //OutputDebugString(PChar(Format('Detour_UxTheme_DrawThemeTextEx hTheme %d iPartId %d iStateId %d  text %s', [hTheme, iPartId, iStateId, pszText])));
             Exit(TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions));
          end;
        end;
   end
   else
   {$ENDIF}
   {$IFDEF HOOK_Button}
   if SameText(LThemeClass, VSCLASS_BUTTON) then
   begin
        case iPartId of
            BP_COMMANDLINK  :
            begin
              case iStateId of
                  CMDLS_NORMAL   : LDetails:=StyleServices.GetElementDetails(tbPushButtonNormal);
                  CMDLS_HOT      : LDetails:=StyleServices.GetElementDetails(tbPushButtonHot);
                  CMDLS_PRESSED  : LDetails:=StyleServices.GetElementDetails(tbPushButtonPressed);
                  CMDLS_DISABLED : LDetails:=StyleServices.GetElementDetails(tbPushButtonDisabled);
                  CMDLS_DEFAULTED     : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaulted);
                  CMDLS_DEFAULTED_ANIMATING        : LDetails:=StyleServices.GetElementDetails(tbPushButtonDefaultedAnimating);
              end;


              if not StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
                ThemeTextColor := StyleServices.GetSystemColor(clBtnText);

              LCanvas:=TCanvas.Create;
              SaveIndex := SaveDC(hdc);
              try
                LCanvas.Handle:=hdc;
                if pOptions.dwFlags AND DTT_FONTPROP <> 0  then
                begin
                  //GetThemeSysFont(hTheme, pOptions.iFontPropId, plf);  // is not working
                  ZeroMemory(@plf, SizeOf(plf));
                  plf.lfHeight := 13;
                  plf.lfCharSet := DEFAULT_CHARSET;
                  StrCopy(plf.lfFaceName, 'Tahoma');
                  LCanvas.Font.Handle := CreateFontIndirect(plf);
                end;
                StyleServices.DrawText(LCanvas.Handle, LDetails, string(pszText), pRect^, TTextFormatFlags(dwTextFlags), ThemeTextColor);
              finally
                if pOptions.dwFlags AND DTT_FONTPROP <> 0  then
                  DeleteObject(LCanvas.Font.Handle);
                LCanvas.Handle:=0;
                LCanvas.Free;
                RestoreDC(hdc, SaveIndex);
              end;

              Result:=S_OK;
            end
            else
               Result:=TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions);
        end;
   end
   else
   {$ENDIF}
    Result:=TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions);
 end
 else
   Result:=TrampolineDrawThemeTextEx(hTheme, hdc, iPartId, iStateId, pszText, cchText, dwTextFlags, pRect, pOptions);
end;


const
  themelib = 'uxtheme.dll';

initialization
 VCLStylesLock    := TCriticalSection.Create;
 THThemesClasses  := TDictionary<HTHEME, string>.Create;
 THThemesHWND     := TDictionary<HTHEME, HWND>.Create;

 if StyleServices.Available then
 begin
    @TrampolineOpenThemeData          := InterceptCreate(themelib, 'OpenThemeData', @Detour_UxTheme_OpenThemeData);
    @TrampolineOpenThemeDataEx        := InterceptCreate(themelib, 'OpenThemeDataEx', @Detour_UxTheme_OpenThemeDataEx);
    @TrampolineDrawThemeBackground    := InterceptCreate(themelib, 'DrawThemeBackground', @Detour_UxTheme_DrawThemeBackground);
    @TrampolineDrawThemeBackgroundEx  := InterceptCreate(themelib, 'DrawThemeBackgroundEx', @Detour_UxTheme_DrawThemeBackgroundEx);
    @TrampolineDrawThemeEdge          := InterceptCreate(themelib, 'DrawThemeEdge', @Detour_UxTheme_DrawThemeEdge);

    @TrampolineDrawThemeText          := InterceptCreate(themelib,'DrawThemeText', @Detour_UxTheme_DrawThemeText);
    @TrampolineDrawThemeTextEx        := InterceptCreate(themelib,'DrawThemeTextEx', @Detour_UxTheme_DrawThemeTextEx);
    @TrampolineGetThemeSysColor       := InterceptCreate(themelib,'GetThemeSysColor', @Detour_GetThemeSysColor);
    @TrampolineGetThemeSysColorBrush  := InterceptCreate(themelib,'GetThemeSysColorBrush', @Detour_GetThemeSysColorBrush);
    @TrampolineGetThemeColor          := InterceptCreate(themelib,'GetThemeColor', @Detour_GetThemeColor);
 end;

finalization
  InterceptRemove(@TrampolineGetThemeSysColor);
  InterceptRemove(@TrampolineGetThemeSysColorBrush);
  InterceptRemove(@TrampolineOpenThemeData);
  InterceptRemove(@TrampolineOpenThemeDataEx);
  InterceptRemove(@TrampolineGetThemeColor);
  InterceptRemove(@TrampolineDrawThemeBackground);
  InterceptRemove(@TrampolineDrawThemeText);
  InterceptRemove(@TrampolineDrawThemeTextEx);
  InterceptRemove(@TrampolineDrawThemeBackgroundEx);
  InterceptRemove(@TrampolineDrawThemeEdge);

  THThemesClasses.Free;
  THThemesHWND.Free;

  VCLStylesLock.Free;
  VCLStylesLock := nil;
end.
