// **************************************************************************************************
//
// Unit Vcl.Styles.Hooks
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
// The Original Code is Vcl.Styles.Hooks.pas.
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2015 Rodrigo Ruz V.
//
// Contributor(s): Mahdi Safsafi.
//
// All Rights Reserved.
//
// **************************************************************************************************
unit Vcl.Styles.Hooks;

interface

uses
  WinApi.Windows;

var
  TrampolineGetSysColorBrush: function(nIndex: Integer): HBRUSH; stdcall;
  TrampolineGetSysColor: function(nIndex: Integer): DWORD; stdcall;


implementation

{$DEFINE HOOK_UXTHEME}
{$DEFINE HOOK_TDateTimePicker}
{$DEFINE HOOK_TProgressBar}


uses
  DDetours,
  System.SyncObjs,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Generics.Collections,
  System.StrUtils,
  WinApi.Messages,
  Vcl.Graphics,
{$IFDEF HOOK_UXTHEME}
  Vcl.Styles.Utils.Graphics,
  Vcl.Styles.UxTheme,
{$ENDIF HOOK_UXTHEME}
  Vcl.Styles.Utils.SysControls,
  Vcl.Forms,
  Vcl.StdCtrls,
  Vcl.ComCtrls,
  Vcl.Themes;

type
  TListStyleBrush = TObjectDictionary<Integer, HBRUSH>;
  TSetStyle = procedure(Style: TCustomStyleServices) of object;

var
  VCLStylesBrush: TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock: TCriticalSection = nil;
  LSetStylePtr: TSetStyle;

  TrampolineFillRect  : function(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
  TrampolineDrawEdge  : function(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL;  stdcall = nil;
  TrampolineSetStyle  : procedure(Self: TObject; Style: TCustomStyleServices);
  {$IFDEF HOOK_UXTHEME}
  TrampolineLoadImageW: function (hInst: HINST; ImageName: LPCWSTR; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall = nil;
  {$ENDIF HOOK_UXTHEME}


function Detour_DrawEdge(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL; stdcall;
var
  CanDraw: Boolean;
  SaveIndex : Integer;
begin
  CanDraw := (not StyleServices.IsSystemStyle) and (TSysStyleManager.Enabled);
  if (CanDraw) and (edge <> BDR_OUTER) and (edge <> BDR_INNER) then
  begin
    SaveIndex:=SaveDC(hDC);
    try
      DrawStyleEdge(hDC, qrc, TStyleElementEdges(edge), TStyleElementEdgeFlags(grfFlags));
    finally
      RestoreDC(hDC, SaveIndex);
    end;
    Exit(True);
  end;
  Exit(TrampolineDrawEdge(hDC, qrc, edge, grfFlags));
end;

function Detour_FillRect(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(TrampolineFillRect(hDC, lprc, hbr))
  else if (hbr > 0) and (hbr < COLOR_ENDCOLORS + 1) then
    Exit(TrampolineFillRect(hDC, lprc, GetSysColorBrush(hbr - 1)))
  else
    Exit(TrampolineFillRect(hDC, lprc, hbr));
end;

function Detour_GetSysColor(nIndex: Integer): DWORD; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Result := TrampolineGetSysColor(nIndex)
  else if nIndex = COLOR_HOTLIGHT then
    Result := DWORD(StyleServices.GetSystemColor(clHighlight))
  else
    Result := DWORD(StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000))));
end;

function Detour_GetSysColorBrush(nIndex: Integer): HBRUSH; stdcall;
var
  LCurrentStyleBrush: TListStyleBrush;
  LBrush: HBRUSH;
  LColor: TColor;
begin
  {
    The reason to change the previous code implementation
    is that the win32 graphics may differ with the VCL graphics:
    Eg: TColor is signed in VCL and Unsigned in Win32Api.
    When hooking : keep always using the native way !
    Need Color ?
    Use GetObject with LOGBRUSH ! or use TBrushColorPair !
  }
  VCLStylesLock.Enter;
  try
    if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
      Exit(TrampolineGetSysColorBrush(nIndex))
    else
    begin
      if VCLStylesBrush.ContainsKey(StyleServices.Name) then
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name]
      else
      begin
        VCLStylesBrush.Add(StyleServices.Name, TListStyleBrush.Create());
        LCurrentStyleBrush := VCLStylesBrush.Items[StyleServices.Name];
      end;
      if Assigned(LCurrentStyleBrush) then
      begin
        if LCurrentStyleBrush.ContainsKey(nIndex) then
          Exit(LCurrentStyleBrush[nIndex])
        else
        begin
          LColor := StyleServices.GetSystemColor(TColor(nIndex or Integer($FF000000)));
          LBrush := CreateSolidBrush(LColor);
          LCurrentStyleBrush.Add(nIndex, LBrush);
          Exit(LBrush);
        end;
      end;
      Exit(TrampolineGetSysColorBrush(nIndex));
    end;
  finally
    VCLStylesLock.Leave;
  end;
end;

procedure Detour_SetStyle(Self: TObject; Style: TCustomStyleServices);
var
  I: Integer;
  LActiveStyle: TCustomStyleServices;
begin
  LActiveStyle := TStyleManager.ActiveStyle;
  TrampolineSetStyle(Self, Style);
  if (Style <> LActiveStyle) then
  begin
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].HandleAllocated then
        SendMessage(Screen.Forms[I].Handle, WM_SYSCOLORCHANGE, 0, 0);
  end;
end;

{$IFDEF HOOK_UXTHEME}


procedure _BlendMultiply(const AColor: TColor; Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  if AColor=clWhite then
   NewColor:=AColor
  else
  begin
    ARGB := Value;
    GetRGB(AColor, r, g, b);
    GetRGB(ARGB, br, bg, bb);
    r:=(r*br) shr 8;
    g:=(g*bg) shr 8;
    b:=(b*bb) shr 8;
    NewColor:= RGB(r,g,b);
  end;
end;

function RoundIntToByte(i: integer): byte;
begin
  if i > 255 then Result := 255
  else
  if i < 0   then Result := 0
  else
    Result := i;
end;

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
  br, bg, bb   : byte;
  c            : Integer;
begin
  if AColor=clWhite then
   NewColor:=AColor
  else
  begin
    GetRGB(AColor, r,g, b);
    ARGB := Value;
    GetRGB(ARGB, br,bg, bb);

    if br=0 then
     r:=0
    else
    begin
     c:=RoundIntToByte(255-(((255-r) SHL 8) DIV br));
     r:=c;
    end;

    if bg=0 then
     g:=0
    else
    begin
     c:=RoundIntToByte(255-(((255-g) SHL 8) DIV bg));
     g:=c;
    end;

    if bb=0 then
     b:=0
    else
    begin
     c:=RoundIntToByte(255-(((255-b) SHL 8) DIV bb));
     b:=c;
    end;

    NewColor:=RGB(r, g, b);
  end;
end;



function Detour_LoadImageW(hInst: HINST; ImageName: LPCWSTR; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall;
const
  ExplorerFrame = 'explorerframe.dll';
var
  hModule : WinApi.Windows.HMODULE;
  LBitmap, LBuffer : TBitmap;
  s       : string;
  LRect : TRect;
  LBackColor, LColor : TColor;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(TrampolineLoadImageW(hInst, ImageName, ImageType, X, Y, Flags));


  if (hInst>0) and (ImageType=IMAGE_BITMAP) and (X=0) and (Y=0) and IS_INTRESOURCE(ImageName) then
  begin
    hModule:=GetModuleHandle(ExplorerFrame);
    if (hModule = hInst) then
    begin
      s := IntToStr(Integer(ImageName));
      Result:= TrampolineLoadImageW(hInst, ImageName, ImageType, X, Y, Flags);
      LBitmap:=TBitmap.Create;
      try
        LBitmap.Handle := Result;

        //Only for Windows Vista and 7
        if (TOSVersion.Major=6) and ((TOSVersion.Minor=0) or (TOSVersion.Minor=1)) then
        begin
          LBackColor:= StyleServices.GetSystemColor(clWindow);
          LRect:=Rect(0, 0, LBitmap.Width, LBitmap.Height );
          case Integer(ImageName) of
           //Magnifier
           34560..34562
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_search, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                        //Bitmap32_SetAlphaByColor(LBitmap, 255, LColor);
                     end;

           //cross button normal
           34569..34571
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clWindowText);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

           //cross button hot
           34575..34577,
           34581..34583
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

           //Right Arrow, cross button, refresh, down arrow
            288
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clBtnText);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        LRect:=Rect(0, 0, 16, 16);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                        OffsetRect(LRect, 16, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);

                        OffsetRect(LRect, 16, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect, LColor);

                        OffsetRect(LRect, 16, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

            //navigation buttons (arrows)
            577..579,
            581
                    :
                     begin
                        case Integer(ImageName) of
                          577 : LColor:= StyleServices.GetSystemColor(clBtnText);
                          578 : LColor:= StyleServices.GetSystemColor(clHighlight);
                          579 : LColor:= StyleServices.GetSystemColor(clGrayText);
                          581 : LColor:= StyleServices.GetSystemColor(clBtnText);
                        end;

                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        LRect:=Rect(0, 0, 27, 27);
                        InflateRect(LRect, -4, -4);

                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_left, LRect, LColor);

                        OffsetRect(LRect, 27 + 4, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

            280     //background navigation buttons
                    :
                     begin
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        LRect:=Rect(0, 8, 12, 20);

                        LColor:= StyleServices.GetSystemColor(clGrayText);
                        OffsetRect(LRect, 58, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        LColor:= StyleServices.GetSystemColor(clBtnText);
                        OffsetRect(LRect, 70, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        OffsetRect(LRect, 70, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        OffsetRect(LRect, 70, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

          else
               begin
                 Bitmap32_Grayscale(LBitmap);
                 _ProcessBitmap32(LBitmap, StyleServices.GetSystemColor(clHighlight), _BlendBurn)
               end;
          end;
        end
        else
        _BlendMultiply32(LBitmap, StyleServices.GetSystemColor(clHighlight));

        LBitmap.ReleaseHandle;
      finally
        LBitmap.Free;
      end;
      //OutputDebugString(PChar(Format('Detour_LoadImageW ImageName %s', [s])));
      Exit(Result);
    end;
  end;

  Result:= TrampolineLoadImageW(hInst, ImageName, ImageType, X, Y, Flags);
end;
{$ENDIF HOOK_UXTHEME}


//dont hook CreateSolidBrush, because is used internally but GetSysColorBrush

initialization

 VCLStylesLock  := TCriticalSection.Create;
 VCLStylesBrush := TObjectDictionary<string, TListStyleBrush>.Create([doOwnsValues]);

if StyleServices.Available then
begin
{$IFDEF HOOK_TDateTimePicker}
  TCustomStyleEngine.RegisterStyleHook(TDateTimePicker, TStyleHook);
{$ENDIF HOOK_TDateTimePicker}
{$IFDEF HOOK_TProgressBar}
  TCustomStyleEngine.RegisterStyleHook(TProgressBar, TStyleHook);
{$ENDIF HOOK_TProgressBar}
  LSetStylePtr := TStyleManager.SetStyle;

  BeginHooks;
  @TrampolineGetSysColor := InterceptCreate(user32, 'GetSysColor', @Detour_GetSysColor);
  @TrampolineGetSysColorBrush := InterceptCreate(user32, 'GetSysColorBrush', @Detour_GetSysColorBrush);
  @TrampolineFillRect := InterceptCreate(user32, 'FillRect', @Detour_FillRect);
  @TrampolineDrawEdge := InterceptCreate(user32, 'DrawEdge', @Detour_DrawEdge);

{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
   @TrampolineLoadImageW := InterceptCreate(user32, 'LoadImageW', @Detour_LoadImageW);

  // @TrampolineCopyImage := InterceptCreate(user32, 'CopyImage', @Detour_CopyImage);

{$ENDIF HOOK_UXTHEME}

  @TrampolineSetStyle := InterceptCreate(@LSetStylePtr, @Detour_SetStyle);
  EndHooks;
end;

finalization

  BeginUnHooks;
  InterceptRemove(@TrampolineGetSysColor);
  InterceptRemove(@TrampolineGetSysColorBrush);
  InterceptRemove(@TrampolineFillRect);
  InterceptRemove(@TrampolineDrawEdge);

{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
    InterceptRemove(@TrampolineLoadImageW);

 // InterceptRemove(@TrampolineCopyImage);
{$ENDIF HOOK_UXTHEME}

  InterceptRemove(@TrampolineSetStyle);
  EndUnHooks;

  VCLStylesBrush.Free;
  VCLStylesLock.Free;
  VCLStylesLock := nil;

end.
