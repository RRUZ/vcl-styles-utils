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
  TListStyleBrush  = class(TDictionary<Integer, HBRUSH>)
  protected
    procedure ValueNotify(const Value: HBRUSH; Action: TCollectionNotification); override;
  end;

  TSetStyle = procedure(Style: TCustomStyleServices) of object;

var
  VCLStylesBrush: TObjectDictionary<string, TListStyleBrush>;
  VCLStylesLock: TCriticalSection = nil;
  LSetStylePtr: TSetStyle;

  Trampoline_FillRect  : function(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
  Trampoline_DrawEdge  : function(hDC: hDC; var qrc: TRect; edge: UINT; grfFlags: UINT): BOOL;  stdcall = nil;
  Trampoline_SetStyle  : procedure(Self: TObject; Style: TCustomStyleServices);
  Trampoline_DrawFrameControl : function (DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall = nil;
  {$IFDEF HOOK_UXTHEME}
  TrampolineLoadImageW: function (hInst: HINST; ImageName: LPCWSTR; ImageType: UINT; X, Y: Integer; Flags: UINT): THandle; stdcall = nil;
  {$ENDIF HOOK_UXTHEME}

  Trampoline_DrawTextW  : function (hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;

function  Detour_DrawTextW(hDC: HDC; lpString: LPCWSTR; nCount: Integer;  var lpRect: TRect; uFormat: UINT): Integer; stdcall;
begin
//  if (uFormat AND DT_CALCRECT = 0) then
//  begin
//    OutputDebugString(PChar(Format('Detour_DrawTextW Text "%s"', [lpString])));
//  end;

  Result:= Trampoline_DrawTextW(hDC, lpString, nCount, lpRect, uFormat);
end;



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
  Exit(Trampoline_DrawEdge(hDC, qrc, edge, grfFlags));
end;

function Detour_FillRect(hDC: hDC; const lprc: TRect; hbr: HBRUSH): Integer; stdcall;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(Trampoline_FillRect(hDC, lprc, hbr))
  else if (hbr > 0) and (hbr < COLOR_ENDCOLORS + 1) then
    Exit(Trampoline_FillRect(hDC, lprc, GetSysColorBrush(hbr - 1)))
  else
    Exit(Trampoline_FillRect(hDC, lprc, hbr));
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
  Trampoline_SetStyle(Self, Style);
  if (Style <> LActiveStyle) then
  begin
    for I := 0 to Screen.FormCount - 1 do
      if Screen.Forms[I].HandleAllocated then
        SendMessage(Screen.Forms[I].Handle, WM_SYSCOLORCHANGE, 0, 0);
  end;
end;

//based on JvThemes.DrawThemedFrameControl
function Detour_WinApi_DrawFrameControl(DC: HDC; Rect: PRect; uType, uState: UINT): BOOL; stdcall;
const
  Mask = $00FF;
var
  LRect: TRect;
  LDetails: TThemedElementDetails;
  CanDraw : Boolean;

  LThemedButton: TThemedButton;
  LThemedComboBox: TThemedComboBox;
  LThemedScrollBar: TThemedScrollBar;
begin
  Result := False;
  CanDraw:= (not StyleServices.IsSystemStyle) and (TSysStyleManager.Enabled) and (Rect<>nil);
  if CanDraw then
  begin
    LRect := Rect^;
    case uType of
      DFC_BUTTON:
        case uState and Mask of

          DFCS_BUTTONPUSH:
            begin
              if uState and (DFCS_TRANSPARENT or DFCS_FLAT) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbPushButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbPushButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbPushButtonHot
                else
                if uState and DFCS_MONO <> 0 then
                  LThemedButton := tbPushButtonDefaulted
                else
                  LThemedButton := tbPushButtonNormal;

                LDetails := StyleServices.GetElementDetails(LThemedButton);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;
            end;

          DFCS_BUTTONCHECK:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxCheckedHot
                else
                  LThemedButton := tbCheckBoxCheckedNormal;
              end
              else
              if uState and DFCS_MONO <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxMixedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxMixedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxMixedHot
                else
                  LThemedButton := tbCheckBoxMixedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbCheckBoxUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbCheckBoxUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbCheckBoxUncheckedHot
                else
                  LThemedButton := tbCheckBoxUncheckedNormal;
              end;
              LDetails := StyleServices.GetElementDetails(LThemedButton);
              StyleServices.DrawElement(DC, LDetails, LRect);
              Result := True;
            end;

          DFCS_BUTTONRADIO:
            begin
              if uState and DFCS_CHECKED <> 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbRadioButtonCheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbRadioButtonCheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbRadioButtonCheckedHot
                else
                  LThemedButton := tbRadioButtonCheckedNormal;
              end
              else
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedButton := tbRadioButtonUncheckedDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedButton := tbRadioButtonUncheckedPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedButton := tbRadioButtonUncheckedHot
                else
                  LThemedButton := tbRadioButtonUncheckedNormal;
              end;
              LDetails := StyleServices.GetElementDetails(LThemedButton);
              StyleServices.DrawElement(DC, LDetails, LRect);
              Result := True;
            end;
        end;

      DFC_SCROLL:
        begin
          case uState and Mask of

            DFCS_SCROLLCOMBOBOX:
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedComboBox := tcDropDownButtonDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedComboBox := tcDropDownButtonPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedComboBox := tcDropDownButtonHot
                else
                  LThemedComboBox := tcDropDownButtonNormal;

                LDetails := StyleServices.GetElementDetails(LThemedComboBox);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLUP:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnUpDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnUpPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnUpHot
                else
                  LThemedScrollBar := tsArrowBtnUpNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLDOWN:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnDownDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnDownPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnDownHot
                else
                  LThemedScrollBar := tsArrowBtnDownNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLLEFT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnLeftHot
                else
                  LThemedScrollBar := tsArrowBtnLeftNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

            DFCS_SCROLLRIGHT:
              if uState and (DFCS_TRANSPARENT {or DFCS_FLAT}) = 0 then
              begin
                if uState and DFCS_INACTIVE <> 0 then
                  LThemedScrollBar := tsArrowBtnRightDisabled
                else
                if uState and DFCS_PUSHED <> 0 then
                  LThemedScrollBar := tsArrowBtnRightPressed
                else
                if uState and DFCS_HOT <> 0 then
                  LThemedScrollBar := tsArrowBtnRightHot
                else
                  LThemedScrollBar := tsArrowBtnRightNormal;

                LDetails := StyleServices.GetElementDetails(LThemedScrollBar);
                StyleServices.DrawElement(DC, LDetails, LRect);
                Result := True;
              end;

          end;
        end;
    end;
  end;

  if not Result then
    Exit(Trampoline_DrawFrameControl(DC, Rect, uType, uState));
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
  s : string;
  LRect, LRect2 : TRect;
  LBackColor, LColor : TColor;
//  LIcon : TIcon;
begin
  if StyleServices.IsSystemStyle or not TSysStyleManager.Enabled then
    Exit(TrampolineLoadImageW(hInst, ImageName, ImageType, X, Y, Flags));
                                                                                                                         //w8 - W10
  if (hInst>0) and (hInst<>HInstance) and (ImageType=IMAGE_ICON) and (X=16) and (Y=16) and IS_INTRESOURCE(ImageName) and TOSVersion.Check(6, 2) then
  begin
    s := IntToStr(Integer(ImageName));

//    LIcon:=TIcon.Create;
//    try
//     LIcon.Handle:=Result;
//     try
//     LIcon.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+s+'.ico');
//       except
//
//     end;
//    finally
//      LIcon.Free;
//    end;

     case Integer(ImageName) of
        //W8, W10
        //comctl32.dll
        16817
             :
              begin
                Exit(AwesomeFont.GetIcon(fa_arrow_up, X, Y, StyleServices.GetSystemColor(clBtnText), StyleServices.GetSystemColor(clBtnFace), 0));
              end;
        16818
             :
              begin
                Exit(AwesomeFont.GetIcon(fa_arrow_up, X, Y, StyleServices.GetSystemColor(clGrayText), StyleServices.GetSystemColor(clBtnFace), 0));
              end;
        //W10
        //shell32.dll
        5100
             :
              begin
                //OutputDebugString(PChar('GetModuleName '+GetModuleName(hInst)));
                Exit(AwesomeFont.GetIcon(fa_check_square_o, 16, 16, StyleServices.GetSystemColor(clWindowText), StyleServices.GetSystemColor(clWindow), 0));
              end;
     end;

    Exit(TrampolineLoadImageW(hInst, ImageName, ImageType, X, Y, Flags));
  end
  else
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
        //LBitmap.SaveToFile(IncludeTrailingPathDelimiter(ExtractFilePath(ParamStr(0)))+s+'.bmp');//SaveToFile('C:\Users\Rodrigo\Desktop\vcl-styles-utils\Vcl Styles Utils New Dialogs (Demo App)\Win32\Debug\Images\'+s+'.bmp');

        //W8 - W10
        if TOSVersion.Check(6, 2) then
        begin
          LBackColor:= StyleServices.GetSystemColor(clWindow);
          LRect:=Rect(0, 0, LBitmap.Width, LBitmap.Height );
           case Integer(ImageName) of
             // Right Arrow, cross button, refresh, down arrow
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
                          LRect2:=LRect;
                          InflateRect(LRect2, -2, -2);
                          AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect2, LColor);

                          OffsetRect(LRect, 16 + 2, 0);
                          LRect2:=LRect;
                          InflateRect(LRect2, -2, -2);
                          AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect2, LColor);

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
        //Windows Vista - W7
        if (TOSVersion.Major=6) and ((TOSVersion.Minor=0) or (TOSVersion.Minor=1)) then
        begin
          LBackColor:= StyleServices.GetSystemColor(clWindow);
          LRect:=Rect(0, 0, LBitmap.Width, LBitmap.Height );
          case Integer(ImageName) of
           //Magnifier
           34560..34562,  // Aero Enabled
           34563..34568   // Classic Theme

                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_search, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                        //Bitmap32_SetAlphaByColor(LBitmap, 255, LColor);
                     end;

           //cross button normal
           34569..34571,  // Aero Enabled
           34572..34574   // Classic Theme
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clWindowText);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

           //cross button hot
           34575..34577, // Aero Enabled
           34581..34583, // Aero Enabled
           34578..34580  // Classic Theme
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clHighlight);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);
                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

           // Aero Enabled
           // Right Arrow, cross button, refresh, down arrow
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

            // Classic Theme
            // Right Arrow, cross button, refresh, down arrow
            289, 290
                    :
                     begin
                        LColor:= StyleServices.GetSystemColor(clBtnText);
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        LRect:=Rect(0, 0, 21, 21);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                        OffsetRect(LRect, 21, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_remove, LRect, LColor);

                        OffsetRect(LRect, 21, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_refresh, LRect, LColor);

                        OffsetRect(LRect, 21, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);

                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

            // Aero Enabled
            // navigation buttons (arrows)
            577..579,
            581
                    :
                     begin

                        case Integer(ImageName) of
                          577 : LColor:= StyleServices.GetSystemColor(clBtnText);
                          578 : LColor:= StyleServices.GetSystemColor(clHighlight);
                          579 : LColor:= StyleServices.GetSystemColor(clGrayText);
                          581 : LColor:= StyleServices.GetSystemColor(clBtnText);
                          else
                                LColor:= StyleServices.GetSystemColor(clBtnText);
                        end;

                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        LRect:=Rect(0, 0, 27, 27);
                        InflateRect(LRect, -4, -4);

                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_left, LRect, LColor);

                        OffsetRect(LRect, 27 + 4, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;

            //Classic Theme
            // navigation buttons (arrows)
            582..584
                    :
                     begin
                        case Integer(ImageName) of
                          582 : LColor:= StyleServices.GetSystemColor(clBtnText);
                          583 : LColor:= StyleServices.GetSystemColor(clHighlight);
                          584 : LColor:= StyleServices.GetSystemColor(clGrayText);
                        end;

                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);

                        //left arrow
                        LRect:=Rect(0, 0, 25, 25);
                        InflateRect(LRect, -4, -4);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_left, LRect, LColor);

                        //right arrow
                        OffsetRect(LRect, 25 + 4, 0);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_arrow_right, LRect, LColor);

                        //dropdown arrow
                        LRect:=Rect(60, 8, 72, 20);
                        AwesomeFont.DrawChar(LBitmap.Canvas.Handle, fa_caret_down, LRect, LColor);


                        Bitmap32_SetAlphaExceptColor(LBitmap, 255, LBackColor);
                     end;


            //Aero Enabled
            //background navigation buttons
            280
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

            //Classic Theme
            //background navigation buttons
            281
                    :
                     begin
                        Bitmap32_SetAlphaAndColor(LBitmap, 1, LBackColor);
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

{ TListStyleBrush }

//Delete brushes
procedure TListStyleBrush.ValueNotify(const Value: HBRUSH; Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnRemoved then
    DeleteObject(Value);
end;

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
  @Trampoline_FillRect := InterceptCreate(user32, 'FillRect', @Detour_FillRect);
  @Trampoline_DrawEdge := InterceptCreate(user32, 'DrawEdge', @Detour_DrawEdge);
  @Trampoline_DrawFrameControl :=  InterceptCreate(user32, 'DrawFrameControl', @Detour_WinApi_DrawFrameControl);
  //@Trampoline_DrawTextW :=  InterceptCreate(user32, 'DrawTextW', @Detour_DrawTextW);
{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
   @TrampolineLoadImageW := InterceptCreate(user32, 'LoadImageW', @Detour_LoadImageW);
  // @TrampolineCopyImage := InterceptCreate(user32, 'CopyImage', @Detour_CopyImage);
{$ENDIF HOOK_UXTHEME}

  @Trampoline_SetStyle := InterceptCreate(@LSetStylePtr, @Detour_SetStyle);
  EndHooks;
end;

finalization

  BeginUnHooks;
  InterceptRemove(@TrampolineGetSysColor);
  InterceptRemove(@TrampolineGetSysColorBrush);
  InterceptRemove(@Trampoline_FillRect);
  InterceptRemove(@Trampoline_DrawEdge);
  InterceptRemove(@Trampoline_DrawFrameControl);
  //InterceptRemove(@Trampoline_DrawTextW);

{$IFDEF HOOK_UXTHEME}
  if TOSVersion.Check(6) then
    InterceptRemove(@TrampolineLoadImageW);
{$ENDIF HOOK_UXTHEME}
  InterceptRemove(@Trampoline_SetStyle);
  EndUnHooks;
  VCLStylesBrush.Free;
  VCLStylesLock.Free;
  VCLStylesLock := nil;

end.
