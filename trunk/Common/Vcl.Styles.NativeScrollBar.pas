{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.NativeScrollBar                                                                  }
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
{ The Original Code is uNativeScrollBar.pas                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.NativeScrollBar;
{
  Note: The development of this Unit is not finished yet .
  Only vertical ScrollBar is supported !!.
  You should use uScrollBarWnd unit instead of this,
  until i finished this .
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Styles,
  Vcl.Themes,
  System.Types,
  System.SysUtils,
  Vcl.Styles.ControlWnd,
  Winapi.CommCtrl,
  System.Math;

type
  TScrollingKind = (skUp, skDown, skLeft, skRight);

  TScrollBarWnd = class(TControlWnd)
  private
    FBtnUpDetail: TThemedScrollBar;
    FBtnDownDetail: TThemedScrollBar;
    FThumbVBtnDetail: TThemedScrollBar;
    FThumbVBtnRect: TRect;
    FBtnUpRect: TRect;
    FBtnDownRect: TRect;
    FDown: Boolean;
    FDargScrolling: Boolean;
    FDownPoint: TPoint;
    FPrevPoint: TPoint;
    FDownDis: integer;
    FScrollingKind: TScrollingKind;
  protected
    function GetVertTrackRect: TRect;
    function GetScrollPosFromPoint(P: TPoint): integer;
    function GetPageSize: integer;
    function DoNCCalcSize(Message: TMessage): LRESULT;
    function GetVertScrollRect: TRect;
    function GetScrollInfo: TScrollInfo;
    function GetVertScrollRect2: TRect;
    function GetThumbSize: integer;
    function GetThumbPos: integer;
    function GetScollBarBtnSize: TSize;
    function GetBtnSize: integer;
    procedure PaintScrollBar;
    procedure PaintVScrollBar;
    procedure PaintHScrollBar;
    function GetBtnUpRect: TRect;
    function GetBtnDownRect: TRect;
    function GetThumbRect: TRect;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

const
  SM_SCROLL_BTNUP_DOWN = WM_USER + $70;
  SM_SCROLL_BTNDOWN_DOWN = WM_USER + $72;

function TScrollBarWnd.DoNCCalcSize(Message: TMessage): LRESULT;
var
  OldRect: TRect;
  NewStyle, OldStyle: DWORD;
  lParam: Winapi.Windows.lParam;
begin
  lParam := Message.lParam;
  OldRect := TNCCalcSizeParams(Pointer(lParam)^).rgrc[0];
  OldStyle := Style;
  NewStyle := Style;

  if Style and WS_HSCROLL = WS_HSCROLL then
    NewStyle := Style and not WS_HSCROLL;

  if Style and WS_VSCROLL = WS_VSCROLL then
    NewStyle := Style and not WS_VSCROLL;

  if NewStyle <> Style then
    begin
      SetWindowLong(Handle, GWL_STYLE, NewStyle);
      Result := CallOrgWndProc(Message);
      SetWindowLong(Handle, GWL_STYLE, OldStyle);
    end
  else
    Result := CallOrgWndProc(Message);
  dec(TNCCalcSizeParams(Pointer(lParam)^).rgrc[0].right, GetBtnSize);
  // dec(TNCCalcSizeParams(Pointer(lParam)^).rgrc[0].Bottom, GetBtnSize);

end;

constructor TScrollBarWnd.Create(AHandle: THandle);
begin
  inherited;
  FThumbVBtnDetail := tsThumbBtnVertNormal;
  FBtnUpDetail := tsArrowBtnUpNormal;
  FBtnDownDetail := tsArrowBtnDownNormal;
end;

function TScrollBarWnd.GetVertScrollRect2: TRect;
var
  P: TPoint;
  BarInfo: TScrollBarInfo;
begin
  FillChar(BarInfo, sizeof(TScrollBarInfo), Char(0));
  BarInfo.cbSize := sizeof(BarInfo);
  GetScrollBarInfo(Handle, integer(OBJID_VSCROLL), BarInfo);
  if STATE_SYSTEM_INVISIBLE and BarInfo.rgstate[0] <> 0 then
    Result := Rect(0, 0, 0, 0)
  else
    begin
      P := BarInfo.rcScrollBar.TopLeft;
      ScreenToClient(Handle, P);
      Result.TopLeft := P;
      P := BarInfo.rcScrollBar.BottomRight;
      ScreenToClient(Handle, P);
      Result.BottomRight := P;
      if HasBorder then
        if HasClientEdge then
          OffsetRect(Result, 2, 2)
        else
          OffsetRect(Result, 1, 1);
    end;
end;

procedure TScrollBarWnd.PaintHScrollBar;
begin

end;

procedure TScrollBarWnd.PaintScrollBar;
begin
  PaintVScrollBar;
end;

procedure TScrollBarWnd.PaintVScrollBar;
var
  R, TmpRect: TRect;
  Canvas: TCanvas;
  DC: HDC;
  Bmp: TBItmap;
  LDetails: TThemedElementDetails;
  P: TPoint;
begin
  Canvas := TCanvas.Create;
  try
    DC := GetWindowDC(Handle);
    Canvas.Handle := DC;
    Bmp := TBItmap.Create;
    R := GetVertScrollRect;
    Bmp.SetSize(R.Width, R.Height);

    { ScrollBar Face }
    TmpRect := Rect(0, 0, R.Width, R.Height);
    LDetails := StyleServices.GetElementDetails(tsUpperTrackVertNormal);
    StyleServices.DrawElement(Bmp.Canvas.Handle, LDetails, TmpRect);
    { ScrollBar Up button }
    LDetails := StyleServices.GetElementDetails(FBtnUpDetail);
    TmpRect := GetBtnUpRect;
    TmpRect := Rect(0, 0, TmpRect.Width, TmpRect.Height);
    StyleServices.DrawElement(Bmp.Canvas.Handle, LDetails, TmpRect);
    { ScrollBar Down button }
    LDetails := StyleServices.GetElementDetails(FBtnDownDetail);
    TmpRect := GetBtnDownRect;
    TmpRect := Rect(0, Bmp.Height - TmpRect.Height, TmpRect.Width, Bmp.Height);
    StyleServices.DrawElement(Bmp.Canvas.Handle, LDetails, TmpRect);
    { ScrollBar Thumb button }
    LDetails := StyleServices.GetElementDetails(FThumbVBtnDetail);
    TmpRect := GetThumbRect;
    if TmpRect.Bottom > GetBtnDownRect.Top then
      TmpRect.Bottom := GetBtnDownRect.Top;
    TmpRect := Rect(0, TmpRect.Top, TmpRect.Width, TmpRect.Top + TmpRect.Height);
    if FDargScrolling then
      begin
        GetCursorPos(P);
        ScreenToClient(Handle, P);

        TmpRect := Rect(0, P.Y-FDownDis, TmpRect.Width,
          P.Y-FDownDis  + TmpRect.Height);
      end;
    StyleServices.DrawElement(Bmp.Canvas.Handle, LDetails, TmpRect);

    Canvas.Draw(R.Left, R.Top, Bmp);
    Bmp.Free;
    ReleaseDC(Handle, DC);
  finally
    Canvas.Free;
  end;
end;

function TScrollBarWnd.GetScollBarBtnSize: TSize;
begin
  Result.cx := GetSystemMetrics(SM_CXVSCROLL);
  Result.cy := GetSystemMetrics(SM_CYVSCROLL);
end;

function TScrollBarWnd.GetBtnDownRect: TRect;
var
  R: TRect;
begin
  R := GetVertScrollRect;
  Result := Rect(R.Left, R.Bottom - GetBtnSize, R.right, R.Bottom);
  FBtnDownRect := Result;
end;

function TScrollBarWnd.GetBtnSize: integer;
begin
  Result := GetScollBarBtnSize.cy;
end;

function TScrollBarWnd.GetBtnUpRect: TRect;
var
  R: TRect;
begin
  R := GetVertScrollRect;
  Result := Rect(R.Left, R.Top, R.right, R.Top + GetBtnSize);
  FBtnUpRect := Result;
end;

function TScrollBarWnd.GetVertScrollRect: TRect;
var
  WinRect: TRect;
begin
  GetWindowRect(Handle, WinRect);
  Result := Rect(ClientRect.right, 0, ClientRect.right +
    (WinRect.Width - ClientRect.Width), WinRect.Height);
end;

function TScrollBarWnd.GetPageSize: integer;
begin
  Result := GetVertScrollRect.Height - (2 * GetScollBarBtnSize.cy);
end;

function TScrollBarWnd.GetScrollInfo: TScrollInfo;
begin
  FillChar(Result, sizeof(TScrollInfo), Char(0));
  Result.cbSize := sizeof(TScrollInfo);
  Result.fMask := SIF_ALL;
  Winapi.Windows.GetScrollInfo(Handle, SB_VERT, Result);
end;

var
  nPrevPos: integer = 0;
  nPrevThumb: integer = 0;

function TScrollBarWnd.GetScrollPosFromPoint(P: TPoint): integer;
var
  ThumbSize: integer;
  TrackRect, WorkRect: TRect;
  Pos, siMaxMin: integer;
begin
  TrackRect := GetVertTrackRect;
  ThumbSize := GetThumbSize;
  WorkRect := TrackRect;
  dec(WorkRect.Bottom, ThumbSize);
  Pos := P.Y - FDownDis;

  if Pos < TrackRect.Top then
    Pos := TrackRect.Top;
  if Pos > TrackRect.Bottom - ThumbSize then
    Pos := TrackRect.Bottom - ThumbSize;

  siMaxMin := GetScrollInfo.nMax;
  if Pos - TrackRect.Top = TrackRect.Height - ThumbSize then // If max
    Pos := siMaxMin - integer(GetScrollInfo.nPage) + 2
  else
    Pos := MulDiv(Pos - TrackRect.Top ,
      siMaxMin - integer(GetScrollInfo.nPage) ,
      (WorkRect.Height) - ThumbSize);

  Result := Pos;
end;

function TScrollBarWnd.GetThumbPos: integer;
var
  R: TRect;
  BtnSize: TSize;
  LScrollInfo: TScrollInfo;
  PageSize: integer;
  ThumbSize: integer;
begin
  R := GetVertScrollRect;
  BtnSize := GetScollBarBtnSize;
  LScrollInfo := GetScrollInfo;
  PageSize := R.Height - (BtnSize.cy * 2);
  ThumbSize := GetThumbSize;
  with LScrollInfo do
    Result := MulDiv(nPos - nMin, PageSize - ThumbSize - 1, nMax - Max(1, nPage));
end;

function TScrollBarWnd.GetThumbRect: TRect;
var
  LScrollInfo: TScrollInfo;
  ThumbPos, ThumbSize: integer;
  R: TRect;
begin
  R := GetVertScrollRect;
  LScrollInfo := GetScrollInfo;
  ThumbPos := GetThumbPos - 1;
  if ThumbPos < 0 then
    ThumbPos := 0;
  ThumbSize := GetThumbSize;
  Result := Rect(R.Left, R.Top + GetBtnSize + ThumbPos, R.right,
    R.Top + ThumbPos + GetBtnSize + ThumbSize);
  FThumbVBtnRect := Result;
end;

function TScrollBarWnd.GetVertTrackRect: TRect;
begin
  Result := GetVertScrollRect;
  if Result.Width > 0 then
    begin
      Result.Top := Result.Top + GetSystemMetrics(SM_CYVTHUMB);
      Result.Bottom := Result.Bottom - GetSystemMetrics(SM_CYVTHUMB);
    end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetThumbSize: integer;
var
  R: TRect;
  LScrollInfo: TScrollInfo;
  MaxMin: integer;
  BtnSize: TSize;
  PageSize: integer;
begin
  R := GetVertScrollRect;
  BtnSize := GetScollBarBtnSize;
  LScrollInfo := GetScrollInfo;
  PageSize := R.Height - (BtnSize.cy * 2);
  with LScrollInfo do
    begin
      if nMax > nMin then
        MaxMin := nMax - nMin
      else
        MaxMin := nMax;
      Result := MulDiv(nPage, PageSize, MaxMin);
      if Result < BtnSize.cy then
        Result := BtnSize.cy;
    end;
end;

procedure TScrollBarWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  wParam: UINT_PTR;
  lParam: UINT_PTR;
  sColor: TColor;
  P, PP: TPoint;
  DC: HDC;
  RedrawScrollBar: Boolean;
  R, R2: TRect;
  LScrollInfo: TScrollInfo;
  nPos, {nMax, nMaxPos, nPage,} CurrPos{, Y}: integer;
begin

  uMsg := Message.Msg;
  wParam := Message.wParam;
  lParam := Message.lParam;
  case uMsg of

    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCCALCSIZE:
      begin
        Message.Result := DoNCCalcSize(Message);
        exit;
      end;

    WM_VSCROLL, WM_HSCROLL, WM_KEYDOWN:
      begin
        Message.Result := CallOrgWndProc(Message);
        // PaintScrollBar;
      end;

    SM_SCROLL_BTNUP_DOWN:
      begin
        SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
        FBtnUpDetail := tsArrowBtnUpPressed;
        FScrollingKind := skUp;
        SetTimer(Handle, $77, 50, nil);
      end;

    SM_SCROLL_BTNDOWN_DOWN:
      begin
        SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
        FBtnDownDetail := tsArrowBtnDownPressed;
        FScrollingKind := skDown;
        SetTimer(Handle, $77, 50, nil);
      end;

    WM_NCLBUTTONDOWN:
      begin

        FPrevPoint := Point(0, 0);
        if (wParam = HTHSCROLL) or (wParam = HTVSCROLL) then
          begin
            FDargScrolling := False;
            FDown := True;
            P.x := GET_X_LPARAM(lParam);
            P.Y := GET_Y_LPARAM(lParam);
            ScreenToClient(Handle, P);
            R := GetVertScrollRect;
            if R.Contains(P) then
              begin
                if (not FThumbVBtnRect.Contains(P)) and
                  (not FBtnUpRect.Contains(P)) and (not FBtnDownRect.Contains(P))
                then
                  begin
                    if P.Y > FThumbVBtnRect.Top then
                      FScrollingKind := skDown
                    else
                      FScrollingKind := skUp;
                 //   SetTimer(Handle, $77, 50, nil);
                  end;
              end;

            //if FBtnUpRect.Contains(P) then
            //  SendMessage(Handle, SM_SCROLL_BTNUP_DOWN, wParam, lParam);

            //if FBtnDownRect.Contains(P) then
              //SendMessage(Handle, SM_SCROLL_BTNDOWN_DOWN, wParam, lParam);

            if FThumbVBtnRect.Contains(P) then
              begin
                FThumbVBtnDetail := tsThumbBtnVertPressed;
                KillTimer(Handle, $77);

                FDownDis := P.Y -  GetThumbPos;
            //    FDargScrolling := True;
                FDownPoint := P;
                GetCursorPos(PP);
                SetTimer(Handle, $72, 10, nil);
              end;

            InvalidateNC;
            Message.Result := CallOrgWndProc(Message);
            FBtnUpDetail := tsArrowBtnUpNormal;
            FBtnDownDetail := tsArrowBtnDownNormal;
            FThumbVBtnDetail := tsThumbBtnVertNormal;
            KillTimer(Handle, $77);
            KillTimer(Handle, $72);
            FDargScrolling := False;
            FDown := False;
            InvalidateNC;
          end
        else
          Message.Result := CallOrgWndProc(Message);
      end;

    WM_TIMER:
      begin
        if wParam = $77 then
          begin
            if FDown then
              begin
                GetCursorPos(P);
                ScreenToClient(Handle, P);
                R := FThumbVBtnRect;
                if not R.Contains(P) then
                  begin
                    case FScrollingKind of
                      skUp:
                        SendMessage(Handle, WM_VSCROLL, SB_LINEUP, 0);
                      skDown:
                        SendMessage(Handle, WM_VSCROLL, SB_LINEDOWN, 0);
                    end;
                  end;
              end;
          end
        else if wParam = $72 then
          begin
            if FDown then
              begin
                GetCursorPos(P);
                ScreenToClient(Handle, P);
                LScrollInfo := GetScrollInfo;
                //nMax := LScrollInfo.nMax;
                //nPage := LScrollInfo.nPage;
                nPos := LScrollInfo.nPos;
                nPrevPos := nPos;
                R := GetVertScrollRect;
                if (P.Y <> FDownPoint.Y) and (FPrevPoint.Y <> P.Y) then
                  begin
                    R := GetVertTrackRect;
                    R2 := GetVertScrollRect;
                    //nMaxPos := nMax - (nPage - 1);
                    //Y := P.Y - GetBtnSize; // -FDownDis;
                    //nPos := (nMaxPos * Y) div (R.Height - GetThumbRect.Height);
                    //nPos := MulDiv(nMaxPos, Y, R.Height - GetThumbRect.Height);
                    //nPos := (Y) * nMaxPos div (GetVertTrackRect.Height - GetThumbRect.Height);
                    PP:=P;

                    nPos := GetScrollPosFromPoint(PP);
                    nPos := nPos - nPrevPos;

                    CurrPos := GetScrollPos(Handle, SB_VERT);
                    CurrPos := CurrPos + nPos;

                    if ClassNameNative = 'SysListView32' then
                      begin
                        if ListView_GetView(Handle) = LVS_REPORT then
                          begin
                            R := TRect.Empty;
                            ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
                            nPos := nPos * R.Height;
                          end;

                        ListView_Scroll(Handle, 0, nPos);
                      end
                    else
                      begin
                        SetScrollPos(Handle, SB_VERT, CurrPos, False);
                        SendMessage(Handle, WM_VSCROLL,
                          MakeWParam(SB_THUMBPOSITION, CurrPos), 0);
                      end;

                    nPrevPos := GetScrollPos(Handle, SB_VERT);

                    FPrevPoint := P;
                    // if P.Y<GetBtnDownRect.Top then
                   // if nPrevPos <= nMaxPos then
                      FDargScrolling := True;

                      PaintScrollBar;

                    nPrevThumb := GetThumbRect.Top;

                  end;

              end;
          end
        else
          Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCMOUSELEAVE:
      begin
        if (FBtnUpDetail <> tsArrowBtnUpNormal) or
          (FBtnDownDetail <> tsArrowBtnDownNormal) or
          (FThumbVBtnDetail <> tsThumbBtnVertNormal) then
          begin
            if FThumbVBtnDetail <> tsThumbBtnVertPressed then
              FThumbVBtnDetail := tsThumbBtnVertNormal;
            if FBtnUpDetail <> tsArrowBtnUpPressed then
              FBtnUpDetail := tsArrowBtnUpNormal;
            if FBtnDownDetail <> tsArrowBtnDownPressed then
              FBtnDownDetail := tsArrowBtnDownNormal;
            InvalidateNC;
          end;
        Message.Result := CallOrgWndProc(Message);

      end;

    WM_NCHITTEST:
      begin
        P.x := GET_X_LPARAM(lParam);
        P.Y := GET_Y_LPARAM(lParam);
        ScreenToClient(Handle, P);
        RedrawScrollBar := False;
        if GetVertScrollRect.Contains(P) then
          begin

            if FThumbVBtnRect.Contains(P) then
              begin
                if FThumbVBtnDetail <> tsThumbBtnVertHot then
                  begin
                    FThumbVBtnDetail := tsThumbBtnVertHot;
                    // InvalidateNC;
                    RedrawScrollBar := True;

                  end;
              end
            else
              begin
                if (FThumbVBtnDetail = tsThumbBtnVertHot) then
                  begin
                    FThumbVBtnDetail := tsThumbBtnVertNormal;
                    // InvalidateNC;
                    RedrawScrollBar := True;
                  end;
              end;
            if (FBtnUpRect.Contains(P)) then
              begin
                if (FBtnUpDetail <> tsArrowBtnUpHot) then
                  begin
                    FThumbVBtnDetail := tsThumbBtnVertNormal;
                    FBtnUpDetail := tsArrowBtnUpHot;
                    // InvalidateNC;
                    RedrawScrollBar := True;
                  end;
              end
            else
              begin
                if (FBtnUpDetail = tsArrowBtnUpHot) then
                  begin
                    FBtnUpDetail := tsArrowBtnUpNormal;
                    // InvalidateNC;
                    RedrawScrollBar := True;
                  end;
              end;
            if (FBtnDownRect.Contains(P)) then
              begin
                if (FBtnDownDetail <> tsArrowBtnDownHot) then
                  begin
                    FThumbVBtnDetail := tsThumbBtnVertNormal;
                    FBtnDownDetail := tsArrowBtnDownHot;
                    // InvalidateNC;
                    RedrawScrollBar := True;
                  end;
              end
            else
              begin
                if (FBtnDownDetail = tsArrowBtnDownHot) then
                  begin
                    FBtnDownDetail := tsArrowBtnDownNormal;
                    // InvalidateNC;
                    RedrawScrollBar := True;
                  end;
              end;

            if RedrawScrollBar then
              InvalidateNC;
          end;
        Message.Result := CallOrgWndProc(Message);

      end;

    WM_NCPAINT, WM_PAINT:
      begin
        Message.Result := CallOrgWndProc(Message);
        if GetScrollInfo.nPage > 0 then
          PaintScrollBar
        else
          begin
            DC := GetWindowDC(Handle);
            try
              sColor := StyleServices.GetStyleColor(scWindow);
              FillRectangle(DC, GetVertScrollRect, sColor);
            finally
             ReleaseDC(Handle, DC);
            end;
          end;
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
