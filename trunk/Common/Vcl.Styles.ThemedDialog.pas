{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ThemedDialog                                                                     }
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
{ The Original Code is uThemedDialog.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ThemedDialog;
{ +BugFix : Clicking on CloseButton does not close the Window }
{
  Update (13/10/2013) :
  +BugFix:Window size is smaller than the original size .
  +Main Icon is painted .
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles.ControlWnd,
  Vcl.Styles.ButtonWnd,
  Vcl.Styles.ComboBoxWnd;

type
  TDialogWnd = class(TControlWnd)
  private
    FFrameActive: Boolean;
    SysBtnState: TSysButtonsStates;
    FRegion: HRGN;
    FSizeAligned: Boolean;
    FSizeAlignedMsg: Boolean;
    FChangWidth: Integer;
    SysCloseBtnRect: TRect;
    StaticBrush: HBRUSH;
    EditBrush: HBRUSH;
  private
    procedure DrawCaptionText(DC: HDC; CaptionRect: TRect;
      FrameState: TFrameState);
    procedure DrawWindowFrame(hWin: HWND; DC, CaptionBmpDC: HDC; R: TRect;
      FrameState: TFrameState; BtnState: TSysButtonsStates);
    procedure PaintNC(hWin: HWND; FrameState: TFrameState;
      BtnState: TSysButtonsStates);
    procedure PaintBkgnd(DC: HDC; R: TRect; FrameState: TFrameState);
    procedure DrawSysCloseButton(DC: HDC; CaptionRect: TRect;
      BtnState: TSysButtonsStates);
    function GetRegion: HRGN;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

implementation


{ TDialogWnd }
constructor TDialogWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  SetRectEmpty(SysCloseBtnRect);
  { Default System Close Button State }
  SysBtnState := [bsCloseNormal];
  FRegion := 0;
  EditBrush := 0;
  StaticBrush := 0;
  FChangWidth := 0;
  FSizeAligned := False;
  FSizeAlignedMsg := False;
end;

destructor TDialogWnd.Destroy;
begin
  inherited;
  DeleteObject(StaticBrush);
  DeleteObject(FRegion);
  DeleteObject(EditBrush);
  FRegion := 0;
  EditBrush := 0;
  StaticBrush := 0;
end;

procedure TDialogWnd.DrawSysCloseButton(DC: HDC; CaptionRect: TRect;
  BtnState: TSysButtonsStates);
var
  LDetails: TThemedElementDetails;
  R: TRect;
begin
  if IsWindowMsgBox(Handle) then
    begin
      BtnState := [];
      Include(BtnState, bsCloseDisabled);
    end;

  if bsCloseNormal in BtnState then
    LDetails := StyleServices.GetElementDetails(TThemedWindow.twCloseButtonNormal)
  else if bsCloseHot in BtnState then
    LDetails := StyleServices.GetElementDetails(TThemedWindow.twCloseButtonHot)
  else if bsCloseDisabled in BtnState then
    LDetails := StyleServices.GetElementDetails(TThemedWindow.twCloseButtonDisabled)
  else if bsClosePushed in BtnState then
    LDetails := StyleServices.GetElementDetails(TThemedWindow.twCloseButtonPushed);

  StyleServices.GetElementContentRect(0, LDetails, CaptionRect, R);
  SysCloseBtnRect := R;
  StyleServices.DrawElement(DC, LDetails, R);
end;

procedure TDialogWnd.DrawWindowFrame(hWin: HWND; DC, CaptionBmpDC: HDC;
  R: TRect; FrameState: TFrameState; BtnState: TSysButtonsStates);
var
  LDstRect: TRect;
  Details: TThemedElementDetails;
  ElementSize: TSize;
  CaptionHeight: Integer;
begin
  Details := StyleServices.GetElementDetails(twCaptionActive);
  StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
  CaptionHeight := ElementSize.Height;

  if FrameState = fActive then
    begin
      { Draw Active Caption Bar }
      Details := StyleServices.GetElementDetails(twCaptionActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
      CaptionHeight := ElementSize.Height;
      LDstRect := Rect(0, 0, R.Width, ElementSize.Height);
      StyleServices.DrawElement(CaptionBmpDC, Details, LDstRect);
      { Draw Active Left Border }
      Details := StyleServices.GetElementDetails(twFrameLeftActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
      LDstRect := Rect(0, CaptionHeight, ElementSize.Width, R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);

      { Draw Active Right Border }
      Details := StyleServices.GetElementDetails(twFrameRightActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);

      LDstRect := Rect(R.Width - ElementSize.Width, CaptionHeight, R.Width,
        R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);

      { Draw Active Bottom Border }
      Details := StyleServices.GetElementDetails(twFrameBottomActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);

      LDstRect := Rect(0, R.Height - ElementSize.Height, R.Width, R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);
    end
  else if FrameState = fInActive then
    begin
      { Draw InActive Caption Bar }
      Details := StyleServices.GetElementDetails(twCaptionInActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
      CaptionHeight := ElementSize.Height;
      LDstRect := Rect(0, 0, R.Width, ElementSize.Height);
      StyleServices.DrawElement(CaptionBmpDC, Details, LDstRect);

      { Draw InActive Left Border }
      Details := StyleServices.GetElementDetails(twFrameLeftInActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
      LDstRect := Rect(0, CaptionHeight, ElementSize.Width, R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);

      { Draw InActive Right Border }
      Details := StyleServices.GetElementDetails(twFrameRightInActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);

      LDstRect := Rect(R.Width - ElementSize.Width, CaptionHeight, R.Width,
        R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);

      { Draw InActive Bottom Border }
      Details := StyleServices.GetElementDetails(twFrameBottomInActive);
      StyleServices.GetElementSize(DC, Details, esActual, ElementSize);

      LDstRect := Rect(0, R.Height - ElementSize.Height, R.Width, R.Height);
      StyleServices.DrawElement(DC, Details, LDstRect);
    end;

  { Draw System Close Button }
  DrawSysCloseButton(CaptionBmpDC, Rect(0, 0, R.Width, CaptionHeight), BtnState);
end;

procedure TDialogWnd.PaintBkgnd(DC: HDC; R: TRect; FrameState: TFrameState);
var
  LDetails: TThemedElementDetails;
  ThemeElement: TThemedElement;
begin
  ThemeElement := TThemedElement.teWindow;
  LDetails := TThemedElementDetails.Create(ThemeElement, 0,
    Integer(FrameState));

  StyleServices.DrawElement(DC, LDetails, R);
end;

procedure TDialogWnd.DrawCaptionText(DC: HDC; CaptionRect: TRect;
  FrameState: TFrameState);
var
  LDetails: TThemedElementDetails;
begin
  SetBkMode(DC, TRANSPARENT);
  if FrameState = fActive then
    LDetails := StyleServices.GetElementDetails(TThemedWindow.twCaptionActive)
  else
    LDetails := StyleServices.GetElementDetails
      (TThemedWindow.twCaptionInActive);
  inc(CaptionRect.Left, 10);
  { Use StyleServices.DrawText to draw Text
    StyleServices.DrawText will automatically adjust Text Position .
  }
  StyleServices.DrawText(DC, LDetails, Text, CaptionRect,
    [tfVerticalCenter, tfSingleLine]);

end;

procedure TDialogWnd.PaintNC(hWin: HWND; FrameState: TFrameState;
  BtnState: TSysButtonsStates);
var
  DC: HDC;
  R, CaptionRect, IconRect: TRect;
  Icon: HICON;
  CaptionBmp: HBITMAP;
  CaptionBmpDC: HDC;
  IconSize: TIconSize;
  CaptionHeight: Integer;
  Details: TThemedElementDetails;
  ElementSize: TSize;
begin
  DC := GetWindowDC(hWin);
  GetWindowRect(hWin, R);

  { In order to reduce Graphics Flicker..we should draw first into a clean Bitmap
    than copy this Bitmap to our target DC !!
  }

  Details := StyleServices.GetElementDetails(twCaptionActive);
  StyleServices.GetElementSize(DC, Details, esActual, ElementSize);
  CaptionHeight := ElementSize.Height;

  CaptionBmp := CreateBmp(R.Width, CaptionHeight);
  CaptionBmpDC := GetBmpDc(CaptionBmp);
  { Draw Caption Bar & Window Border }
  DrawWindowFrame(hWin, DC, CaptionBmpDC, R, FrameState, BtnState);
  CaptionRect := Rect(0, 0, R.Width, CaptionHeight);

  { Get the Default Caption Icon }
  Icon := GetWindowIcon(hWin);
  (* if Icon = 0 then
    Icon := LoadIcon(0, IDI_APPLICATION);
  *)
  { Draw Icon }

  { Use DrawIconEX to draw Icon .. }
  IconSize := GetIconSize(Icon);
  IconRect := Rect(0, 0, IconSize.Width, IconSize.Height);
  IconRect := RectVCenter(IconRect, CaptionRect);
  if Icon <> 0 then
    begin
      DrawIconEx(CaptionBmpDC, 7, IconRect.Top, Icon, 16, 16, 0, 0, DI_NORMAL);
      { Draw Window Caption Text }
      inc(CaptionRect.Left, 16);
      DrawCaptionText(CaptionBmpDC, CaptionRect, FrameState);
      dec(CaptionRect.Left, 16);
    end
  else
    DrawCaptionText(CaptionBmpDC, CaptionRect, FrameState);

  ReleaseBmpDc(CaptionBmpDC);
  { Draw Caption Bar }
  DrawBitmap(DC, CaptionBmp, Point(0, 0), CaptionRect, False, False);

  ReleaseDC(hWin, DC);
  DeleteObject(CaptionBmp);
end;

function TDialogWnd.GetRegion: HRGN;
var
  R: TRect;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := 0;
  if not StyleServices.Available then
    Exit;
  { Get Window Region }
  R := Rect(0, 0, Width, Height);
  Detail := twCaptionActive;
  DeleteObject(FRegion);
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementRegion(Details, R, Result);
end;


procedure TDialogWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  wParam: UINT_PTR;
  lParam: UINT_PTR;
  sColor: TColor;
  LDetails: TThemedElementDetails;
  cSize: TSize;
  CSP: PNCCalcSizeParams;
  P: TPoint;
  PS: TPaintStruct;
  WP: TWindowPlacement;
  CStaticHWND: HWND;
  R: TRect;
  DFBW: Integer;

  procedure Close;
  begin
    SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
  end;

begin
  uMsg := Message.Msg;
  wParam := Message.wParam;
  lParam := Message.lParam;

  case uMsg of

    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        { DFBW =Default Frame Border Width }
        DFBW := GetSystemMetrics(SM_CXFIXEDFRAME);
        inc(DFBW);
        SetWindowPos(Handle, 0, 0, 0, Width + DFBW, Height + DFBW,
          SWP_NOMOVE or SWP_FRAMECHANGED);
      end;

    WM_MOUSEMOVE:
      begin
        { if mouse is not on a child control then
          send WM_MOUSELEAVE message to the child control .
        }
        SendMessage(CurrentBtn, WM_MOUSELEAVE, 0, 0);
        SendMessage(CurrentCB, WM_MOUSELEAVE, 0, 0);
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCHITTEST:
      begin
        P.X := GET_X_LPARAM(lParam);
        P.Y := GET_Y_LPARAM(lParam);
        P := NormalizePoint(Handle, P);
        Message.Result := CallOrgWndProc(Message);
        if Message.Result = HTCLOSE then
          begin
            Message.Result := HTCAPTION;
          end;
        if SysCloseBtnRect.Contains(P) then
          begin
            { Mouse is on SysClose Button }
            Message.Result := HTCLOSE;
            if not(bsCloseHot in SysBtnState) then
              begin
                SysBtnState := [bsCloseHot];
                InvalidateNC;
              end;
          end
        else if Message.Result <> HTCLOSE then
          begin
            { Change SysClose Button State (Act or InAct) }
            if not(bsCloseNormal in SysBtnState) and
              not(bsCloseDisabled in SysBtnState) then
              begin
                if FFrameActive then
                  SysBtnState := [bsCloseNormal]
                else
                  SysBtnState := [bsCloseDisabled];
                InvalidateNC;
              end;
          end;
      end;

    WM_NCLBUTTONDOWN:
      begin
        P.X := GET_X_LPARAM(lParam);
        P.Y := GET_Y_LPARAM(lParam);
        P := NormalizePoint(Handle, P);
        if (wParam <> HTCLOSE) then
          begin
            Message.Result := CallOrgWndProc(Message);
          end
        else
          begin
            { Redraw Only if the window need to be RePainted. }
            if not(bsClosePushed in SysBtnState) then
              begin
                SysBtnState := [bsClosePushed];
                InvalidateNC;
                SetRedraw(False);
                Message.Result := CallOrgWndProc(Message);
                SetRedraw(True);
                SysBtnState := [bsCloseNormal];
                InvalidateNC;
                if SysCloseBtnRect.Contains(P) then
                  Close;
              end;
          end;
      end;

    WM_NCLBUTTONUP:
      begin
        if (wParam = HTCLOSE) then
          Close
        else
          Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCCALCSIZE:
      begin
        CSP := PNCCalcSizeParams(lParam);

        with CSP^.rgrc[0] do
          begin

            LDetails := StyleServices.GetElementDetails
              (TThemedWindow.twCaptionActive);
            StyleServices.GetElementSize(0, LDetails, esActual, cSize);
            inc(Top, cSize.Height);
            LDetails := StyleServices.GetElementDetails
              (TThemedWindow.twFrameLeftActive);
            StyleServices.GetElementSize(0, LDetails, esActual, cSize);
            inc(Left, cSize.Width);
            LDetails := StyleServices.GetElementDetails
              (TThemedWindow.twFrameRightActive);
            StyleServices.GetElementSize(0, LDetails, esActual, cSize);
            dec(Right, cSize.Width);
            LDetails := StyleServices.GetElementDetails
              (TThemedWindow.twFrameBottomActive);
            StyleServices.GetElementSize(0, LDetails, esActual, cSize);
            dec(Bottom, cSize.Height);

          end;
        Message.Result := 0;

      end;

    WM_NCACTIVATE:
      begin
        FFrameActive := Boolean(wParam);
        if FFrameActive then
          SysBtnState := [bsCloseNormal]
        else
          SysBtnState := [bsCloseDisabled];
        InvalidateNC;
        Message.Result := 1;
      end;

    WM_ERASEBKGND:
      begin
        PaintBkgnd(wParam, ClientRect, TFrameState(FFrameActive));
        Message.Result := 1;
      end;

    WM_PAINT:
      begin

        if IsWindowMsgBox(Handle) then
          { if Window is MessageBox }
          begin
            BeginPaint(Handle, PS);
            PaintBkgnd(PS.HDC, ClientRect, TFrameState(FFrameActive));
            EndPaint(Handle, PS);
            Message.Result := 0;
          end
        else { not MessageBox Window }
          begin
            Message.Result := CallOrgWndProc(Message);
          end;
      end;

    WM_SIZE:
      begin
        Message.Result := CallOrgWndProc(Message);
        FRegion := GetRegion;
        SetWindowRgn(Handle, FRegion, True);

      end;

    WM_GETMINMAXINFO:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_LBUTTONDOWN:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_LBUTTONUP:
      begin
        Message.Result := CallOrgWndProc(Message);
        if IsWindowColorDialog(Handle) then
          begin
            FillChar(WP, SizeOf(WP), Char(0));
            CStaticHWND := GetDlgItem(Handle, $000002BE);
            GetWindowPlacement(CStaticHWND, WP);
            GetCursorPos(P);
            GetWindowRect(CStaticHWND, R);
            if R.Contains(P) then
              InvalidateRect(Handle, Rect(WP.rcNormalPosition.Right,
                WP.rcNormalPosition.Top - 5, ClientRect.Right,
                WP.rcNormalPosition.Bottom + 10), True);
          end;
      end;

    WM_NCPAINT:
      begin
        { Paint the non client area (Caption Bar & Borders) }
        if FFrameActive then
          PaintNC(Handle, fActive, SysBtnState)
        else
          begin
            PaintNC(Handle, fInActive, SysBtnState);
            Message.Result := 1;
          end;
      end;

    WM_CTLCOLOREDIT:
      begin
        // if GetWindowClassName(GetParent(lParam)) <> 'ComboBox' then
        begin
          { Change Edit Control BackGround Color }
          sColor := StyleServices.GetStyleColor(scEdit);
          SetBkColor(wParam, sColor);
          if EditBrush = 0 then
            EditBrush := CreateSolidBrush(sColor);
          Message.Result := EditBrush;

          if IsWindowEnabled(lParam) then
            LDetails := StyleServices.GetElementDetails
              (TThemedEdit.teEditTextNormal)
          else
            LDetails := StyleServices.GetElementDetails
              (TThemedEdit.teEditTextDisabled);

          StyleServices.GetElementColor(LDetails, ecTextColor, sColor);
          { Change Edit Control Text Color }
          SetTextColor(wParam, sColor);

        end
        // else
        // Message.Result := CallOrgWndProc(Message);
      end;

    WM_CTLCOLORSTATIC:
      begin
        sColor := StyleServices.GetStyleColor(scWindow);
        SetBkColor(wParam, sColor);
        if StaticBrush = 0 then
          StaticBrush := CreateSolidBrush(sColor);

        if IsWindowEnabled(lParam) then
          LDetails := StyleServices.GetElementDetails
            (TThemedTextLabel.ttlTextLabelNormal)
        else
          LDetails := StyleServices.GetElementDetails
            (TThemedTextLabel.ttlTextLabelDisabled);

        StyleServices.GetElementColor(LDetails, ecTextColor, sColor);

        { Change Static Control Text Color }
        SetTextColor(wParam, sColor);
        { Transparent Static BackGound }
        Message.Result := StaticBrush;

      end

  else
    Message.Result := CallOrgWndProc(Message);

  end;
end;

end.
