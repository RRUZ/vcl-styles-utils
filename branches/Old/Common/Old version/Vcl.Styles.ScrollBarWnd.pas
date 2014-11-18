{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ScrollBarWnd                                                                     }
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
{ The Original Code is uScrollBarWnd.pas.                                                          }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.ScrollBarWnd;
{
  +BugFix : If Window has Vert & Horz ScrollBar , the color of BottomRight rectangle is always white .
}

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  Vcl.StdCtrls,
  Vcl.Forms,
  Vcl.Styles.ControlWnd,
  Vcl.Controls;

const
  SB_SetVertThumbState = WM_USER + $55;
  SB_SetVertUpBtnState = WM_USER + $56;
  SB_SetVertDownBtnState = WM_USER + $57;
  SB_SetHorzThumbState = WM_USER + $60;
  SB_SetHorzLeftBtnState = WM_USER + $61;
  SB_SetHorzRightBtnState = WM_USER + $62;

type
  TScrollBarType = (sbVert, sbHorz);

type
  TScrollBarStyleHookHlp = class helper for TScrollBarStyleHook
  private
    procedure SetVSliderState(State: TThemedScrollBar);
    procedure SetVUpBtnState(State: TThemedScrollBar);
    procedure SetVDownBtnState(State: TThemedScrollBar);
    procedure SetHSliderState(State: TThemedScrollBar);
    procedure SetHLeftBtnState(State: TThemedScrollBar);
    procedure SetHRightBtnState(State: TThemedScrollBar);
  end;

type
  TScrollBarStyleHookEx = class(TScrollBarStyleHook)
  private
    procedure SetVertThumbState(var Message: TMessage);
      message SB_SetVertThumbState;
    procedure SetVertUpBtnState(var Message: TMessage);
      message SB_SetVertUpBtnState;
    procedure SetVertDownBtnState(var Message: TMessage);
      message SB_SetVertDownBtnState;
    procedure SetHorzThumbState(var Message: TMessage);
      message SB_SetHorzThumbState;
    procedure SetHorzLeftBtnState(var Message: TMessage);
      message SB_SetHorzLeftBtnState;
    procedure SetHorzRightBtnState(var Message: TMessage);
      message SB_SetHorzRightBtnState;
    procedure WMPaint(var Message: TMessage); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
  public
    constructor Create(AControl: TWinControl); override;
    destructor Destroy; override;
  end;

type
  TScrollBarEx = class(TScrollBar)
  private
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    class constructor Create;
    class destructor Destroy;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

type
  TScrollBarWnd = class(TControlWnd)
  private
    FVertScrollBar: TScrollBarEx;
    FHorzScrollBar: TScrollBarEx;
    FMouseDown: Boolean;
    FUpdateScrollBar: Boolean;
    procedure EraseLRCorner;
    function GetScrollInfo(ScrollBarType: TScrollBarType): TScrollInfo;
    function GetScrollBarInfo(ScrollBarType: TScrollBarType): TScrollBarInfo;
    function GetVertSliderRect: TRect;
    function GetVertScrollBarRect: TRect;
    function GetVertUpBtnRect: TRect;
    function GetVertDownRect: TRect;
    function GetHorzScrollBarRect: TRect;
    function GetBtnSize: Integer;
    function GetVertScrollBar: Boolean;
    function GetHorzScrollBar: Boolean;
    function GetHorzSliderRect: TRect;
    function GetHorzLeftRect: TRect;
    function GetHorzRightRect: TRect;
  protected
    procedure UpdateVertScollBar;
    procedure UpdateHorzScrollBar;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property VertScrollBar: Boolean read GetVertScrollBar;
    property HorzScrollBar: Boolean read GetHorzScrollBar;
    property VertSliderRect: TRect read GetVertSliderRect;
    property VertScrollBarRect: TRect read GetVertScrollBarRect;
    property VertUpBtnRect: TRect read GetVertUpBtnRect;
    property VertDownRect: TRect read GetVertDownRect;
    property HorzScrollBarRect: TRect read GetHorzScrollBarRect;
    property HorzSliderRect: TRect read GetHorzSliderRect;
    property HorzLeftRect: TRect read GetHorzLeftRect;
    property HorzRightRect: TRect read GetHorzRightRect;
  end;

implementation

{ TScrollBarWnd }
constructor TScrollBarWnd.Create(AHandle: THandle);
begin
  inherited;
  FVertScrollBar := nil;
  FHorzScrollBar := nil;
  FMouseDown := False;
  FUpdateScrollBar := True;
end;

destructor TScrollBarWnd.Destroy;
begin
  if Assigned(FVertScrollBar) then
    FreeAndNil(FVertScrollBar);
  if Assigned(FHorzScrollBar) then
    FreeAndNil(FHorzScrollBar);
  inherited;
end;

procedure TScrollBarWnd.EraseLRCorner;
var
  DC: HDC;
  R: TRect;
begin
  if VertScrollBar and HorzScrollBar then
    begin
      DC := GetWindowDC(Handle);
      R := HorzScrollBarRect;
      if (R.Width > 0) and (R.Top > 0) then
        begin
          R := Rect(R.Left, R.Top, R.Right + GetBtnSize, R.Bottom);
          FillRectangle(DC, R, StyleServices.GetStyleColor(scWindow));
        end;
      ReleaseDC(Handle, DC);
    end;
end;

function TScrollBarWnd.GetBtnSize: Integer;
begin
  Result := GetSystemMetrics(SM_CYVSCROLL);
end;

function TScrollBarWnd.GetVertScrollBar: Boolean;
begin
  Result := Style and WS_VSCROLL = WS_VSCROLL;
end;

function TScrollBarWnd.GetHorzLeftRect: TRect;
var
  R: TRect;
begin
  R := GetHorzScrollBarRect;
  if R.Width > 0 then
    Result := Rect(R.Left, R.Top, R.Left + GetBtnSize, R.Top + GetBtnSize)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetHorzRightRect: TRect;
var
  R: TRect;
begin
  R := GetHorzScrollBarRect;
  if R.Width > 0 then
    Result := Rect(R.Right - GetBtnSize, R.Top, R.Right, R.Top + GetBtnSize)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetHorzScrollBar: Boolean;
begin
  Result := Style and WS_HSCROLL = WS_HSCROLL;
end;

function TScrollBarWnd.GetHorzScrollBarRect: TRect;
var
  P: TPoint;
  R: TRect;
  LScrollBarInfo: TScrollBarInfo;
begin
  LScrollBarInfo := GetScrollBarInfo(sbHorz);
  R := LScrollBarInfo.rcScrollBar;
  if R.Width > 0 then
    begin
      P := Point(R.Left, R.Top);
      ScreenToClient(Handle, P);
      Result := Rect(P.X, P.Y, P.X + R.Width, P.Y + R.Height);
    end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetHorzSliderRect: TRect;
var
  R: TRect;
  LScrollBarInfo: TScrollBarInfo;
begin
  LScrollBarInfo := GetScrollBarInfo(sbHorz);
  R := GetHorzScrollBarRect;
  if R.Width > 0 then
    Result := Rect(LScrollBarInfo.xyThumbTop, R.Top,
      LScrollBarInfo.xyThumbBottom, R.Bottom)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetScrollBarInfo(ScrollBarType: TScrollBarType): TScrollBarInfo;
var
  BarInfo: TScrollBarInfo;
  idObj: System.Longint;
begin
  if ScrollBarType = sbVert then
    idObj := Longint(OBJID_VSCROLL)
  else
    idObj := Longint(OBJID_HSCROLL);
  FillChar(BarInfo, sizeof(TScrollBarInfo), Char(0));
  BarInfo.cbSize := sizeof(BarInfo);
  Winapi.Windows.GetScrollBarInfo(Handle, idObj, BarInfo);
  Result := BarInfo;
end;

function TScrollBarWnd.GetScrollInfo(ScrollBarType: TScrollBarType): TScrollInfo;
var
  idObj: Integer;
begin
  if ScrollBarType = sbVert then
    idObj := SB_VERT
  else
    idObj := SB_HORZ;
  FillChar(Result, sizeof(TScrollInfo), Char(0));
  Result.cbSize := sizeof(TScrollInfo);
  Result.fMask := SIF_ALL;
  Winapi.Windows.GetScrollInfo(Handle, idObj, Result);
end;

function TScrollBarWnd.GetVertDownRect: TRect;
var
  R: TRect;
begin
  R := GetVertScrollBarRect;
  if R.Width > 0 then
    Result := Rect(R.Left, R.Bottom - GetBtnSize, R.Right, R.Bottom)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetVertScrollBarRect: TRect;
var
  P: TPoint;
  R: TRect;
  LScrollBarInfo: TScrollBarInfo;
begin
  LScrollBarInfo := GetScrollBarInfo(sbVert);
  R := LScrollBarInfo.rcScrollBar;
  if R.Width > 0 then
    begin
      P := Point(R.Left, R.Top);
      ScreenToClient(Handle, P);
      Result := Rect(P.X, P.Y, P.X + R.Width, P.Y + R.Height);
    end
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetVertSliderRect: TRect;
var
  R: TRect;
  LScrollBarInfo: TScrollBarInfo;
begin
  LScrollBarInfo := GetScrollBarInfo(sbVert);
  R := GetVertScrollBarRect;
  if R.Width > 0 then
    Result := Rect(R.Left, LScrollBarInfo.xyThumbTop, R.Right,
      LScrollBarInfo.xyThumbBottom)
  else
    Result := Rect(0, 0, 0, 0);
end;

function TScrollBarWnd.GetVertUpBtnRect: TRect;
var
  R: TRect;
begin
  R := GetVertScrollBarRect;
  if R.Width > 0 then
    Result := Rect(R.Left, R.Top, R.Left + GetBtnSize, R.Top + GetBtnSize)
  else
    Result := Rect(0, 0, 0, 0);
end;

procedure TScrollBarWnd.UpdateHorzScrollBar;
var
  LScrollInfo: TScrollInfo;
begin
  if not Assigned(FHorzScrollBar) then
    begin
      if (HorzScrollBarRect.Width > 0) and (HorzScrollBar) then
        begin
          FHorzScrollBar := TScrollBarEx.CreateParented(GetParentHandle);
          FHorzScrollBar.Kind := sbHorizontal;
        end;
    end;
  if Assigned(FHorzScrollBar) then
    begin
      if (HorzScrollBarRect.Width > 0) and (HorzScrollBar) then
        begin
          FHorzScrollBar.Left := HorzScrollBarRect.Left;
          FHorzScrollBar.Top := HorzScrollBarRect.Top;
          FHorzScrollBar.Height := HorzScrollBarRect.Height;
          FHorzScrollBar.Width := HorzScrollBarRect.Width;
          LScrollInfo := GetScrollInfo(sbHorz);
          with LScrollInfo do
            begin
              if ((nMax or nPos or nPage) <> 0) then
                begin
                  FHorzScrollBar.PageSize := 0;
                  if (nPage > 0) and (nPage < nMax) and (nMax > 0) then
                    FHorzScrollBar.PageSize := nPage;
                  FHorzScrollBar.SetParams(nPos, nMin, nMax);
                  FHorzScrollBar.Enabled := not(nMax = nPage);
                end
            end;
          SetWindowPos(FHorzScrollBar.Handle, HWND_TOP, FHorzScrollBar.Left,
            FHorzScrollBar.Top, FHorzScrollBar.Width, FHorzScrollBar.Height,
            SWP_SHOWWINDOW);
        end
      else
        begin
          FHorzScrollBar.Hide;
          FreeAndNil(FHorzScrollBar);
        end;
    end;
end;

procedure TScrollBarWnd.UpdateVertScollBar;
var
  LScrollInfo: TScrollInfo;
begin
  if not Assigned(FVertScrollBar) then
    begin
      if (VertScrollBarRect.Width > 0) and (VertScrollBar) then
        begin
          FVertScrollBar := TScrollBarEx.CreateParented(GetParentHandle);
          FVertScrollBar.Kind := sbVertical;
        end;
    end;
  if Assigned(FVertScrollBar) then
    begin
      if (VertScrollBarRect.Width > 0) and (VertScrollBar) then
        begin
          FVertScrollBar.Left := VertScrollBarRect.Left;
          FVertScrollBar.Top := VertScrollBarRect.Top;
          FVertScrollBar.Height := VertScrollBarRect.Height;
          FVertScrollBar.Width := VertScrollBarRect.Width;
          LScrollInfo := GetScrollInfo(sbVert);
          with LScrollInfo do
            begin
              if ((nMax or nPos or nPage) <> 0) then
                begin
                  FVertScrollBar.PageSize := 0;
                  if (nPage > 0) and (nPage < nMax) and (nMax > 0) then
                    FVertScrollBar.PageSize := nPage;
                  FVertScrollBar.SetParams(nPos, nMin, nMax);
                  FVertScrollBar.Enabled := not (nMax = nPage);
                end
            end;

          SetWindowPos(FVertScrollBar.Handle, HWND_TOP, FVertScrollBar.Left,
            FVertScrollBar.Top, FVertScrollBar.Width, FVertScrollBar.Height,
            SWP_SHOWWINDOW);
        end
      else
        begin
          FVertScrollBar.Hide;
          FreeAndNil(FVertScrollBar);
        end;
    end;
end;

procedure TScrollBarWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  lParam: UINT_PTR;
  DownPoint, P: TPoint;
begin

  uMsg := Message.Msg;
  lParam := Message.lParam;
  case uMsg of

    WM_CREATE, WM_DESTROY, WM_NCDESTROY:
      begin
        FUpdateScrollBar := True;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_VSCROLL:
      begin
        Message.Result := CallOrgWndProc(Message);
        UpdateVertScollBar;
        FUpdateScrollBar := False;
      end;

    WM_HSCROLL:
      begin
        Message.Result := CallOrgWndProc(Message);
        UpdateHorzScrollBar;
        FUpdateScrollBar := False;
      end;

    WM_WINDOWPOSCHANGED, WM_SHOWWINDOW:
      begin
        Message.Result := CallOrgWndProc(Message);
        UpdateVertScollBar;
        UpdateHorzScrollBar;
        FUpdateScrollBar := True;
      end;

    WM_NCMOUSELEAVE:
      begin
        Message.Result := CallOrgWndProc(Message);
        if Assigned(FVertScrollBar) then
          if not FMouseDown then
            begin
              SendMessage(FVertScrollBar.Handle, SB_SetVertThumbState,
                Integer(tsThumbBtnVertNormal), 0);
              SendMessage(FVertScrollBar.Handle, SB_SetVertUpBtnState,
                Integer(tsArrowBtnUpNormal), 0);
              SendMessage(FVertScrollBar.Handle, SB_SetVertDownBtnState,
                Integer(tsArrowBtnDownNormal), 0);
            end;

        if Assigned(FHorzScrollBar) then
          if not FMouseDown then
            begin
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzThumbState,
                Integer(tsThumbBtnHorzNormal), 0);
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzLeftBtnState,
                Integer(tsArrowBtnLeftNormal), 0);
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzRightBtnState,
                Integer(tsArrowBtnRightNormal), 0);
            end;
      end;

    WM_NCLBUTTONDOWN:
      begin
        FMouseDown := True;
        DownPoint.X := GET_X_LPARAM(lParam);
        DownPoint.Y := GET_Y_LPARAM(lParam);
        ScreenToClient(Handle, DownPoint);
        if Assigned(FVertScrollBar) then
          begin
            if VertSliderRect.Contains(DownPoint) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertThumbState,
                Integer(tsThumbBtnVertPressed), 0);

            if VertUpBtnRect.Contains(DownPoint) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertUpBtnState,
                Integer(tsArrowBtnUpPressed), 0);

            if VertDownRect.Contains(DownPoint) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertDownBtnState,
                Integer(tsArrowBtnDownPressed), 0);
          end;

        if Assigned(FHorzScrollBar) then
          begin
            if HorzSliderRect.Contains(DownPoint) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzThumbState,
                Integer(tsThumbBtnHorzPressed), 0);

            if HorzLeftRect.Contains(DownPoint) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzLeftBtnState,
                Integer(tsArrowBtnLeftPressed), 0);

            if HorzRightRect.Contains(DownPoint) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzRightBtnState,
                Integer(tsArrowBtnRightPressed), 0);
          end;

        Message.Result := CallOrgWndProc(Message);
        FMouseDown := False;

        if Assigned(FVertScrollBar) then
          begin
            SendMessage(FVertScrollBar.Handle, SB_SetVertThumbState,
              Integer(tsThumbBtnVertNormal), 0);
            SendMessage(FVertScrollBar.Handle, SB_SetVertUpBtnState,
              Integer(tsArrowBtnUpNormal), 0);
            SendMessage(FVertScrollBar.Handle, SB_SetVertDownBtnState,
              Integer(tsArrowBtnDownNormal), 0);
          end;

        if Assigned(FHorzScrollBar) then
          begin
            SendMessage(FHorzScrollBar.Handle, SB_SetHorzThumbState,
              Integer(tsThumbBtnHorzNormal), 0);
            SendMessage(FHorzScrollBar.Handle, SB_SetHorzLeftBtnState,
              Integer(tsArrowBtnLeftNormal), 0);
            SendMessage(FHorzScrollBar.Handle, SB_SetHorzRightBtnState,
              Integer(tsArrowBtnRightNormal), 0);
          end;
      end;

    WM_NCHITTEST:
      begin
        P.X := GET_X_LPARAM(lParam);
        P.Y := GET_Y_LPARAM(lParam);
        ScreenToClient(Handle, P);
        if Assigned(FVertScrollBar) then
          begin
            if VertSliderRect.Contains(P) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertThumbState,
                Integer(tsThumbBtnVertHot), 0)
            else
              SendMessage(FVertScrollBar.Handle, SB_SetVertThumbState,
                Integer(tsThumbBtnVertNormal), 0);

            if VertUpBtnRect.Contains(P) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertUpBtnState,
                Integer(tsArrowBtnUpHot), 0)
            else
              SendMessage(FVertScrollBar.Handle, SB_SetVertUpBtnState,
                Integer(tsArrowBtnUpNormal), 0);

            if VertDownRect.Contains(P) then
              SendMessage(FVertScrollBar.Handle, SB_SetVertDownBtnState,
                Integer(tsArrowBtnDownHot), 0)
            else
              SendMessage(FVertScrollBar.Handle, SB_SetVertDownBtnState,
                Integer(tsArrowBtnDownNormal), 0);
          end;

        if Assigned(FHorzScrollBar) then
          begin
            if HorzSliderRect.Contains(P) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzThumbState,
                Integer(tsThumbBtnHorzHot), 0)
            else
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzThumbState,
                Integer(tsThumbBtnHorzNormal), 0);

            if HorzLeftRect.Contains(P) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzLeftBtnState,
                Integer(tsArrowBtnLeftHot), 0)
            else
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzLeftBtnState,
                Integer(tsArrowBtnLeftNormal), 0);

            if HorzRightRect.Contains(P) then
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzRightBtnState,
                Integer(tsArrowBtnRightHot), 0)
            else
              SendMessage(FHorzScrollBar.Handle, SB_SetHorzRightBtnState,
                Integer(tsArrowBtnRightNormal), 0)
          end;

        Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCPAINT:
      begin
        Message.Result := CallOrgWndProc(Message);
        EraseLRCorner;
      end;

    WM_PAINT:
      begin
        if FUpdateScrollBar then
          begin
            UpdateVertScollBar;
            UpdateHorzScrollBar;
          end;

        Message.Result := CallOrgWndProc(Message);
        EraseLRCorner;
      end
  else
    Message.Result := CallOrgWndProc(Message);
  end;

end;

{ TScrollBarEx }

class constructor TScrollBarEx.Create;
begin
  TCustomStyleEngine.RegisterStyleHook(TScrollBarEx, TScrollBarStyleHookEx);
end;

class destructor TScrollBarEx.Destroy;
begin
  TCustomStyleEngine.UnRegisterStyleHook(TScrollBarEx, TScrollBarStyleHookEx);
end;

constructor TScrollBarEx.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TScrollBarEx.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or WS_CHILDWINDOW or WS_CLIPCHILDREN or
    WS_CLIPSIBLINGS;
  Params.ExStyle := Params.ExStyle or WS_EX_NOPARENTNOTIFY;
end;

procedure TScrollBarEx.WMNCHitTest(var Message: TWMNCHitTest);
begin
  Message.Result := HTTRANSPARENT;
end;

{ TScrollBarStyleHookEx }

constructor TScrollBarStyleHookEx.Create(AControl: TWinControl);
begin
  inherited;
end;

destructor TScrollBarStyleHookEx.Destroy;
begin
  inherited;
end;

procedure TScrollBarStyleHookEx.SetHorzLeftBtnState(var Message: TMessage);
begin
  SetHLeftBtnState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.SetHorzRightBtnState(var Message: TMessage);
begin
  SetHRightBtnState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.SetHorzThumbState(var Message: TMessage);
begin
  SetHSliderState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.SetVertDownBtnState(var Message: TMessage);
begin
  SetVDownBtnState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.SetVertThumbState(var Message: TMessage);
begin
  SetVSliderState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.SetVertUpBtnState(var Message: TMessage);
begin
  SetVUpBtnState(TThemedScrollBar(Message.wParam));
end;

procedure TScrollBarStyleHookEx.WMNCPaint(var Message: TMessage);
begin
  inherited;
end;

procedure TScrollBarStyleHookEx.WMPaint(var Message: TMessage);
begin
  inherited;
end;

{ TScrollBarStyleHookHlp }

procedure TScrollBarStyleHookHlp.SetVUpBtnState(State: TThemedScrollBar);
begin
  if State <> Self.FVUpState then
    begin
      Self.FVUpState := State;
      Self.Invalidate;
    end;
end;

procedure TScrollBarStyleHookHlp.SetHLeftBtnState(State: TThemedScrollBar);
begin
  if State <> Self.FHUpState then
    begin
      Self.FHUpState := State;
      Self.Invalidate;
    end;
end;

procedure TScrollBarStyleHookHlp.SetHRightBtnState(State: TThemedScrollBar);
begin
  if State <> Self.FHDownState then
    begin
      Self.FHDownState := State;
      Self.Invalidate;
    end;
end;

procedure TScrollBarStyleHookHlp.SetHSliderState(State: TThemedScrollBar);
begin
  if State <> Self.FHSliderState then
    begin
      Self.FHSliderState := State;
      Self.Invalidate;
    end;
end;

procedure TScrollBarStyleHookHlp.SetVDownBtnState(State: TThemedScrollBar);
begin
  if State <> Self.FVDownState then
    begin
      Self.FVDownState := State;
      Self.Invalidate;
    end;
end;

procedure TScrollBarStyleHookHlp.SetVSliderState(State: TThemedScrollBar);
begin
  if State <> Self.FVSliderState then
    begin
      Self.FVSliderState := State;
      Self.Invalidate;
    end;
end;

end.
