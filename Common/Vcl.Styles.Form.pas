{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Form                                                                             }
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
{ The Original Code is Vcl.Styles.Form.pas                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Form;


interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles.ControlWnd;

type
  TFormWnd = class(TControlWnd)
  private
    FFormActive: Boolean;
    FRegion: HRGN;
    FSizeAligned: Boolean;
    FSizeAlignedMsg: Boolean;
    FChangWidth: Integer;
    SysCloseBtnRect: TRect;
    StaticBrush: HBRUSH;
    EditBrush: HBRUSH;

    FCloseButtonRect : TRect;
    FMaxButtonRect :TRect;
    FMinButtonRect :TRect;
    FHelpButtonRect :TRect;
    FSysMenuButtonRect :TRect;
    FCaptionRect :TRect;

    FIcon: TIcon;
    FIconHandle: HICON;
    FChangeSizeCalled: Boolean;

    FLeft : Integer;
    FTop : Integer;
    FWidth : Integer;
    FHeight : Integer;
    FHotButton: Integer;
    FPressedButton: Integer;
    FCaptionEmulation: Boolean;
    FNeedsUpdate: Boolean;
  private
    FWindowState: TWindowState;
    function GetBorderSize: TRect;
    procedure PaintNC(Canvas: TCanvas);
    procedure PaintBkgnd(DC: HDC; R: TRect; FrameState: TFrameState);
    function GetRegion: HRGN;
    function GetBorderStyle: TFormBorderStyle;
    function GetBorderIcons: TBorderIcons;
    function GetIconFast: TIcon;
    function GetIcon: TIcon;
    procedure ChangeSize;
    function NormalizePoint(P: TPoint): TPoint;
    function GetHitTest(P: TPoint): Integer;
    procedure Close;
    procedure Help;
    procedure Maximize;
    procedure Minimize;
    procedure Restore;
    procedure UpdateForm;
  protected
    property BorderStyle: TFormBorderStyle read GetBorderStyle;
    property BorderIcons : TBorderIcons read  GetBorderIcons;
    property WindowState : TWindowState read  FWindowState;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

implementation

uses
   System.SysUtils;

{ TFormWnd }

procedure TFormWnd.Close;
begin
  if Handle <> 0 then
    SendMessage(Handle, WM_SYSCOMMAND, SC_CLOSE, 0);
end;

constructor TFormWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  SetRectEmpty(SysCloseBtnRect);

  FRegion := 0;
  EditBrush := 0;
  StaticBrush := 0;
  FChangWidth := 0;
  FSizeAligned := False;
  FSizeAlignedMsg := False;
  FIcon:=nil;
  FIconHandle:=0;
  FWindowState:=wsNormal;
  FChangeSizeCalled:=False;

  FLeft := Self.Left;
  FTop := Self.Top;
  FWidth := Self.Width;
  FHeight := Self.Height;

  FCaptionEmulation:=False;
  FNeedsUpdate:=True;
end;

destructor TFormWnd.Destroy;
begin
  inherited;
  DeleteObject(StaticBrush);
  DeleteObject(FRegion);
  DeleteObject(EditBrush);
  FRegion := 0;
  EditBrush := 0;
  StaticBrush := 0;
  if FIcon <> nil then
    FreeAndNil(FIcon);
end;

procedure TFormWnd.ChangeSize;
var
  LRegion: HRGN;
  LRect: TRect;
begin
  FChangeSizeCalled := True;

  if IsIconic(Handle) then
   begin
     LRect := GetBorderSize;
     FHeight := LRect.Top + LRect.Bottom;
   end;

  if BorderStyle <> bsNone then
  begin
    LRegion := FRegion;
    try
      FRegion := GetRegion;
      SetWindowRgn(Handle, FRegion, True);
    finally
      if LRegion <> 0 then
        DeleteObject(LRegion);
    end;
    FChangeSizeCalled := False;
  end;
end;

procedure TFormWnd.PaintBkgnd(DC: HDC; R: TRect; FrameState: TFrameState);
var
  LDetails: TThemedElementDetails;
  ThemeElement: TThemedElement;
begin
  ThemeElement := TThemedElement.teWindow;
  LDetails := TThemedElementDetails.Create(ThemeElement, 0,
    Integer(FrameState));

  StyleServices.DrawElement(DC, LDetails, R);
end;

function TFormWnd.GetBorderIcons: TBorderIcons;
var
  LStyle, LExStyle : Cardinal;
begin
  //Style ... (WS_MINIMIZEBOX or WS_MAXIMIZEBOX or WS_SYSMENU);
  //ExStyle ... WS_EX_CONTEXTHELP;
  LStyle:=Style;
  LExStyle:=ExStyle;


  Result := [];
  if (LStyle and WS_MINIMIZEBOX) = WS_MINIMIZEBOX then
   Result:= Result + [biMinimize];

  if (LStyle and WS_MAXIMIZEBOX) = WS_MAXIMIZEBOX then
   Result:= Result + [biMaximize];

  if (LStyle and WS_SYSMENU) = WS_SYSMENU then
   Result:= Result + [biSystemMenu];

  if (LExStyle and WS_EX_CONTEXTHELP) = WS_EX_CONTEXTHELP then
   Result:= Result + [biHelp];

  case BorderStyle of
    bsNone: Result := [];
    bsDialog: Result := Result * [biSystemMenu, biHelp];
    bsToolWindow,
    bsSizeToolWin: Result := Result * [biSystemMenu];
  end;
end;

function TFormWnd.GetBorderSize: TRect;
var
  LSize    : TSize;
  LDetails : TThemedElementDetails;
  LDetail  : TThemedWindow;
begin
  Result := Rect(0, 0, 0, 0);
  if BorderStyle = bsNone then Exit;

  //height
  if (BorderStyle <> bsToolWindow) and  (BorderStyle <> bsSizeToolWin) then
    LDetail := twCaptionActive
  else
    LDetail := twSmallCaptionActive;
  LDetails := StyleServices.GetElementDetails(LDetail);
  StyleServices.GetElementSize(0, LDetails, esActual, LSize);
  Result.Top := LSize.cy;

  //left
  if (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
    LDetail := twFrameLeftActive
  else
    LDetail := twSmallFrameLeftActive;
  LDetails := StyleServices.GetElementDetails(LDetail);
  StyleServices.GetElementSize(0, LDetails, esActual, LSize);
  Result.Left := LSize.cx;

  //right
  if (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
    LDetail := twFrameRightActive
  else
    LDetail := twSmallFrameRightActive;
  LDetails := StyleServices.GetElementDetails(LDetail);
  StyleServices.GetElementSize(0, LDetails, esActual, LSize);
  Result.Right := LSize.cx;

  //bottom
  if (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
    LDetail := twFrameBottomActive
  else
    LDetail := twSmallFrameBottomActive;
  LDetails := StyleServices.GetElementDetails(LDetail);
  StyleServices.GetElementSize(0, LDetails, esActual, LSize);
  Result.Bottom := LSize.cy;
end;

function TFormWnd.GetBorderStyle: TFormBorderStyle;
var
  LStyle, LExStyle : Cardinal;
begin
  Result:=bsNone;

  LStyle:=Style;
  LExStyle:=ExStyle;

  if ((LStyle and WS_POPUP) = WS_POPUP) and  ((LStyle and WS_CAPTION) = WS_CAPTION)  then
   Result:=bsDialog
  else
  if (LStyle and WS_POPUP) = WS_POPUP then
   Result:=bsNone
  else
  if ((LStyle and WS_CAPTION) = WS_CAPTION) and ((LStyle and WS_THICKFRAME) = WS_THICKFRAME) and ((LExStyle and WS_EX_TOOLWINDOW) = WS_EX_TOOLWINDOW) then
   Result:=bsSizeToolWin
  else
  if ((LStyle and WS_CAPTION) = WS_CAPTION) and ((LStyle and WS_BORDER) = WS_BORDER) and ((LExStyle and WS_EX_TOOLWINDOW) = WS_EX_TOOLWINDOW) then
   Result:=bsToolWindow
  else
  if ((LStyle and WS_CAPTION) = WS_CAPTION) and ((LStyle and WS_THICKFRAME) = WS_THICKFRAME) then
   Result:=bsSizeable
  else
  if ((LStyle and WS_CAPTION) = WS_CAPTION) and ((LStyle and WS_BORDER) = WS_BORDER) then
   Result:=bsSingle;
end;


function TFormWnd.GetHitTest(P: TPoint): Integer;
var
  FBorderSize  : TRect;
  FTopLeftRect,  FTopRightRect,
  FBottomLeftRect, FBottomRightRect,
  FTopRect, FLeftRect, FRightRect, FBottomRect, FHitCaptionRect: TRect;
begin
  Result := HTCLIENT;
  if BorderStyle = bsNone then
      Exit;

  FBorderSize := GetBorderSize;
  FHitCaptionRect := FCaptionRect;
  FHitCaptionRect.Top := FBorderSize.Left;
  FBorderSize.Top := FHitCaptionRect.Top;

  if PtInRect(FHitCaptionRect, P) then
    Exit(HTCAPTION)
  else if PtInRect(FCloseButtonRect, P) then
    Exit(HTCLOSE)
  else if PtInRect(FMaxButtonRect, P) then
    Exit(HTMAXBUTTON)
  else if PtInRect(FMinButtonRect, P) then
    Exit(HTMINBUTTON)
  else if PtInRect(FHelpButtonRect, P) then
    Exit(HTHELP)
  else if PtInRect(FSysMenuButtonRect, P) then
    Exit(HTSYSMENU);

  if (WindowState = wsMaximized) or
     (WindowState = wsMinimized) then
    Exit;

  if (BorderStyle = bsDialog) or  (BorderStyle = bsSingle) or (BorderStyle = bsToolWindow) then
  begin
    if PtInRect(Rect(FBorderSize.Left, FBorderSize.Top, FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom), P) then
      Exit(HTCLIENT)
    else
      Exit(HTBORDER);
  end;

  FTopLeftRect := Rect(0, 0, FBorderSize.Left, FBorderSize.Top);
  FTopRightRect := Rect(FWidth - FBorderSize.Right, 0, FWidth, FBorderSize.Top);
  FBottomLeftRect := Rect(0, FHeight - FBorderSize.Bottom, FBorderSize.Left, FHeight);
  FBottomRightRect := Rect(FWidth - FBorderSize.Right, FHeight - FBorderSize.Bottom,
    FWidth, FHeight);
  FTopRect := Rect(FTopLeftRect.Right, 0, FTopRightRect.Left, FBorderSize.Top);
  FLeftRect := Rect(0, FTopLeftRect.Bottom, FBorderSize.Left, FBottomLeftRect.Top);
  FRightRect := Rect(FWidth - FBorderSize.Right, FTopRightRect.Bottom, FWidth, FBottomRightRect.Top);
  FBottomRect := Rect(FBottomLeftRect.Right, FHeight - FBorderSize.Bottom, FBottomRightRect.Left, FHeight);

  if PtInRect(FTopLeftRect, P) then
    Result := HTTOPLEFT
  else if PtInRect(FTopRightRect, P) then
    Result := HTTOPRIGHT
  else if PtInRect(FBottomLeftRect, P) then
    Result := HTBOTTOMLEFT
   else if PtInRect(FBottomRightRect, P) then
    Result := HTBOTTOMRIGHT
  else if PtInRect(FLeftRect, P) then
    Result := HTLEFT
  else if PtInRect(FRightRect, P) then
    Result := HTRIGHT
  else if PtInRect(FBottomRect, P) then
    Result := HTBOTTOM
  else if PtInRect(FTopRect, P) then
    Result := HTTOP;
end;

function TFormWnd.GetIcon: TIcon;
var
  IconX, IconY: Integer;
  TmpHandle: THandle;
  Info: TWndClassEx;
  Buffer: array [0..255] of Char;
begin
  TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_SMALL, 0));
  if TmpHandle = 0 then
    TmpHandle := THandle(SendMessage(Handle, WM_GETICON, ICON_BIG, 0));
  if TmpHandle = 0 then
  begin
    { Get instance }
    GetClassName(Handle, @Buffer, SizeOf(Buffer));
    FillChar(Info, SizeOf(Info), 0);
    Info.cbSize := SizeOf(Info);

    if GetClassInfoEx(GetWindowLong(Handle, GWL_HINSTANCE), @Buffer, Info) then
    begin
      TmpHandle := Info.hIconSm;
      if TmpHandle = 0 then
        TmpHandle := Info.hIcon;
    end
  end;

  if FIcon = nil then
    FIcon := TIcon.Create;
  if TmpHandle <> 0 then
  begin
    IconX := GetSystemMetrics(SM_CXSMICON);
    if IconX = 0 then
      IconX := GetSystemMetrics(SM_CXSIZE);
    IconY := GetSystemMetrics(SM_CYSMICON);
    if IconY = 0 then
      IconY := GetSystemMetrics(SM_CYSIZE);
    FIcon.Handle := CopyImage(TmpHandle, IMAGE_ICON, IconX, IconY, 0);
    FIconHandle := TmpHandle;
  end;

  Result := FIcon;
end;

function TFormWnd.GetIconFast: TIcon;
begin
  if (FIcon = nil) or (FIconHandle = 0) then
    Result := GetIcon
  else
    Result := FIcon;
end;

procedure TFormWnd.PaintNC(Canvas: TCanvas);
var
  Details, CaptionDetails, IconDetails: TThemedElementDetails;
  Detail: TThemedWindow;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
begin

  {init some parameters}
  FCloseButtonRect := Rect(0, 0, 0, 0);
  FMaxButtonRect := Rect(0, 0, 0, 0);
  FMinButtonRect := Rect(0, 0, 0, 0);
  FHelpButtonRect := Rect(0, 0, 0, 0);
  FSysMenuButtonRect := Rect(0, 0, 0, 0);
  FCaptionRect := Rect(0, 0, 0, 0);

  R := GetBorderSize;

  {draw caption}

  if (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twCaptionActive
    else
      Detail := twCaptionInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallCaptionActive
    else
      Detail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(Self.Width, R.Top);

  {draw caption border}
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, DrawRect);
  TextRect := DrawRect;
  CaptionDetails := Details;

  {draw icon}
  if (biSystemMenu in BorderIcons) and
     (BorderStyle <> bsDialog) and
     (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  {draw buttons}
  if (biSystemMenu in BorderIcons) then
  begin
    if (BorderStyle <> bsToolWindow) and
       (BorderStyle <> bsSizeToolWin) then
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else
        if FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
     end
    else
    begin
      if (FPressedButton = HTCLOSE) and (FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    Details := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in BorderIcons) and
     (biSystemMenu in BorderIcons) and
     (BorderStyle <> bsDialog) and
     (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
  begin
    if WindowState = wsMaximized then
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (FPressedButton = HTMAXBUTTON) and (FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in BorderIcons) and
     (biSystemMenu in BorderIcons) and
     (BorderStyle <> bsDialog) and
     (BorderStyle <> bsToolWindow) and
     (BorderStyle <> bsSizeToolWin) then
  begin
    if (FPressedButton = HTMINBUTTON) and (FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else
      if FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);
    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    FMinButtonRect := ButtonRect;
  end;

  if (biHelp in BorderIcons) and (biSystemMenu in BorderIcons) and
     ((not (biMaximize in BorderIcons) and
     not (biMinimize in BorderIcons)) or (BorderStyle = bsDialog))
  then
  begin
    if (FPressedButton = HTHELP) and (FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    Details := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, Details, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, Details, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    FHelpButtonRect := ButtonRect;
  end;

  {draw text}
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];


  LText := Text;
  StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
  FCaptionRect := TextRect;

  {draw caption buffer}

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;


  {draw left border}

  if (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameLeftActive
    else
      Detail := twFrameLeftInActive
  end
  else
  begin
    if FFormActive then
      Detail := twSmallFrameLeftActive
    else
      Detail := twSmallFrameLeftInActive
  end;
  DrawRect := Rect(0, R.Top, R.Left, Self.Height - R.Bottom);
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw right border}
  if (BorderStyle <> bsToolWindow) and (BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameRightActive
    else
      Detail := twFrameRightInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameRightActive
    else
      Detail := twSmallFrameRightInActive
  end;
  DrawRect := Rect(Self.Width - R.Right, R.Top, Self.Width, Self.Height - R.Bottom);
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);

  {draw Bottom border}
  if (BorderStyle <> bsToolWindow) and(BorderStyle <> bsSizeToolWin) then
  begin
    if FFormActive then
      Detail := twFrameBottomActive
    else
      Detail := twFrameBottomInActive
  end
  else
  begin
   if FFormActive then
      Detail := twSmallFrameBottomActive
   else
      Detail := twSmallFrameBottomInActive
  end;
  DrawRect := Rect(0, Self.Height - R.Bottom, Self.Width, Self.Height);
  Details := StyleServices.GetElementDetails(Detail);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    StyleServices.DrawElement(Canvas.Handle, Details, DrawRect);
end;

procedure TFormWnd.Restore;
begin
  FPressedButton := 0;
  FHotButton := 0;
  SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0);
end;

procedure TFormWnd.UpdateForm;
begin
  if (BorderStyle = bsNone) or IsIconic(Handle) then Exit;

  SetWindowPos(Handle, 0, Self.Left, Self.Top, Self.Width-1, Self.Height,  SWP_NOZORDER + SWP_NOACTIVATE);
  SetWindowPos(Handle, 0, Self.Left, Self.Top, Self.Width+1, Self.Height,  SWP_NOZORDER + SWP_NOACTIVATE);
end;

function TFormWnd.GetRegion: HRGN;
var
  R: TRect;
  Details: TThemedElementDetails;
  Detail: TThemedWindow;
begin
  Result := 0;
  R := Rect(0, 0, Width, Height);
  Detail := twCaptionActive;
  DeleteObject(FRegion);
  Details := StyleServices.GetElementDetails(Detail);
  StyleServices.GetElementRegion(Details, R, Result);
end;


procedure TFormWnd.Help;
begin
  SendMessage(Handle, WM_SYSCOMMAND, SC_CONTEXTHELP, 0);
end;

procedure TFormWnd.Maximize;
begin
  FPressedButton := 0;
  FHotButton := 0;

  if IsZoomed(Handle) then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
  else
    SendMessage(Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0);
end;

procedure TFormWnd.Minimize;
begin
  FPressedButton := 0;
  FHotButton := 0;
  if IsIconic(Handle) then
    SendMessage(Handle, WM_SYSCOMMAND, SC_RESTORE, 0)
  else
    SendMessage(Handle, WM_SYSCOMMAND, SC_MINIMIZE, 0);
end;

function TFormWnd.NormalizePoint(P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
begin
  WindowPos := Point(FLeft, FTop);
  ClientPos := Point(0, 0);
  ClientToScreen(Handle, ClientPos);
  Result := P;
  ScreenToClient(Handle, Result);
  Inc(Result.X, ClientPos.X - WindowPos.X);
  Inc(Result.Y, ClientPos.Y - WindowPos.Y);
end;


procedure TFormWnd.WndProc(var Message: TMessage);
const
  WM_NCUAHDRAWCAPTION = $00AE;
var
  uMsg: UINT;
  wParam: UINT_PTR;
  LCalcSize_Params: PNCCalcSizeParams;
  LPoint: TPoint;
  LRect: TRect;
  LCanvas : TCanvas;
  Changed : Boolean;
  FWasPressedButton: Integer;
  LMinMaxInfo : PMinMaxInfo;
begin
  uMsg := Message.Msg;
  wParam := Message.wParam;

  case uMsg of

    WM_NCHITTEST:  //ok
      begin
        //Message.Result := CallOrgWndProc(Message);
        LPoint := NormalizePoint(Point(TWMNCHitTest(Message).XPos, TWMNCHitTest(Message).YPos));
        TWMNCHitTest(Message).Result := GetHitTest(LPoint);
        //CallOrgWndProc(Message);
      end;

    WM_NCMOUSEMOVE : //ok
      begin

        if (TWMNCHitMessage(Message).HitTest = HTCLOSE) or (TWMNCHitMessage(Message).HitTest = HTMAXBUTTON) or
           (TWMNCHitMessage(Message).HitTest = HTMINBUTTON) or (TWMNCHitMessage(Message).HitTest = HTHELP) then
        begin
          if FHotButton <> TWMNCHitMessage(Message).HitTest then
          begin
            FHotButton := TWMNCHitMessage(Message).HitTest;
            InvalidateNC;
          end;
          TWMNCHitMessage(Message).Result := 0;
          TWMNCHitMessage(Message).Msg := WM_NULL;
        end
        else if FHotButton <> 0 then
         begin
           FHotButton := 0;
           InvalidateNC;
         end;

      end;

    WM_NCLBUTTONDOWN: //ok
      begin
          if (TWMNCHitMessage(Message).HitTest = HTCLOSE) or (TWMNCHitMessage(Message).HitTest = HTMAXBUTTON) or
             (TWMNCHitMessage(Message).HitTest = HTMINBUTTON) or (TWMNCHitMessage(Message).HitTest = HTHELP) then
          begin
            FPressedButton := TWMNCHitMessage(Message).HitTest;
            InvalidateNC;
            TWMNCHitMessage(Message).Result := 0;
            TWMNCHitMessage(Message).Msg := WM_NULL;
          end
          else
            CallOrgWndProc(Message);
      end;

    WM_NCLBUTTONUP: //ok
      begin
        FWasPressedButton := FPressedButton;

        if FPressedButton <> 0 then
        begin
          FPressedButton := 0;
          InvalidateNC;
        end;


        if (TWMNCHitMessage(Message).HitTest = HTTOP) or (TWMNCHitMessage(Message).HitTest = HTBOTTOM) or (TWMNCHitMessage(Message).HitTest = HTLEFT) or
           (TWMNCHitMessage(Message).HitTest = HTRIGHT) or (TWMNCHitMessage(Message).HitTest = HTCAPTION) or (TWMNCHitMessage(Message).HitTest = HTTOPLEFT) or
           (TWMNCHitMessage(Message).HitTest = HTTOPRIGHT) or (TWMNCHitMessage(Message).HitTest = HTBOTTOMRIGHT) or
           (TWMNCHitMessage(Message).HitTest = HTBOTTOMLEFT) or (TWMNCHitMessage(Message).HitTest = HTSYSMENU) then
        begin
          Exit;
        end;

        if FWasPressedButton = FHotButton then
          if TWMNCHitMessage(Message).HitTest = HTCLOSE then
            Close
          else
          if (TWMNCHitMessage(Message).HitTest = HTMAXBUTTON) and (biMaximize in BorderIcons) then
          begin
            if WindowState <> wsMaximized then
              Maximize
            else
              Restore;
          end
          else if (TWMNCHitMessage(Message).HitTest = HTMINBUTTON) and (biMinimize in BorderIcons) then
          begin
            if WindowState <> wsMinimized then
              Minimize
            else
              Restore;
          end
          else
          if (TWMNCHitMessage(Message).HitTest = HTHELP) and (biHelp in BorderIcons) then
            Help;

        TWMNCHitMessage(Message).Result := 0;
        TWMNCHitMessage(Message).Msg := WM_NULL;
      end;

    WM_NCRBUTTONUP  : //ok
    begin
      if (TWMNCHitMessage(Message).HitTest = HTCAPTION) and FCaptionEmulation then
      begin
        SendMessage(Handle, $313, 0,
          MakeLong(TWMNCHitMessage(Message).XCursor, TWMNCHitMessage(Message).YCursor));
      end;
    end;

    WM_NCLBUTTONDBLCLK : //ok
    begin
      if (TWMNCHitMessage(Message).HitTest = HTTOP) or (TWMNCHitMessage(Message).HitTest = HTBOTTOM) or (TWMNCHitMessage(Message).HitTest = HTLEFT) or
         (TWMNCHitMessage(Message).HitTest = HTRIGHT) or (TWMNCHitMessage(Message).HitTest = HTCAPTION) or (TWMNCHitMessage(Message).HitTest = HTTOPLEFT) or
         (TWMNCHitMessage(Message).HitTest = HTTOPRIGHT) or (TWMNCHitMessage(Message).HitTest = HTBOTTOMRIGHT) or (TWMNCHitMessage(Message).HitTest = HTBOTTOMLEFT) then
      begin
        Exit;
      end;

      TWMNCHitMessage(Message).Result := 0;
      TWMNCHitMessage(Message).Msg := WM_NULL;
    end;

    WM_NCCALCSIZE:  //OK
      begin

        {calc NC info}
        if TWMNCCalcSize(Message).CalcValidRects and (BorderStyle <> bsNone) then
        begin
          LRect := GetBorderSize;

          LCalcSize_Params := TWMNCCalcSize(Message).CalcSize_Params;
          with LCalcSize_Params^.rgrc[0] do
          begin
            Inc(Left, LRect.Left);
            Inc(Top, LRect.Top);
            Dec(Right, LRect.Right);
            Dec(Bottom, LRect.Bottom);
          end;

        end;
        Message.Result := 0;

      end;

    WM_NCACTIVATE:  //ok
      begin
        FFormActive := (Message.WParam > 0);
        Message.Result := 1;
        if (BorderStyle <> bsNone) then
          InvalidateNC;
      end;

    WM_ACTIVATE : //ok
      begin
        CallOrgWndProc(Message);
        FFormActive := (TWMActivate(Message).Active > 0);
      end;

    WM_NCUAHDRAWCAPTION : //ok
      begin
        InvalidateNC;
      end;

    WM_SHOWWINDOW : //ok
      begin
        if TWMShowWindow(Message).Show and FNeedsUpdate then
        begin
          FNeedsUpdate := False;
          if not TStyleManager.SystemStyle.Enabled and (GetWindowLong(Handle, GWL_STYLE) and WS_CAPTION <> 0)  then
          begin
            FCaptionEmulation := True;
            SetWindowLong(Handle, GWL_STYLE, GetWindowLong(Handle, GWL_STYLE) and not WS_CAPTION);
          end;
          UpdateForm;
        end;

      end;

    WM_GETMINMAXINFO :  //ok
      begin
          CallOrgWndProc(Message);
          LRect := GetBorderSize;
          LMinMaxInfo := TWMGetMinMaxInfo(Message).MinMaxInfo;
          LMinMaxInfo^.ptMinTrackSize.y := LRect.Top + LRect.Bottom;
      end;

    //WM_SETTEXT : ;
    //WM_SYSCOMMAND :;

    WM_WINDOWPOSCHANGING : //ok
      begin
          CallOrgWndProc(Message);

          Changed := False;

          if FChangeSizeCalled then
            Exit;

          if (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0) or
             (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOMOVE = 0) then
          begin
            if (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOMOVE = 0) then
            begin
              FLeft := TWMWindowPosChanging(Message).WindowPos^.x;
              FTop := TWMWindowPosChanging(Message).WindowPos^.y;
            end;
            if (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0) then
            begin
              Changed := ((TWMWindowPosChanging(Message).WindowPos^.cx <> FWidth) or (TWMWindowPosChanging(Message).WindowPos^.cy <> FHeight)) and
                         (TWMWindowPosChanging(Message).WindowPos^.flags and SWP_NOSIZE = 0);
              FWidth := TWMWindowPosChanging(Message).WindowPos^.cx;
              FHeight := TWMWindowPosChanging(Message).WindowPos^.cy;
            end;
          end;

          if Changed then
          begin
            ChangeSize;
            if BorderStyle <> bsNone then
              InvalidateNC;
          end;

      end;

    WM_ERASEBKGND:
      begin
        PaintBkgnd(wParam, ClientRect, TFrameState(FFormActive));
        Message.Result := 1;
      end;

    WM_SIZE:   //ok
      begin
        Message.Result := CallOrgWndProc(Message);
        FRegion := GetRegion;
        SetWindowRgn(Handle, FRegion, True);
        case Message.WParam of
          SIZE_MINIMIZED  : FWindowState := wsMinimized;
          SIZE_MAXIMIZED  : FWindowState := wsMaximized;
          SIZE_RESTORED   : FWindowState := wsNormal;
        end;

        if IsIconic(Handle)  then
           InvalidateNC;
      end;

    WM_NCPAINT: //ok
      begin
        LCanvas := TCanvas.Create;
        try
          LCanvas.Handle := GetWindowDC(Handle);
          PaintNC(LCanvas);
        finally
          ReleaseDC(Handle, LCanvas.Handle);
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
