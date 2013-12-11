{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.StdCtrls                                                                         }
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
{ The Original Code is Vcl.Styles.StdCtrls.pas.                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.StdCtrls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  System.Types,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Styles.ScrollBarWnd,
  Vcl.Styles.ControlWnd;

type
  TStaticTextWnd = class(TControlWnd)
  private
    FStaticBrush: HBRUSH;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TCheckBoxState = (cbUnchecked, cbChecked, cbGrayed);
  TCheckBoxTextWnd = class(TControlWnd)
  private
    FPressed : Boolean;
    function GetDrawState(State: TCheckBoxState): TThemedButton;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TEditTextWnd = class(TControlWnd)
  private
    FBrush: TBrush;
    FFontColor : TColor;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TMemoWnd = class(TScrollBarWnd)
  private
    FBrush: TBrush;
    FFontColor : TColor;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TListBoxWnd = class(TScrollBarWnd)
  private
    FBrush: TBrush;
    FFontColor : TColor;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TNewCheckListBoxWnd = class(TScrollBarWnd)
  private
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

  TRichEditViewerWnd = class(TScrollBarWnd)
  private
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    destructor Destroy; override;
  end;

implementation

uses
  {$IFDEF DEBUG}
  System.IOUtils,
  System.SysUtils,
  {$ENDIF}
  System.UITypes,
  System.Classes;

{ TEditWnd }
type
  TStaticBorderStyle = (sbsNone, sbsSingle, sbsSunken);

procedure Frame3D(Canvas: TCanvas; var Rect: TRect; TopColor, BottomColor: TColor;
  Width: Integer);

  procedure DoRect;
  var
    TopRight, BottomLeft: TPoint;
  begin
    with Canvas, Rect do
    begin
      TopRight.X := Right;
      TopRight.Y := Top;
      BottomLeft.X := Left;
      BottomLeft.Y := Bottom;
      Pen.Color := TopColor;
      PolyLine([BottomLeft, TopLeft, TopRight]);
      Pen.Color := BottomColor;
      Dec(BottomLeft.X);
      PolyLine([TopRight, BottomRight, BottomLeft]);
    end;
  end;

begin
  Canvas.Pen.Width := 1;
  Dec(Rect.Bottom); Dec(Rect.Right);
  while Width > 0 do
  begin
    Dec(Width);
    DoRect;
    InflateRect(Rect, -1, -1);
  end;
  Inc(Rect.Bottom); Inc(Rect.Right);
end;

constructor TStaticTextWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FStaticBrush := 0;
end;



procedure TStaticTextWnd.WndProc(var Message: TMessage);
const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  States: array[Boolean] of TThemedTextLabel = (ttlTextLabelDisabled, ttlTextLabelNormal);

Borders: array[TStaticBorderStyle] of DWORD = (0, WS_BORDER, SS_SUNKEN);
var
  Msg: UINT;
  DC: HDC;
  LDetails: TThemedElementDetails;
  LCanvas : TCanvas;
  LRect: TRect;
  lpPaint: TPaintStruct;
  LBuffer: TBitmap;
  C1, C2: TColor;
begin
  Msg := Message.Msg;
  case Msg of
    WM_NCPAINT:
      begin
          LCanvas := TCanvas.Create;
          try
            LCanvas.Handle := GetWindowDC(Handle);

            LRect := Rect(0, 0, Self.Width, Self.Height);
            if (LRect.Width = 0) or (LRect.Height = 0) then Exit;
              if not ((Self.Style and  WS_BORDER) = WS_BORDER) or ((Self.Style and  SS_SUNKEN) = SS_SUNKEN)  then
              Exit;

            LBuffer := TBitMap.Create;
            try
              LBuffer.Width := LRect.Width;
              LBuffer.Height := LRect.Height;

              C1 := StyleServices.ColorToRGB(clBtnShadow);
              if (Self.Style and  SS_SUNKEN) = SS_SUNKEN then
                C2 := StyleServices.ColorToRGB(clBtnHighLight)
              else
                C2 := C1;
              Frame3D(LBuffer.Canvas, LRect, C1, C2, 1);
              ExcludeClipRect(LCanvas.Handle, 1, 1, Self.Width - 1, Self.Height - 1);
              LCanvas.Draw(0, 0, LBuffer);
            finally
              LBuffer.Free;
            end;
          finally
            ReleaseDC(Handle, LCanvas.Handle);
            LCanvas.Handle := 0;
            LCanvas.Free;
          end;
      end;


    WM_PAINT:
      begin
        DC := HDC(Message.WParam);
        LCanvas := TCanvas.Create;
        try
          if DC <> 0 then
            LCanvas.Handle := DC
          else
            LCanvas.Handle := BeginPaint(Handle, lpPaint);

          LRect := Self.ClientRect;

//          if (Self.ExStyle and  WS_EX_TRANSPARENT) = WS_EX_TRANSPARENT then
//          begin
//            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
//            StyleServices.DrawParentBackground(Handle, LCanvas.Handle, LDetails, False);
//            LCanvas.Brush.Style := bsClear;
//          end
//          else
//          begin
//            LCanvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
//            LCanvas.FillRect(LRect);
//          end;
          LDetails := StyleServices.GetElementDetails(States[Self.Enabled]);

          if (Style and SS_CENTER = SS_CENTER) then
           DrawControlText(LCanvas, LDetails, Text, LRect, DT_CENTER or DT_WORDBREAK)
          else
          if (Style and SS_RIGHT = SS_RIGHT) then
           DrawControlText(LCanvas, LDetails, Text, LRect, DT_RIGHT or DT_WORDBREAK)
          else
           DrawControlText(LCanvas, LDetails, Text, LRect, DT_LEFT or DT_WORDBREAK);

          if DC = 0 then
            EndPaint(Handle, lpPaint);
        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

{ TCheckBoxTextWnd }

constructor TCheckBoxTextWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FPressed:=False;
end;

function TCheckBoxTextWnd.GetDrawState(State: TCheckBoxState): TThemedButton;
begin
  Result := tbButtonDontCare;

  if not Enabled then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedDisabled;
      cbChecked: Result := tbCheckBoxCheckedDisabled;
      cbGrayed: Result := tbCheckBoxMixedDisabled;
    end
  else if FPressed and MouseInControl then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedPressed;
      cbChecked: Result := tbCheckBoxCheckedPressed;
      cbGrayed: Result := tbCheckBoxMixedPressed;
    end
  else if MouseInControl then
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedHot;
      cbChecked: Result := tbCheckBoxCheckedHot;
      cbGrayed: Result := tbCheckBoxMixedHot;
    end
  else
    case State of
      cbUnChecked: Result := tbCheckBoxUncheckedNormal;
      cbChecked: Result := tbCheckBoxCheckedNormal;
      cbGrayed: Result := tbCheckBoxMixedNormal;
    end;
end;

procedure TCheckBoxTextWnd.WndProc(var Message: TMessage);
var
  Msg: UINT;
  DC: HDC;
  LDetails: TThemedElementDetails;
  LCanvas : TCanvas;
  LRect: TRect;
  lpPaint: TPaintStruct;
  State: TCheckBoxState;
  Spacing : Integer;
  BoxSize : TPoint;
  LCaption: string;
begin
  Msg := Message.Msg;
  case Msg of

     WM_LBUTTONUP,
     WM_LBUTTONDOWN : begin
                        SetRedraw(False);
                        CallOrgWndProc(Message);
                        SetRedraw(True);
                        FPressed := (Msg=WM_LBUTTONDOWN);
                        Invalidate;
                      end;
    BM_SETCHECK,
    WM_LBUTTONDBLCLK :begin
                        SetRedraw(False);
                        CallOrgWndProc(Message);
                        SetRedraw(True);
                        Invalidate;
                      end;

    WM_KEYUP,
    WM_KEYDOWN       :begin
                          if TWMKeyDown(Message).CharCode = VK_SPACE then
                            SetRedraw(False);
                          CallOrgWndProc(Message);
                          if TWMKeyDown(Message).CharCode = VK_SPACE then
                          begin
                            SetRedraw(True);
                            Invalidate;
                          end;
                      end;

    WM_PAINT:
      begin
        DC := HDC(Message.WParam);
        LCanvas := TCanvas.Create;
        try
          if DC <> 0 then
            LCanvas.Handle := DC
          else
            LCanvas.Handle := BeginPaint(Handle, lpPaint);

          //background
          LDetails.Element := teButton;
          if StyleServices.HasTransparentParts(LDetails) then
              StyleServices.DrawParentBackground(Handle, LCanvas.Handle, LDetails, False);

          //paint
          State := TCheckBoxState(SendMessage(Handle, BM_GETCHECK, 0, 0));
          LDetails := StyleServices.GetElementDetails(GetDrawState(State));

          Spacing := 5;
          LRect := ClientRect;
          BoxSize := Point(13, 13);
          if not (Style and BS_RIGHTBUTTON = BS_RIGHTBUTTON) then
          begin
            LRect := Rect(0, 0, BoxSize.X, BoxSize.Y);
            RectVCenter(LRect, Rect(0, 0, Width, Height));
          end
          else
          begin
            LRect := Rect(Width - BoxSize.X - 1, 0, Width, Height);
            RectVCenter(LRect, Rect(Width - BoxSize.X - 1, 0, Width, Height));
          end;

          StyleServices.DrawElement(LCanvas.Handle, LDetails, LRect);
          LCanvas.Font := Self.Font;

          LRect := Rect(0, 0, Width - BoxSize.X - 10, Height);
          LCaption := Text;
          DrawText(LCanvas.Handle, PWideChar(LCaption), Length(LCaption), LRect, (DT_CALCRECT or DT_EXPANDTABS));

          if not (Style and BS_RIGHTBUTTON = BS_RIGHTBUTTON) then
            RectVCenter(LRect, Rect(BoxSize.X + Spacing, 0, Width, Height));

          DrawControlText(LCanvas, LDetails, LCaption, LRect, (DT_LEFT or DT_VCENTER or DT_EXPANDTABS));

          if Focused then
          begin
            InflateRect(LRect, 2, 1);
            if LRect.Top < 0 then
              LRect.Top := 0;
            if LRect.Bottom > Height then
              LRect.Bottom := Height;
            LCanvas.Brush.Color := StyleServices.GetSystemColor(clBtnFace);
            LCanvas.DrawFocusRect(LRect);
          end;


          if DC = 0 then
            EndPaint(Handle, lpPaint);
        finally
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;


{ TEditTextWnd }

constructor TEditTextWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FBrush:=TBrush.Create;
  FBrush.Color := StyleServices.GetStyleColor(scWindow);
  FFontColor := StyleServices.GetSystemColor(clWindowText);
end;

destructor TEditTextWnd.Destroy;
begin
  FBrush.Free;
  inherited;
end;

procedure TEditTextWnd.WndProc(var Message: TMessage);
const
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);

var
  Msg: UINT;
  LCalcSize_Params: PNCCalcSizeParams;
  LDetails: TThemedElementDetails;
  LCanvas : TCanvas;
  LRect: TRect;
begin
  Msg := Message.Msg;
  case Msg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        FBrush.Color := StyleServices.GetStyleColor(ColorStates[Self.Enabled]);
        FFontColor := StyleServices.GetStyleFontColor(FontColorStates[Enabled]);
      end;

    WM_NCCALCSIZE:
      begin
        LCalcSize_Params := TWMNCCalcSize(Message).CalcSize_Params;
        Inc(LCalcSize_Params^.rgrc[0].Left, 2);
        Inc(LCalcSize_Params^.rgrc[0].Top, 2);
        Dec(LCalcSize_Params^.rgrc[0].Right, 2);
        Dec(LCalcSize_Params^.rgrc[0].Bottom, 2);
      end;

    WM_NCPAINT:
      begin
        LCanvas := TCanvas.Create;
        try
          LCanvas.Handle := GetWindowDC(Handle);
          LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
          LRect := Rect(0, 0, Width, Height);
          InflateRect(LRect, -2, -2);
          ExcludeClipRect(LCanvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
          StyleServices.DrawElement(LCanvas.Handle, LDetails, Rect(0, 0, Width, Height));
        finally
          ReleaseDC(Handle, LCanvas.Handle);
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;

      end;

    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
      with Message do
        Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);

    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FFontColor));
        SetBkColor(Message.WParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

{ TMemoWnd }

constructor TMemoWnd.Create(AHandle: THandle);
begin
  inherited ;
  FBrush:=TBrush.Create;
  FBrush.Color := StyleServices.GetStyleColor(scWindow);
  FFontColor := StyleServices.GetSystemColor(clWindowText);
end;

destructor TMemoWnd.Destroy;
begin
  FBrush.Free;
  inherited;
end;

procedure TMemoWnd.WndProc(var Message: TMessage);
const
  ColorStates: array[Boolean] of TStyleColor = (scEditDisabled, scEdit);
  FontColorStates: array[Boolean] of TStyleFont = (sfEditBoxTextDisabled, sfEditBoxTextNormal);

var
  Msg: UINT;
  LCalcSize_Params: PNCCalcSizeParams;
  LDetails: TThemedElementDetails;
  LCanvas : TCanvas;
  LRect: TRect;
begin
  Msg := Message.Msg;
  case Msg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        FBrush.Color := StyleServices.GetStyleColor(ColorStates[Self.Enabled]);
        FFontColor := StyleServices.GetStyleFontColor(FontColorStates[Enabled]);
      end;

    WM_NCCALCSIZE:
      begin
        LCalcSize_Params := TWMNCCalcSize(Message).CalcSize_Params;
        Inc(LCalcSize_Params^.rgrc[0].Left, 2);
        Inc(LCalcSize_Params^.rgrc[0].Top, 2);
        Dec(LCalcSize_Params^.rgrc[0].Right, 2);
        Dec(LCalcSize_Params^.rgrc[0].Bottom, 2);
      end;

    WM_NCPAINT:
      begin
        LCanvas := TCanvas.Create;
        try
          LCanvas.Handle := GetWindowDC(Handle);
          LDetails := StyleServices.GetElementDetails(teEditBorderNoScrollNormal);
          LRect := Rect(0, 0, Width, Height);
          InflateRect(LRect, -2, -2);
          ExcludeClipRect(LCanvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
          StyleServices.DrawElement(LCanvas.Handle, LDetails, Rect(0, 0, Width, Height));
        finally
          ReleaseDC(Handle, LCanvas.Handle);
          LCanvas.Handle := 0;
          LCanvas.Free;
        end;

      end;

    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
      with Message do
        Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);

    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FFontColor));
        SetBkColor(Message.WParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
      end;

  else
      //Inherited;
      Message.Result := CallOrgWndProc(Message);
  end;
end;

{ TListBoxWnd }

constructor TListBoxWnd.Create(AHandle: THandle);
begin
  inherited ;
  FBrush:=TBrush.Create;
  FBrush.Color := StyleServices.GetStyleColor(scListBox);
  FFontColor := StyleServices.GetStyleFontColor(sfListItemTextNormal);
  //Addlog('TListBoxWnd Create');
end;

destructor TListBoxWnd.Destroy;
begin
  FBrush.Free;
  inherited;
end;

procedure TListBoxWnd.WndProc(var Message: TMessage);
const
  ColorStates: array[Boolean] of TStyleColor = (scListBoxDisabled, scListBox);
  FontColorStates: array[Boolean] of TStyleFont = (sfListItemTextDisabled, sfListItemTextNormal);
var
  Msg: UINT;
begin
  Msg := Message.Msg;
  case Msg of

    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
      with Message do
        Result := SendMessage(LParam, CN_BASE + Msg, WParam, LParam);


    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(FBrush.Color));
        Message.Result := LRESULT(FBrush.Handle);
      end;

    CM_ENABLEDCHANGED:
      begin
        FBrush.Color := StyleServices.GetStyleColor(ColorStates[Self.Enabled]);
        FFontColor := StyleServices.GetStyleFontColor(FontColorStates[Self.Enabled]);
      end;

    WM_SETFOCUS,
    WM_KILLFOCUS:
      begin
        CallOrgWndProc(Message);
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE or RDW_UPDATENOW);
      end;

    WM_ERASEBKGND :
      begin
          Message.Result:=1;
      end;

    WM_NCPAINT:   //remove when scrrollbars code was actived.
      begin
        PaintBorder(True);
      end;
  else
      //inherited WndProc(Message);
      Message.Result := CallOrgWndProc(Message);
  end;
end;

{ TNewCheckListBoxWnd }

constructor TNewCheckListBoxWnd.Create(AHandle: THandle);
begin
  inherited;

end;

destructor TNewCheckListBoxWnd.Destroy;
begin

  inherited;
end;

procedure TNewCheckListBoxWnd.WndProc(var Message: TMessage);
var
  Msg: UINT;
begin
  Msg := Message.Msg;
  case Msg of

    WM_NCPAINT:   //remove when scrrollbars code was actived.
      begin
        PaintBorder(True);
      end;
  else
      //inherited WndProc(Message);
      Message.Result := CallOrgWndProc(Message);
  end;
end;

{ TRichEditViewerWnd }

constructor TRichEditViewerWnd.Create(AHandle: THandle);
begin
  inherited;

end;

destructor TRichEditViewerWnd.Destroy;
begin

  inherited;
end;

procedure TRichEditViewerWnd.WndProc(var Message: TMessage);
var
  Msg: UINT;
begin
  Msg := Message.Msg;
  case Msg of

    WM_NCPAINT:   //remove when scrrollbars code was actived.
      begin
        PaintBorder(True);
      end;
  else
      //inherited WndProc(Message);
      Message.Result := CallOrgWndProc(Message);
  end;
end;
end.
