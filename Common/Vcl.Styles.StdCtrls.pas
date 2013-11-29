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


implementation

uses
  System.Classes,
  Vcl.Graphics;

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

          if (Self.ExStyle and  WS_EX_TRANSPARENT) = WS_EX_TRANSPARENT then
          begin
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
            StyleServices.DrawParentBackground(Handle, LCanvas.Handle, LDetails, False);
            LCanvas.Brush.Style := bsClear;
          end
          else
          begin
            LCanvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
            LCanvas.FillRect(LRect);
          end;
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
  inherited;
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

end.
