{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ToolTipsWnd                                                                      }
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
{ The Original Code is uTooltipsWnd.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ToolTipsWnd;

interface

uses
  Vcl.Styles.ControlWnd,
  Winapi.Messages;

type
  TooltipsWnd = class(TControlWnd)
  private
    procedure PaintHint;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

uses
  System.Types,
  Vcl.GraphUtil,
  Winapi.CommCtrl,
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Themes;


{ TooltipsWnd }

constructor TooltipsWnd.Create(AHandle: THandle);
begin
  inherited;
end;

procedure TooltipsWnd.PaintHint;
var
  DC: HDC;
  LDetails: TThemedElementDetails;
  BkColor, GradientStartColor, GradientEndColor, TextColor, LColor: TColor;
  Canvas: TCanvas;
  R: TRect;
  SaveIndex: integer;
  Brush: HBRUSH;
begin
  DC := GetDC(Handle);
  SaveIndex := SaveDC(DC);
  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;

    BkColor := $00767676;
    GradientStartColor := clWhite;
    GradientEndColor := $EFE4E3;
    TextColor := $00575757;

    if StyleServices.Enabled then
      begin
        LDetails := StyleServices.GetElementDetails(thHintBalloon);
        if StyleServices.GetElementColor(LDetails, ecBorderColor, LColor) and
          (LColor <> clNone) then
          BkColor := LColor;
        if StyleServices.GetElementColor(LDetails, ecGradientColor1, LColor) and
          (LColor <> clNone) then
          GradientStartColor := LColor;
        if StyleServices.GetElementColor(LDetails, ecGradientColor2, LColor) and
          (LColor <> clNone) then
          GradientEndColor := LColor;
        if StyleServices.GetElementColor(LDetails, ecTextColor, LColor) and
          (LColor <> clNone) then
          TextColor := LColor;
      end;
    { Draw Tooltips Face }
    GradientFillCanvas(Canvas, GradientStartColor, GradientEndColor, ClientRect,
      gdVertical);
    { Draw Tooltips Border }
    Brush := CreateSolidBrush(BkColor);
    FrameRect(DC, ClientRect, Brush);
    DeleteObject(Brush);
    { Use default font for Tooltips text }
    SelectObject(DC, Screen.HintFont.Handle);
    { Adjust text rectangle }
    R := ClientRect;
    SendMessage(Handle, TTM_ADJUSTRECT, 0, UINT_PTR(@R));
    { Draw Tooltips Text }
    SetBkMode(DC, TRANSPARENT);
    SetTextColor(DC, TextColor);
    DrawText(DC, Text, -1, R, DT_LEFT);

    RestoreDC(DC, SaveIndex);
    ReleaseDC(Handle, DC);
  finally
    Canvas.Free;
  end;
end;

procedure TooltipsWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
begin
  uMsg   := Message.Msg;
  case uMsg of
    WM_PAINT:
      begin
        { It's very important to call the default message. }
        Message.Result := CallOrgWndProc(Message);
        PaintHint;
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
