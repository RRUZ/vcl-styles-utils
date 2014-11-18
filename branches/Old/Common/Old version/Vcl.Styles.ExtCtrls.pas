{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ExtCtrls                                                                         }
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
{ The Original Code is Vcl.Styles.ExtCtrls.pas.                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ExtCtrls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  System.Types,
  Vcl.Styles.ControlWnd;

type
  TPanelWnd = class(TControlWnd)
  private
    FStaticBrush: HBRUSH;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

//  TNotebookWnd = class(TControlWnd)
//  protected
//    procedure WndProc(var Message: TMessage); override;
//  public
//    constructor Create(AHandle: THandle); override;
//  end;
//
//  TNotebookPagelWnd = class(TControlWnd)
//  protected
//    procedure WndProc(var Message: TMessage); override;
//  public
//    constructor Create(AHandle: THandle); override;
//  end;

implementation

uses
  Vcl.Graphics;

{ TEditWnd }

constructor TPanelWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FStaticBrush := 0;
end;

procedure TPanelWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  DC: HDC;
  LDetails: TThemedElementDetails;
  lpPaint: TPaintStruct;
  Rect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  BaseColor{, BaseTopColor, BaseBottomColor}: TColor;
  LCanvas:TCanvas;
//  Flags: Longint;
//  LCaption : string;
begin

  uMsg := Message.Msg;

  case uMsg of

    WM_PAINT:
      begin
        DC := HDC(Message.WParam);
        LCanvas := TCanvas.Create;
        try
          if DC <> 0 then
            LCanvas.Handle := DC
          else
            LCanvas.Handle := BeginPaint(Handle, lpPaint);

          Rect := Self.ClientRect;

          BaseColor :=  StyleServices.GetSystemColor(clBtnFace);
          {
          BaseTopColor := StyleServices.GetSystemColor(clBtnHighlight);
          BaseBottomColor := StyleServices.GetSystemColor(clBtnShadow);
          }
          LStyle := StyleServices;

          LDetails := LStyle.GetElementDetails(tpPanelBackground);
          if LStyle.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then
            BaseColor := LColor;
          LDetails := LStyle.GetElementDetails(tpPanelBevel);
          {
          if LStyle.GetElementColor(LDetails, ecEdgeHighLightColor, LColor) and (LColor <> clNone) then
            BaseTopColor := LColor;
          if LStyle.GetElementColor(LDetails, ecEdgeShadowColor, LColor) and (LColor <> clNone) then
            BaseBottomColor := LColor;

                   {
          if BevelOuter <> bvNone then
          begin
            AdjustColors(BevelOuter);
            Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
          end;
          if not (LStyle.Enabled and (csParentBackground in ControlStyle)) then
            Frame3D(Canvas, Rect, BaseColor, BaseColor, BorderWidth)
          else
            InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));
          if BevelInner <> bvNone then
          begin
            AdjustColors(BevelInner);
            Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
          end;   }


          with LCanvas do
          begin
            //if not LStyle.Enabled or not ParentBackground then
            //begin
              Brush.Color := BaseColor;
              FillRect(Rect);
            //end;

//            LCaption:=Self.Text;
//            if {FShowCaption and} (LCaption <> '') then
//            begin
//              Brush.Style := bsClear;
//              Font := Self.Font;
//              Flags := DT_EXPANDTABS or DT_SINGLELINE{ or
//                VerticalAlignments[FVerticalAlignment] or Alignments[FAlignment]};
//              //Flags := DrawTextBiDiModeFlags(Flags);
//              if LStyle.Enabled then
//              begin
//                LDetails := LStyle.GetElementDetails(tpPanelBackground);
//                if not LStyle.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
//                  LColor := Font.Color;
//                LStyle.DrawText(Handle, LDetails, LCaption, Rect, TTextFormatFlags(Flags), LColor)
//              end
//              else
//                DrawText(Handle, LCaption, -1, Rect, Flags);
//            end;

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
//
//{ TNotebookPagelWnd }
//
//constructor TNotebookPagelWnd.Create(AHandle: THandle);
//var
//  brush :HBRUSH;
//begin
//  inherited;
//         brush := CreateSolidBrush(RGB(0, 0, 255));
//         SetClassLongPtr(Handle, -10, LONG(brush));
//
//end;
//
//
//procedure TNotebookPagelWnd.WndProc(var Message: TMessage);
//
//      procedure PaintBkgnd(DC: HDC; R: TRect);
//      var
//        LDetails: TThemedElementDetails;
//        ThemeElement: TThemedElement;
//      begin
//        ThemeElement := TThemedElement.teWindow;
//        LDetails := TThemedElementDetails.Create(ThemeElement, 0, 1);
//
//        StyleServices.DrawElement(DC, LDetails, R);
//      end;
//
//var
//  uMsg: UINT;
//begin
//
//  uMsg := Message.Msg;
//
//  case uMsg of
//
//    WM_ERASEBKGND:
//      begin
//        PaintBkgnd(Message.WParam, ClientRect);
//        Message.Result := 1;
//      end;
//
//  else
//    Message.Result := CallOrgWndProc(Message);
//  end;
//end;
//
//{ TNotebooklWnd }
//
//constructor TNotebookWnd.Create(AHandle: THandle);
//begin
//  inherited;
//
//end;
//
//procedure TNotebookWnd.WndProc(var Message: TMessage);
//var
//  uMsg: UINT;
//begin
//  uMsg := Message.Msg;
//  case uMsg of
//    WM_ERASEBKGND:
//      begin
//         CallOrgWndProc(Message);
//      end;
//  else
//    Message.Result := CallOrgWndProc(Message);
//  end;
//end;
end.
