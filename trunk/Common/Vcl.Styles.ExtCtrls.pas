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
  R: TRect;
  LDetails: TThemedElementDetails;
  CSP: PNCCalcSizeParams;
  Rect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  TopColor, BottomColor: TColor;
  BaseColor, BaseTopColor, BaseBottomColor: TColor;
  LCanvas:TCanvas;
  Flags: Longint;
begin

  uMsg := Message.Msg;

  case uMsg of

    WM_DESTROY:
      begin
        if FStaticBrush<>0 then
         DeleteObject(FStaticBrush);
        Message.Result := CallOrgWndProc(Message);
      end;


    WM_CTLCOLORSTATIC :
     begin
        LDetails := LStyle.GetElementDetails(tpPanelBackground);
        {
        if LStyle.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then
          BaseColor := LColor;
        }
        BaseColor:=clRed;
        SetBkColor(Message.WParam, BaseColor);

        if FStaticBrush = 0 then
          FStaticBrush := CreateSolidBrush(BaseColor);

        Message.Result := FStaticBrush;
//
//        if FStaticBrush = 0 then
//          FStaticBrush := CreateSolidBrush(sColor);
//
//        if IsWindowEnabled(LPARAM) then
//          LDetails := StyleServices.GetElementDetails
//            (TThemedTextLabel.ttlTextLabelNormal)
//        else
//          LDetails := StyleServices.GetElementDetails
//            (TThemedTextLabel.ttlTextLabelDisabled);
//
//        StyleServices.GetElementColor(LDetails, ecTextColor, sColor);
//
//        { Change Static Control Text Color }
//        SetTextColor(WPARAM, sColor);
//        { Transparent Static BackGound }
//        Message.Result := FStaticBrush;

     end;
//    WM_PAINT:
//      begin
//          LDetails := LStyle.GetElementDetails(tpPanelBackground);
//          if LStyle.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then
//            BaseColor := LColor;
//          LDetails := LStyle.GetElementDetails(tpPanelBevel);
//          if LStyle.GetElementColor(LDetails, ecEdgeHighLightColor, LColor) and (LColor <> clNone) then
//            BaseTopColor := LColor;
//          if LStyle.GetElementColor(LDetails, ecEdgeShadowColor, LColor) and (LColor <> clNone) then
//            BaseBottomColor := LColor;
//
//          DC := GetWindowDC(Handle);
//          try
//                                {
//            if BevelOuter <> bvNone then
//            begin
//              AdjustColors(BevelOuter);
//              Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
//            end;
//            if not (LStyle.Enabled and (csParentBackground in ControlStyle)) then
//              Frame3D(Canvas, Rect, BaseColor, BaseColor, BorderWidth)
//            else
//              InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));
//            if BevelInner <> bvNone then
//            begin
//              AdjustColors(BevelInner);
//              Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
//            end;
//                                     }
//            LCanvas:=TCanvas.Create;
//            try
//              LCanvas.Handle:=DC;
//              LCanvas.Brush.Style := bsSolid;
//              LCanvas.Brush.Color := clred;//BaseColor;
//              LCanvas.FillRect(ClientRect);
//            finally
//              LCanvas.Free;
//            end;
//          finally
//             ReleaseDC(Handle, DC);
//          end;
//
//      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
