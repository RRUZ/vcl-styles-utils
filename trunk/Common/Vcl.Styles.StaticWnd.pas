{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.StaticWnd                                                                        }
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
{ The Original Code is uStaticWnd.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.StaticWnd;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Styles.ControlWnd;

type
  TStaticWnd = class(TControlWnd)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

uses
  Vcl.Graphics,
  Vcl.Themes,
  System.Classes;

{ TStaticWnd }

constructor TStaticWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

procedure TStaticWnd.WndProc(var Message: TMessage);
const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  States: array [Boolean] of TThemedTextLabel = (ttlTextLabelDisabled,
    ttlTextLabelNormal);
var
  uMsg: UINT;
  DC: HDC;
  LDetails: TThemedElementDetails;
  PS: TPaintStruct;
  LRect: TRect;
  LFont: HFont;
  LCanvas : TCanvas;
  LTextFormat : TTextFormat;
begin
  uMsg := Message.Msg;
  case uMsg of

    WM_PAINT:
      begin
        if (Style and SS_ICON <> SS_ICON) and (Style and SS_BITMAP <> SS_BITMAP)
          and (Style and SS_GRAYRECT <> SS_GRAYRECT) and
          (Style and SS_GRAYFRAME <> SS_GRAYFRAME) then
          begin
            //DC := HDC(Message.WParam);
            SetRedraw(False);
            Message.Result := CallOrgWndProc(Message);
            SetRedraw(True);

            BeginPaint(Handle, PS);
            DC := GetDC(Handle);


            FillRectangle(DC, ClientRect,
              StyleServices.GetStyleColor(scWindow));

            LDetails := StyleServices.GetElementDetails(States[Enabled]);

            LTextFormat := [{tfSingleLine, tfHidePrefix}];
            if Style and SS_LEFT = SS_LEFT then
              include(LTextFormat, tfLeft)
            else
            if Style and SS_RIGHT = SS_RIGHT then
              include(LTextFormat, tfRight)
            else
            if Style and SS_CENTER = SS_CENTER then
              include(LTextFormat, tfCenter);

            if Style and SS_ENDELLIPSIS = SS_ENDELLIPSIS then
              include(LTextFormat, tfEndEllipsis);

            if Style and SS_PATHELLIPSIS = SS_PATHELLIPSIS then
              include(LTextFormat, tfPathEllipsis);

            if Style and SS_WORDELLIPSIS = SS_WORDELLIPSIS then
              include(LTextFormat, tfWordEllipsis);

            if not (Style and SS_ENDELLIPSIS = SS_ENDELLIPSIS) and not (Style and SS_PATHELLIPSIS = SS_PATHELLIPSIS) and not (Style and SS_WORDELLIPSIS = SS_WORDELLIPSIS)  then
              include(LTextFormat, tfWordBreak);

            LRect := ClientRect;

            LFont := Self.Font.Handle; //CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
            SelectObject(DC, LFont);
            StyleServices.DrawText(DC, LDetails, Text, LRect, LTextFormat);
            ReleaseDC(Handle, DC);
            EndPaint(Handle, PS);

          end
        else
          Message.Result := CallOrgWndProc(Message);
      end;

    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
