{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.EditWnd                                                                          }
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
{ The Original Code is uEditWnd.pas.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.EditWnd;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Themes,
  System.Types,
  Vcl.Styles.ControlWnd;

type
  TEditWnd = class(TControlWnd)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

implementation

{ TEditWnd }

constructor TEditWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

procedure TEditWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  lParam: UINT_PTR;
  DC: HDC;
  R: TRect;
  LDetails: TThemedElementDetails;
  CSP: PNCCalcSizeParams;
begin

  uMsg := Message.Msg;
  lParam := Message.lParam;

  case uMsg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or  SWP_FRAMECHANGED);
      end;

    WM_PAINT:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
      end;

    WM_NCCALCSIZE:
      begin
        { Adjust Client Rect in order to paint the Edit Border !! }
        if IsChildIn32770Dialog(Handle) and HasBorder then
          if GetWindowClassName(GetParent(Handle)) <> 'ComboBox' then
            begin
              CSP := PNCCalcSizeParams(lParam);
              with CSP^.rgrc[0] do
                begin
                  inc(Left, 2);
                  inc(Top, 2);
                  Dec(Right, 2);
                  Dec(Bottom, 2);
                end;
            end
          else
            Message.Result := CallOrgWndProc(Message);
      end;

    WM_NCPAINT: { Paint Edit Border }
      begin
        { Check if Edit is not a VCL Edit Control }
        if IsChildIn32770Dialog(Handle) and HasBorder then
          if GetWindowClassName(GetParent(Handle)) <> 'ComboBox' then
            begin
              DC := GetWindowDC(Handle);
              try
                R := ClientRect;
                if IsWindowEnabled(Handle) then
                  LDetails := StyleServices.GetElementDetails
                    (TThemedEdit.teEditTextNormal)
                else
                  LDetails := StyleServices.GetElementDetails
                    (TThemedEdit.teEditTextDisabled);
                R := Rect(0, 0, ClientRect.Width + 4, ClientRect.Height + 4);
                StyleServices.DrawElement(DC, LDetails, R);
              finally
               ReleaseDC(Handle, DC);
              end;

              InvalidateRect(Handle, nil, False);
            end
          else { VCL Edit Control }
            Message.Result := CallOrgWndProc(Message);
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
