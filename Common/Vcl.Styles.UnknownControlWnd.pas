{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.UnknownControlWnd                                                                }
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
{ The Original Code is uUnknownControlWnd.pas.                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.UnknownControlWnd;

interface

uses
  Winapi.Windows,
  Winapi.Messages,



  Vcl.Themes,
  Vcl.Styles.ControlWnd;

type
  TUnknownControlWnd = class(TControlWnd)
    procedure WndProc(var Message: TMessage); override;
  end;

implementation

{ TUnknownControlWnd }

procedure TUnknownControlWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  DC: HDC;
  LDetails: TThemedElementDetails;
  PS: TPaintStruct;
begin
  uMsg := Message.Msg;
  case uMsg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_PAINT:
      begin
        BeginPaint(Handle, PS);
        try
          DC := GetDC(Handle);
          try
            Message.Result := CallOrgWndProc(Message);
            { if its ScrollBar (Open/Save Dialogs) }
            if (Style and SBS_SIZEBOX = SBS_SIZEBOX) or
              (Style and SBS_SIZEBOXBOTTOMRIGHTALIGN = SBS_SIZEBOXBOTTOMRIGHTALIGN)
              or (Style and SBS_SIZEBOXTOPLEFTALIGN = SBS_SIZEBOXTOPLEFTALIGN) then
              begin
                FillRectangle(DC, ClientRect, StyleServices.GetStyleColor(scWindow));
                LDetails := StyleServices.GetElementDetails(TThemedScrollBar.tsSizeBoxRightAlign);
                StyleServices.DrawElement(DC, LDetails, ClientRect);
              end;
          finally
             ReleaseDC(Handle, DC);
          end;
        finally
          EndPaint(Handle, PS);
        end;
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
