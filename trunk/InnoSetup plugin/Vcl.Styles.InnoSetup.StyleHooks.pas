{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.InnoSetup.StyleHooks                                                             }
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
{ The Original Code is  Vcl.Styles.InnoSetup.pas.                                                  }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{                                                                                                  }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.InnoSetup.StyleHooks;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  System.SysUtils,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.StdCtrls,
  Vcl.GraphUtil,
  Vcl.Controls;

type
  TRichEditViewerStyleHook = class(TSysScrollingStyleHook)
  protected
    function GetBorderSize: TRect; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

  TNewCheckListBoxStyleHook = class(TSysScrollingStyleHook)
  protected
    function GetBorderSize: TRect; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

   TNewButtonStyleHook = class(TSysButtonStyleHook)
   protected
    function CheckIfParentBkGndPainted: Boolean; override;
   end;

implementation

{ TRichEditViewerStyleHook }

constructor TRichEditViewerStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := False;
end;

function TRichEditViewerStyleHook.GetBorderSize: TRect;
begin
  Result := inherited GetBorderSize;
  if (SysControl.HasBorder) then
    Result := Rect(2, 2, 2, 2);
end;

procedure TRichEditViewerStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND:
      begin
        CallDefaultProc(Message);
        Exit;
      end;
//    CN_CTLCOLORMSGBOX .. CN_CTLCOLORSTATIC:
//      begin
//        SetTextColor(Message.wParam, ColorToRGB(FontColor));
//        SetBkColor(Message.wParam, ColorToRGB(Brush.Color));
//        Message.Result := LRESULT(Brush.Handle);
//      end;
//    CM_ENABLEDCHANGED:
//      begin
//        UpdateColors;
//        CallDefaultProc(Message);
//        // Handled := False; // Allow control to handle message
//      end
  else
    inherited WndProc(Message);
  end;
end;

{ TNewCheckListBoxStyleHook }

constructor TNewCheckListBoxStyleHook.Create(AHandle: THandle);
begin
  inherited;
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := False;
end;

function TNewCheckListBoxStyleHook.GetBorderSize: TRect;
begin
  Result := inherited GetBorderSize;
  if (SysControl.HasBorder) then
  begin
    Result := Rect(2, 2, 2, 2);
  end;
end;

procedure TNewCheckListBoxStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TNewButtonStyleHook }

function TNewButtonStyleHook.CheckIfParentBkGndPainted: Boolean;
begin
  Result:=True;
end;

end.
