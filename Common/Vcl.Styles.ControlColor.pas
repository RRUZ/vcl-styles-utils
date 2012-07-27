{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ControlColor                                                                     }
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
{ The Original Code is Vcl.Styles.ControlColor                                                     }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ControlColor;

interface

uses
 Vcl.StdCtrls,
 Vcl.Controls,
 Winapi.Messages;

type
  TEditStyleHookColor = class(TEditStyleHook)
  private
    procedure UpdateColors;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

  TMemoStyleHookColor = class(TMemoStyleHook)
  private
    procedure UpdateColors;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AControl: TWinControl); override;
  end;

implementation

Uses
  Winapi.Windows,
  Vcl.Graphics,
  Vcl.Themes,
  Vcl.Styles;


type
 TWinControlClass= class(TWinControl);


constructor TEditStyleHookColor.Create(AControl: TWinControl);
begin
  inherited;
  UpdateColors;
end;

procedure TEditStyleHookColor.UpdateColors;
var
  LStyle: TCustomStyleServices;
begin
 if Control.Enabled then
 begin
  Brush.Color := TWinControlClass(Control).Color;
  FontColor   := TWinControlClass(Control).Font.Color;
 end
 else
 begin
  LStyle := StyleServices;
  Brush.Color := LStyle.GetStyleColor(scEditDisabled);
  FontColor := LStyle.GetStyleFontColor(sfEditBoxTextDisabled);
 end;
end;

procedure TEditStyleHookColor.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        UpdateColors;
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False;
      end
  else
    inherited WndProc(Message);
  end;
end;

{ TMemoStyleHookColor }

constructor TMemoStyleHookColor.Create(AControl: TWinControl);
begin
  inherited;
  UpdateColors;
end;

procedure TMemoStyleHookColor.UpdateColors;
var
  LStyle: TCustomStyleServices;
begin
 if Control.Enabled then
 begin
  Brush.Color := TWinControlClass(Control).Color;
  FontColor   := TWinControlClass(Control).Font.Color;
 end
 else
 begin
  LStyle := StyleServices;
  Brush.Color := LStyle.GetStyleColor(scEditDisabled);
  FontColor := LStyle.GetStyleFontColor(sfEditBoxTextDisabled);
 end;
end;

procedure TMemoStyleHookColor.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    CN_CTLCOLORMSGBOX..CN_CTLCOLORSTATIC:
      begin
        UpdateColors;
        SetTextColor(Message.WParam, ColorToRGB(FontColor));
        SetBkColor(Message.WParam, ColorToRGB(Brush.Color));
        Message.Result := LRESULT(Brush.Handle);
        Handled := True;
      end;

    CM_COLORCHANGED,
    CM_ENABLEDCHANGED:
      begin
        UpdateColors;
        Handled := False;
      end
  else
    inherited WndProc(Message);
  end;
end;

end.
