{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.PlatformVclStylesActnCtrls                                                              }
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
{ The Original Code is Vcl.PlatformVclStylesActnCtrls                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.PlatformVclStylesActnCtrls;

interface

uses
   Vcl.ActnMan,
   Vcl.PlatformDefaultStyleActnCtrls;

type
  TPlatformVclStylesStyle = class(TPlatformDefaultStyleActionBars)
  public
    function GetControlClass(ActionBar: TCustomActionBar; AnItem: TActionClientItem): TCustomActionControlClass; override;
    function GetStyleName: string; override;
  end;

var
  PlatformVclStylesStyle: TPlatformVclStylesStyle;

implementation

uses
  Vcl.Menus,
  Winapi.Windows,
  System.SysUtils,
  Vcl.ActnMenus,
  Vcl.ActnCtrls,
  Vcl.ThemedActnCtrls,
  Vcl.Forms,
  Vcl.ListActns,
  Vcl.ActnColorMaps,
  Vcl.Themes,
  Vcl.XPActnCtrls,
  Vcl.StdActnMenus,
  Vcl.Graphics;

type
  TActionControlStyle = (csStandard, csXPStyle, csThemed);

  TThemedMenuItemEx = class(Vcl.ThemedActnCtrls.TThemedMenuItem)
  private
    procedure NativeDrawText(DC: HDC; const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawText(var Rect: TRect; var Flags: Cardinal; Text: string); override;
  end;

  TThemedMenuButtonEx = class(Vcl.ThemedActnCtrls.TThemedMenuButton)
  private
    procedure NativeDrawText(const Text: string; var Rect: TRect; Flags: Longint);
  protected
    procedure DrawText(var ARect: TRect; var Flags: Cardinal;
      Text: string); override;
  end;

  TThemedMenuItemHelper = class Helper for TThemedMenuItem
  private
   function GetPaintRect: TRect;
   property PaintRect: TRect read GetPaintRect;
  end;

{ TThemedMenuItemHelper }
function TThemedMenuItemHelper.GetPaintRect: TRect;
begin
 Result:=Self.FPaintRect;
end;

function GetActionControlStyle: TActionControlStyle;
begin
  if TStyleManager.IsCustomStyleActive then
    Result := csThemed
  else
  if TOSVersion.Check(6) then
  begin
    if StyleServices.Theme[teMenu] <> 0 then
      Result := csThemed
    else
      Result := csXPStyle;
  end
  else
  if TOSVersion.Check(5, 1) then
    Result := csXPStyle
  else
    Result := csStandard;
end;

{ TPlatformDefaultStyleActionBarsStyle }

function TPlatformVclStylesStyle.GetControlClass(ActionBar: TCustomActionBar;
  AnItem: TActionClientItem): TCustomActionControlClass;
begin
  if ActionBar is TCustomActionToolBar then
  begin
    if AnItem.HasItems then
      case GetActionControlStyle of
        csStandard: Result := TStandardDropDownButton;
        csXPStyle: Result := TXPStyleDropDownBtn;
      else
        Result := TThemedDropDownButton;
      end
    else
    if (AnItem.Action is TStaticListAction) or (AnItem.Action is TVirtualListAction) then
      Result := TCustomComboControl
    else
    case GetActionControlStyle of
      csStandard: Result := TStandardButtonControl;
      csXPStyle: Result := TXPStyleButton;
    else
      Result := TThemedButtonControl;
    end
  end
  else
  if ActionBar is TCustomActionMainMenuBar then
    case GetActionControlStyle of
      csStandard: Result := TStandardMenuButton;
      csXPStyle: Result := TXPStyleMenuButton;
    else
      Result := TThemedMenuButtonEx;
    end
  else
  if ActionBar is TCustomizeActionToolBar then
  begin
    with TCustomizeActionToolbar(ActionBar) do
      if not Assigned(RootMenu) or (AnItem.ParentItem <> TCustomizeActionToolBar(RootMenu).AdditionalItem) then
        case GetActionControlStyle of
          csStandard: Result := TStandardMenuItem;
          csXPStyle: Result := TXPStyleMenuItem;
        else
          Result := TThemedMenuItemEx;
        end
      else
      case GetActionControlStyle of
          csStandard: Result := TStandardAddRemoveItem;
          csXPStyle: Result := TXPStyleAddRemoveItem;
      else
          Result := TThemedAddRemoveItem;
      end
  end
  else
  if ActionBar is TCustomActionPopupMenu then
    case GetActionControlStyle of
      csStandard: Result := TStandardMenuItem;
      csXPStyle: Result := TXPStyleMenuItem;
    else
      Result := TThemedMenuItemEx;
    end
  else
  case GetActionControlStyle of
    csStandard: Result := TStandardButtonControl;
    csXPStyle: Result := TXPStyleButton;
  else
    Result := TThemedButtonControl;
  end
end;

function TPlatformVclStylesStyle.GetStyleName: string;
begin
  Result := 'Platform VclStyles Style';
end;

{ TThemedMenuItemEx }

procedure TThemedMenuItemEx.NativeDrawText(DC: HDC; const Text: string;
  var Rect: TRect; Flags: Integer);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmPopupItemDisabled, tmPopupItemNormal);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LNativeStyle : TCustomStyleServices;
begin
  LNativeStyle:=TStyleManager.SystemStyle;

  LFormats := TTextFormatFlags(Flags);
  if Selected and Enabled then
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot)
  else
    LDetails := StyleServices.GetElementDetails(MenuStates[Enabled or ActionBar.DesignMode]);

  if not StyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  LNativeStyle.DrawText(DC, LDetails, LCaption, Rect, LFormats, LColor);
end;

procedure TThemedMenuItemEx.DrawText(var Rect: TRect; var Flags: Cardinal;
  Text: string);
var
  LRect: TRect;
begin
  if Selected and Enabled then
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupItemHot), PaintRect)
  else if Selected then
    StyleServices.DrawElement(Canvas.Handle, StyleServices.GetElementDetails(tmPopupItemDisabledHot), PaintRect);

  if (Parent is TCustomActionBar) and (not ActionBar.PersistentHotkeys) then
    Text := FNoPrefix;
  Canvas.Font := Screen.MenuFont;

  if ActionClient.Default then
    Canvas.Font.Style := Canvas.Font.Style + [fsBold];

  LRect := PaintRect;
  NativeDrawText(Canvas.Handle, Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  OffsetRect(LRect, Rect.Left,
    ((PaintRect.Bottom - PaintRect.Top) - (LRect.Bottom - LRect.Top)) div 2);
  NativeDrawText(Canvas.Handle, Text, LRect, Flags);

  if ShowShortCut and ((ActionClient <> nil) and not ActionClient.HasItems) then
  begin
    Flags := DrawTextBiDiModeFlags(DT_RIGHT);
    LRect := TRect.Create(ShortCutBounds.Left, LRect.Top, ShortCutBounds.Right, LRect.Bottom);
    LRect.Offset(Width, 0);
    NativeDrawText(Canvas.Handle, ActionClient.ShortCutText, LRect, Flags);
  end;
end;

{ TThemedMenuButtonEx }
procedure TThemedMenuButtonEx.NativeDrawText(const Text: string; var Rect: TRect;
  Flags: Integer);
const
  MenuStates: array[Boolean] of TThemedMenu = (tmMenuBarItemNormal, tmMenuBarItemHot);
var
  LCaption: string;
  LFormats: TTextFormat;
  LColor: TColor;
  LDetails: TThemedElementDetails;
  LNativeStyle : TCustomStyleServices;
begin
  LNativeStyle:=TStyleManager.SystemStyle;

  LFormats := TTextFormatFlags(Flags);
  if Enabled then
    LDetails := StyleServices.GetElementDetails(MenuStates[Selected or MouseInControl or ActionBar.DesignMode])
  else
    LDetails := StyleServices.GetElementDetails(tmMenuBarItemDisabled);

  Canvas.Brush.Style := bsClear;
  if Selected then
    Canvas.Font.Color := clHighlightText
  else
    Canvas.Font.Color := clMenuText;

  if not StyleServices.GetElementColor(LDetails, ecTextColor, LColor) or (LColor = clNone) then
    LColor := ActionBar.ColorMap.FontColor;

  LCaption := Text;
  if (tfCalcRect in LFormats) and ( (LCaption = '') or (LCaption[1] = cHotkeyPrefix) and (LCaption[2] = #0) ) then
    LCaption := LCaption + ' ';

  if Enabled then
    LDetails := StyleServices.GetElementDetails(MenuStates[Selected or MouseInControl]);

  LNativeStyle.DrawText(Canvas.Handle, LDetails, LCaption, Rect, LFormats, LColor);
end;

procedure TThemedMenuButtonEx.DrawText(var ARect: TRect; var Flags: Cardinal;
  Text: string);
var
  LRect: TRect;
begin
  if Parent is TCustomActionMainMenuBar then
    if not TCustomActionMainMenuBar(Parent).PersistentHotkeys then
      Text := StripHotkey(Text);

  LRect := ARect;
  Inc(LRect.Left);
  Canvas.Font := Screen.MenuFont;
  NativeDrawText(Text, LRect, Flags or DT_CALCRECT or DT_NOCLIP);
  NativeDrawText(Text, LRect, Flags);
end;

initialization
  PlatformVclStylesStyle := TPlatformVclStylesStyle.Create;
  DefaultActnBarStyle := PlatformVclStylesStyle.GetStyleName;
  RegisterActnBarStyle(PlatformVclStylesStyle);
finalization
  UnregisterActnBarStyle(PlatformVclStylesStyle);
  PlatformVclStylesStyle.Free;
end.

