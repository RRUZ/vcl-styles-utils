{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ColorTabs                                                                        }
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
{ The Original Code is Vcl.Styles.ColorTabs                                                        }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ColorTabs;

interface

uses
  Vcl.Graphics,
  Winapi.Messages,
  Vcl.ComCtrls;

type
  TTabSheet = class(Vcl.ComCtrls.TTabSheet)
  private
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  end;

  TTabColorControlStyleHook= class(TTabControlStyleHook)
  private
    procedure WMEraseBkgnd(var Message: TMessage); message WM_ERASEBKGND;
  protected
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure DrawTab(Canvas: TCanvas; Index: Integer); override;
  end;

implementation

uses
 System.Classes,
 System.SysUtils,
 Vcl.Styles,
 Vcl.Themes,
 Vcl.Controls,
 Winapi.Windows;

type
  TPageControlHelper = class helper for TPageControl
  public
    procedure UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
  end;

  TWinControlClass = class(TWinControl);
  TCustomTabControlClass = class(TCustomTabControl);

  TTabControlStyleHookHelper = class helper for TTabControlStyleHook
  public
    procedure AngleTextOut2(Canvas: TCanvas; Angle: Integer; X, Y: Integer; const Text: string);
  end;


function GetColorTab(Index : Integer) : TColor;
Const
  MaxColors =9;
  Colors : Array [0..MaxColors-1] of TColor = (6512214,16755712,8355381,1085522,115885,1098495,1735163,2248434,4987610);
begin
  Result:=Colors[Index mod MaxColors];
end;


function GetColorTextTab(ThemedTab  : TThemedTab) : TColor;
Const
 ColorSelected = clYellow;
 ColorHot      = clGray;
 ColorNormal   = clWhite;
begin
    Result:=ColorNormal;
    case ThemedTab of
       ttTabItemSelected,
       ttTabItemLeftEdgeSelected,
       ttTabItemBothEdgeSelected,
       ttTabItemRightEdgeSelected : Result:= ColorSelected;

       ttTabItemHot,
       ttTabItemLeftEdgeHot,
       ttTabItemBothEdgeHot,
       ttTabItemRightEdgeHot      : Result := ColorHot;

       ttTabItemNormal,
       ttTabItemLeftEdgeNormal,
       ttTabItemBothEdgeNormal,
       ttTabItemRightEdgeNormal  : Result := ColorNormal;
    end;
end;


{ TPageControlHelper }

procedure TPageControlHelper.UpdateTab2(Page: Vcl.ComCtrls.TTabSheet);
begin
  Self.UpdateTab(Page);
end;


{ TTabSheet }

procedure TTabSheet.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  LRect  : TRect;
  LSize  : Integer;
  LCanvas: TCanvas;
begin
  if (PageControl <> nil) and StyleServices.Enabled and TStyleManager.IsCustomStyleActive then
    // ((PageControl.Style = tsTabs) or TStyleManager.IsCustomStyleActive) then
  begin
    GetWindowRect(Handle, LRect);
    OffsetRect(LRect, -LRect.Left, -LRect.Top);
    LSize := ClientToParent(Point(0, 0)).X;
    InflateRect(LRect, LSize, LSize); // remove border
    //create a TCanvas for erase the background, using the DC of the message
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := Message.DC;
      LCanvas.Brush.Color:=GetColorTab(TabIndex);
      LCanvas.FillRect(LRect);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;

    Message.Result := 1;
    PageControl.UpdateTab2(PageControl.ActivePage);
  end
  else
    inherited;
end;


{ TTabControlStyleHookHelper }

procedure TTabControlStyleHookHelper.AngleTextOut2(Canvas: TCanvas; Angle, X,
  Y: Integer; const Text: string);
begin
  Self.AngleTextOut(Canvas, Angle, X, Y, Text);
end;


{ TTabColorControlStyleHook }
procedure TTabColorControlStyleHook.DrawTab(Canvas: TCanvas; Index: Integer);
var
  LDetails    : TThemedElementDetails;
  LImageIndex : Integer;
  LThemedTab  : TThemedTab;
  LIconRect   : TRect;
  R, LayoutR  : TRect;
  LImageW, LImageH, DxImage : Integer;
  LTextX, LTextY: Integer;
  LTextColor    : TColor;

    procedure DrawControlText(const S: string; var R: TRect; Flags: Cardinal);
    var
      TextFormat: TTextFormatFlags;
    begin
      Canvas.Font       := TWinControlClass(Control).Font;
      TextFormat        := TTextFormatFlags(Flags);
      Canvas.Font.Color := LTextColor;
      StyleServices.DrawText(Canvas.Handle, LDetails, S, R, TextFormat, Canvas.Font.Color);
    end;

begin
  if (Images <> nil) and (Index < Images.Count) then
  begin
    LImageW := Images.Width;
    LImageH := Images.Height;
    DxImage := 3;
  end
  else
  begin
    LImageW := 0;
    LImageH := 0;
    DxImage := 0;
  end;

  R := TabRect[Index];
  if R.Left < 0 then Exit;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if Index = TabIndex then
      InflateRect(R, 0, 2);
  end
  else if Index = TabIndex then
    Dec(R.Left, 2) else Dec(R.Right, 2);

  Canvas.Font.Assign(TCustomTabControlClass(Control).Font);
  LayoutR := R;
  LThemedTab := ttTabDontCare;
  //Get the type of the active tab
  case TabPosition of
    tpTop:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemHot
        else
          LThemedTab := ttTabItemNormal;
      end;
    tpLeft:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemLeftEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemLeftEdgeHot
        else
          LThemedTab := ttTabItemLeftEdgeNormal;
      end;
    tpBottom:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemBothEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemBothEdgeHot
        else
          LThemedTab := ttTabItemBothEdgeNormal;
      end;
    tpRight:
      begin
        if Index = TabIndex then
          LThemedTab := ttTabItemRightEdgeSelected
        else if (Index = HotTabIndex) and MouseInControl then
          LThemedTab := ttTabItemRightEdgeHot
        else
          LThemedTab := ttTabItemRightEdgeNormal;
      end;
  end;

  //draw the tab
  if StyleServices.Available then
  begin
    LDetails := StyleServices.GetElementDetails(LThemedTab);//necesary for  DrawControlText
    InflateRect(R,-1,0);//adjust the size of the tab creating blanks space between the tabs
    Canvas.Brush.Color:=GetColorTab(Index);
    Canvas.FillRect(R);
  end;

  //get the index of the image (icon)
  if Control is TCustomTabControl then
    LImageIndex := TCustomTabControlClass(Control).GetImageIndex(Index)
  else
    LImageIndex := Index;

  //draw the image
  if (Images <> nil) and (LImageIndex >= 0) and (LImageIndex < Images.Count) then
  begin
    LIconRect := LayoutR;
    case TabPosition of
      tpTop, tpBottom:
        begin
          LIconRect.Left := LIconRect.Left + DxImage;
          LIconRect.Right := LIconRect.Left + LImageW;
          LayoutR.Left := LIconRect.Right;
          LIconRect.Top := LIconRect.Top + (LIconRect.Bottom - LIconRect.Top) div 2 - LImageH div 2;
          if (TabPosition = tpTop) and (Index = TabIndex) then
            OffsetRect(LIconRect, 0, -1)
          else
          if (TabPosition = tpBottom) and (Index = TabIndex) then
            OffsetRect(LIconRect, 0, 1);
        end;
      tpLeft:
        begin
          LIconRect.Bottom := LIconRect.Bottom - DxImage;
          LIconRect.Top := LIconRect.Bottom - LImageH;
          LayoutR.Bottom := LIconRect.Top;
          LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left) div 2 - LImageW div 2;
        end;
      tpRight:
        begin
          LIconRect.Top := LIconRect.Top + DxImage;
          LIconRect.Bottom := LIconRect.Top + LImageH;
          LayoutR.Top := LIconRect.Bottom;
          LIconRect.Left := LIconRect.Left + (LIconRect.Right - LIconRect.Left) div 2 - LImageW div 2;
        end;
    end;
    if StyleServices.Available then
      StyleServices.DrawIcon(Canvas.Handle, LDetails, LIconRect, Images.Handle, LImageIndex);
  end;

  //draw the text of the tab
  if StyleServices.Available then
  begin
    LTextColor:=GetColorTextTab(LThemedTab);

    if (TabPosition = tpTop) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, -1)
    else
    if (TabPosition = tpBottom) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, 1);

    if TabPosition = tpLeft then
    begin
      LTextX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 - Canvas.TextHeight(Tabs[Index]) div 2;
      LTextY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 + Canvas.TextWidth(Tabs[Index]) div 2;
      Canvas.Font.Color := LTextColor;
      AngleTextOut2(Canvas, 90, LTextX, LTextY, Tabs[Index]);
    end
    else
    if TabPosition = tpRight then
    begin
      LTextX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 + Canvas.TextHeight(Tabs[Index]) div 2;
      LTextY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 - Canvas.TextWidth(Tabs[Index]) div 2;
      Canvas.Font.Color := LTextColor;
      AngleTextOut2(Canvas, -90, LTextX, LTextY, Tabs[Index]);
    end
    else
     DrawControlText(Tabs[Index], LayoutR, DT_VCENTER or DT_CENTER or DT_SINGLELINE  or DT_NOCLIP);
  end;
end;

procedure TTabColorControlStyleHook.Paint(Canvas: TCanvas);
var
  LRect  : TRect;
  LIndex : Integer;
  SavedDC: Integer;
begin
  SavedDC := SaveDC(Canvas.Handle);
  try
    LRect := DisplayRect;
    ExcludeClipRect(Canvas.Handle, LRect.Left, LRect.Top, LRect.Right, LRect.Bottom);
    PaintBackground(Canvas);
  finally
    RestoreDC(Canvas.Handle, SavedDC);
  end;

  // Draw tabs , except the active
  for LIndex := 0 to TabCount - 1 do
  begin
    if LIndex = TabIndex then
      Continue;
    DrawTab(Canvas, LIndex);
  end;

  //Draw the body
  case TabPosition of
    tpTop   : InflateRect(LRect, Control.Width - LRect.Right, Control.Height - LRect.Bottom);
    tpLeft  : InflateRect(LRect, Control.Width - LRect.Right, Control.Height - LRect.Bottom);
    tpBottom: InflateRect(LRect, LRect.Left, LRect.Top);
    tpRight : InflateRect(LRect, LRect.Left, LRect.Top);
  end;

  if StyleServices.Available then
  begin
    Canvas.Brush.Color:=GetColorTab(TabIndex);
    Canvas.FillRect(LRect);
  end;

  // Draw active tab
  if TabIndex >= 0 then
    DrawTab(Canvas, TabIndex);

  // paint the controls of the tab
  TWinControlClass(Control).PaintControls(Canvas.Handle, nil);
end;

procedure TTabColorControlStyleHook.PaintBackground(Canvas: TCanvas);
var
  LColor : TColor;
begin
  if StyleServices.Available then
  begin
    LColor:=StyleServices.GetSystemColor(clWindowFrame);
    Canvas.Brush.Color:=LColor;
    Canvas.FillRect(Control.ClientRect);
  end;
end;

procedure TTabColorControlStyleHook.WMEraseBkgnd(var Message: TMessage);
var
  LCanvas : TCanvas;
begin
  if (Message.LParam = 1) and StyleServices.Available then
  begin
    LCanvas := TCanvas.Create;
    try
      LCanvas.Handle := HDC(Message.WParam);
      LCanvas.Brush.Color:=GetColorTab(TabIndex);
      LCanvas.FillRect(Control.ClientRect);
    finally
      LCanvas.Handle := 0;
      LCanvas.Free;
    end;
  end;
  Message.Result := 1;
  Handled := True;
end;


end.
