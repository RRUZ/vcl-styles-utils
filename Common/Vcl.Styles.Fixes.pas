{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Fixes                                                                            }
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
{ The Original Code is Vcl.Styles.Fixes                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.Fixes;

interface

uses
 Vcl.ComCtrls,
 Vcl.StdCtrls,
 Winapi.Windows,
 Vcl.Graphics;

type
  /// <summary> The <c>TButtonStyleHookFix</c> vcl style hook fix these QC #103708, #107764 for Delphi XE2
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TButton, TButtonStyleHookFix);
  /// </code>
  /// </remarks>
  TButtonStyleHookFix = class(TButtonStyleHook)
  protected
    procedure Paint(Canvas: TCanvas); override;
  end;

  /// <summary> The <c>TListViewStyleHookFix</c> vcl style hook fix these QC #108678, #108875 for Delphi XE2
  /// </summary>
  /// <remarks>
  /// Use this hook in this way
  /// <code>
  /// TStyleManager.Engine.RegisterStyleHook(TListView, TListViewStyleHookFix);
  /// </code>
  /// </remarks>
 TListViewStyleHookFix=class(TListViewStyleHook)
    procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: Integer;
      const Text: string; IsPressed, IsBackground: Boolean); override;
 end;


implementation

uses
  Winapi.CommCtrl,
  Vcl.Themes,
  System.Types;

type
  TCustomButtonClass=class(TCustomButton);

  //we need this helper to access some strict private fields
  TButtonStyleHookHelper = class Helper for TButtonStyleHook
  protected
   function Pressed : Boolean;
   function DropDown: Boolean;
  end;

 TListViewStyleHookHelper = class helper for TListViewStyleHook
 function  HeaderHandle: HWnd;
 end;

procedure TButtonStyleHookFix.Paint(Canvas: TCanvas);
const
  PBS_NORMAL = 0;
  PBS_HOT = 1;
  PBS_PRESSED = 2;
  PBS_DISABLED = 3;
  PBS_DEFAULTED = 4;
  PBS_STYLUSHOT = 5;
var
  LDetails          : TThemedElementDetails;
  DrawRect          : TRect;
  pbuttonImagelist  : BUTTON_IMAGELIST;
  IW, IH, IY        : Integer;
  LTextFormatFlags  : TTextFormatFlags;
  ThemeTextColor    : TColor;
  Buffer            : string;
  BufferLength      : Integer;
  SaveIndex         : Integer;
  X, Y, I           : Integer;
  BCaption          : String;
  LImageIndex       : Integer;
begin
  //LImageIndex:=PBS_NORMAL;

  if StyleServices.Available then
  begin
    BCaption := Text;
    if Pressed then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
      LImageIndex:=PBS_PRESSED;
    end
    else
    if MouseInControl then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonHot);
      LImageIndex := PBS_HOT;
    end
    else
    if Focused then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonDefaulted) ;
      LImageIndex := PBS_DEFAULTED;
    end
    else
    if Control.Enabled then
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonNormal);
      LImageIndex := PBS_NORMAL;
    end
    else
    begin
      LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled);
      LImageIndex := PBS_DISABLED;
    end;

    DrawRect := Control.ClientRect;
    StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);

    if Button_GetImageList(handle, pbuttonImagelist) and (pbuttonImagelist.himl <> 0) and ImageList_GetIconSize(pbuttonImagelist.himl, IW, IH) then
    begin

      if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK then
        IY := DrawRect.Top + 15
      else
        IY := DrawRect.Top + (DrawRect.Height - IH) div 2;

      //here the image is drawn properly according to the ImageAlignment value
      case TCustomButton(Control).ImageAlignment of
        TImageAlignment.iaLeft  :
                  begin
                    ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle, DrawRect.Left + 3, IY, ILD_NORMAL);
                    Inc(DrawRect.Left, IW + 3);
                  end;
        TImageAlignment.iaRight :
                  begin
                    ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle, DrawRect.Right - IW -3, IY, ILD_NORMAL);
                    Dec(DrawRect.Right, IW - 3);
                  end;

        TImageAlignment.iaCenter:
                  begin
                   ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle, (DrawRect.Right - IW) div 2, IY, ILD_NORMAL);
                  end;


        TImageAlignment.iaTop   :
                  begin
                   ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle, (DrawRect.Right - IW) div 2, 3, ILD_NORMAL);
                  end;


        TImageAlignment.iaBottom:
                  begin
                   ImageList_Draw(pbuttonImagelist.himl, LImageIndex, Canvas.Handle, (DrawRect.Right - IW) div 2, (DrawRect.Height - IH) - 3, ILD_NORMAL);
                  end;

      end;


    end;

    if (GetWindowLong(Handle, GWL_STYLE) and BS_COMMANDLINK) = BS_COMMANDLINK then
    begin
      if pbuttonImagelist.himl = 0 then
        Inc(DrawRect.Left, 35);

      Inc(DrawRect.Top, 15);
      Inc(DrawRect.Left, 5);
      Canvas.Font := TCustomButtonClass(Control).Font;
      Canvas.Font.Style := [];
      Canvas.Font.Size := 12;
      LTextFormatFlags := TTextFormatFlags(DT_LEFT);
      if StyleServices.GetElementColor(LDetails, ecTextColor, ThemeTextColor) then
         Canvas.Font.Color := ThemeTextColor;
      StyleServices.DrawText(Canvas.Handle, LDetails, BCaption, DrawRect, LTextFormatFlags, Canvas.Font.Color);
      SetLength(Buffer, Button_GetNoteLength(Handle) + 1);
      if Length(Buffer) <> 0 then
      begin
        BufferLength := Length(Buffer);
        if Button_GetNote(Handle, PChar(Buffer), BufferLength) then
        begin
          LTextFormatFlags := TTextFormatFlags(DT_LEFT or DT_WORDBREAK);
          Inc(DrawRect.Top, Canvas.TextHeight('Wq') + 2);
          Canvas.Font.Size := 8;
          StyleServices.DrawText(Canvas.Handle, LDetails, Buffer, DrawRect,
            LTextFormatFlags, Canvas.Font.Color);
        end;
      end;

      if pbuttonImagelist.himl = 0 then
      begin
        if Pressed then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphPressed)
        else if MouseInControl then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphHot)
        else if Control.Enabled then
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphNormal)
        else
          LDetails := StyleServices.GetElementDetails(tbCommandLinkGlyphDisabled);
        DrawRect.Right := 35;
        DrawRect.Left := 3;
        DrawRect.Top := 10;
        DrawRect.Bottom := DrawRect.Top + 32;
        StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
      end;

    end
    else
    if (GetWindowLong(Handle, GWL_STYLE) and BS_SPLITBUTTON) = BS_SPLITBUTTON then
    begin
      Dec(DrawRect.Right, 15);
      DrawControlText(Canvas, LDetails, Text, DrawRect, DT_VCENTER or DT_CENTER);
      if DropDown then
      begin
        LDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
        SaveIndex := SaveDC(Canvas.Handle);
        try
          IntersectClipRect(Canvas.Handle, Control.Width - 15, 0,
            Control.Width, Control.Height);
          DrawRect := Rect(Control.Width - 30, 0, Control.Width, Control.Height);
          StyleServices.DrawElement(Canvas.Handle, LDetails, DrawRect);
        finally
          RestoreDC(Canvas.Handle, SaveIndex);
        end;
      end;

      with Canvas do
      begin
        Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
        MoveTo(Control.Width - 15, 3);
        LineTo(Control.Width - 15, Control.Height - 3);
        if Control.Enabled then
          Pen.Color := StyleServices.GetSystemColor(clBtnHighLight)
        else
          Pen.Color := Font.Color;
        MoveTo(Control.Width - 14, 3);
        LineTo(Control.Width - 14, Control.Height - 3);
        Pen.Color := Font.Color;
        X := Control.Width - 8;
        Y := Control.Height div 2 + 1;
        for i := 3 downto 0 do
        begin
          MoveTo(X - I, Y - I);
          LineTo(X + I + 1, Y - I);
        end;
      end;

    end
    else
    begin
      //finally the text is aligned and drawn depending of the value of the ImageAlignment property
      case TCustomButton(Control).ImageAlignment of
        TImageAlignment.iaLeft,
        TImageAlignment.iaRight,
        TImageAlignment.iaCenter : DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_VCENTER or DT_CENTER);
        TImageAlignment.iaBottom : DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_TOP or DT_CENTER);
        TImageAlignment.iaTop    : DrawControlText(Canvas, LDetails, BCaption, DrawRect, DT_BOTTOM or DT_CENTER);
      end;
    end;
  end;
end;

{ TButtonStyleHookHelper }

function TButtonStyleHookHelper.DropDown: Boolean;
begin
  Result:=Self.FDropDown;
end;

function TButtonStyleHookHelper.Pressed: Boolean;
begin
  Result:=Self.FPressed;
end;

{ TListViewStyleHookHelper }

function TListViewStyleHookHelper.HeaderHandle: HWnd;
begin
  Result:=Self.FHeaderHandle;
end;

{ TListViewStyleHookFix }

procedure TListViewStyleHookFix.DrawHeaderSection(Canvas: TCanvas; R: TRect;
  Index: Integer; const Text: string; IsPressed, IsBackground: Boolean);
var
  Item: THDItem;
  ImageList: HIMAGELIST;
  DrawState: TThemedHeader;
  IconWidth, IconHeight: Integer;
  Details: TThemedElementDetails;
begin
  FillChar(Item, SizeOf(Item), 0);
  Item.Mask := HDI_FORMAT;
  Header_GetItem(Handle, Index, Item);
  if IsBackground then
    DrawState := thHeaderItemNormal
  else if IsPressed then
    DrawState := thHeaderItemPressed
  else
    DrawState := thHeaderItemNormal;

  Details := StyleServices.GetElementDetails(DrawState);
  StyleServices.DrawElement(Canvas.Handle, Details, R);

  ImageList := SendMessage(HeaderHandle, HDM_GETIMAGELIST, 0, 0);
  Item.Mask := HDI_FORMAT or HDI_IMAGE;
  InflateRect(R, -2, -2);
  if (ImageList <> 0) and Header_GetItem(HeaderHandle, Index, Item) then
  begin
    if Item.fmt and HDF_IMAGE = HDF_IMAGE then
      ImageList_Draw(ImageList, Item.iImage, Canvas.Handle, R.Left, R.Top, ILD_TRANSPARENT);
    ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
    Inc(R.Left, IconWidth + 5);
  end;

  DrawControlText(Canvas, Details, Text, R, DT_VCENTER or DT_LEFT or  DT_SINGLELINE or DT_END_ELLIPSIS);
end;


end.
