{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ButtonWnd                                                                        }
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
{ The Original Code is uButtonWnd.pas.                                                             }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ButtonWnd;

{
  +BugFix : When button is down ,the button style/state does not change .
  +BugFix : Button painted as RadioButton .
  +Added support for SplitButton : Split Button is themed .
  +Added focus effect to CheckBox & RadioButton
  +Added BSFocused state to Button : button is correctly themed when its got focus .
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Styles.ControlWnd;

type
  TButtonWnd = class(TControlWnd)
  private
    FDown: Boolean;
    FMouseEnter: Boolean;
    function GetBoxRect: TRect;
    procedure PaintGroupeBox(DC: HDC);
    function GetCaptionRect: TRect;
    procedure DrawRadioButton(DC: HDC; R: TRect; State: TButtonState);
    procedure DrawCheckBox(DC: HDC; R: TRect; State: TButtonState);
    procedure DrawButton(DC: HDC; R: TRect; State: TButtonState);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

var
  CurrentBtn: THandle;

implementation

uses
  System.Classes,
  Vcl.Graphics,
  Vcl.Themes;


{ TButtonWnd }
constructor TButtonWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
end;

procedure TButtonWnd.DrawButton(DC: HDC; R: TRect; State: TButtonState);
var
  LDetails, LSubDetails: TThemedElementDetails;
  sColor: TColor;
  Font: HFONT;
  DrawRect: TRect;
  LCanvas: TCanvas;
  SaveIndex: Integer;
  X, Y, i: Integer;
begin
  if not Enabled then
    LDetails := StyleServices.GetElementDetails(tbPushButtonDisabled)
  else
    LDetails := StyleServices.GetElementDetails(TThemedButton(State));

  sColor := StyleServices.GetStyleColor(scWindow);
  FillRectangle(DC, R, sColor);
  StyleServices.DrawElement(DC, LDetails, R);

  if IsSplitButton(Handle) then
    begin
      DrawRect := R;
      Dec(DrawRect.Right, 15);
      if State = BSPressed then
        begin
          LSubDetails := StyleServices.GetElementDetails(tbPushButtonPressed);
          SaveIndex := SaveDC(DC);
          try
            IntersectClipRect(DC, Width - 15, 0, Width, Height);
            DrawRect := Rect(Width - 30, 0, Width, Height);
            StyleServices.DrawElement(DC, LSubDetails, DrawRect);
          finally
            RestoreDC(DC, SaveIndex);
          end;
        end;
      LCanvas := TCanvas.Create;
      try
        LCanvas.Handle := DC;
        with LCanvas do
        begin
          // draw split line
          Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
          MoveTo(Width - 15, 3);
          LineTo(Width - 15, Height - 3);
          if Enabled then
            Pen.Color := StyleServices.GetSystemColor(clBtnHighLight)
          else
            Pen.Color := Font.Color;
          MoveTo(Width - 14, 3);
          LineTo(Width - 14, Height - 3);
          // draw arrow
          Pen.Color := Font.Color;
          X := Width - 8;
          Y := Height div 2 + 1;
          for i := 3 downto 0 do
            begin
              MoveTo(X - i, Y - i);
              LineTo(X + i + 1, Y - i);
            end;
        end;
      finally
        LCanvas.Free;
      end;
    end;

    Font := CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
    try
      SelectObject(DC, Font);
      StyleServices.DrawText(DC, LDetails, Text, R, [tfSingleLine, tfVerticalCenter, tfCenter]);
    finally
       DeleteObject(Font);
    end;
end;


procedure TButtonWnd.DrawCheckBox(DC: HDC; R: TRect; State: TButtonState);
var
  LDetails: TThemedElementDetails;
  sColor: TColor;
  sSize: TSize;
  Font: HFONT;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(DC);
  try
    Font := CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
    try
      SelectObject(DC, Font);
      if not Enabled then
        begin
          if IsButtonChecked(Handle) then
            LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedDisabled)
          else
            LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedDisabled)
        end
      else
        begin
          if State = BSNormal then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedNormal)
              else
                LDetails := StyleServices.GetElementDetails
                  (tbCheckBoxUncheckedNormal);
            end
          else if State = BSHot then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails(tbCheckBoxCheckedHot)
              else
                LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedHot);
            end
          else if State = BSPressed then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails
                  (tbCheckBoxCheckedPressed)
              else
                LDetails := StyleServices.GetElementDetails
                  (tbCheckBoxUncheckedPressed);
            end;
        end;

      sColor := StyleServices.GetStyleColor(scWindow);
      FillRectangle(DC, R, sColor);
      // if not StyleServices.GetElementSize(DC, LDetails, R, esActual, sSize) then
      //begin
      sSize.cx := GetSystemMetrics(SM_CXMENUCHECK);
      sSize.cy := GetSystemMetrics(SM_CYMENUCHECK);
      //end;
      StyleServices.DrawElement(DC, LDetails, Rect(0, 0, sSize.Width, R.Height));
      R := Rect(sSize.cx + 5, 0, R.Width, R.Height);
      StyleServices.DrawText(DC, LDetails, Text, R, [tfSingleLine, tfVerticalCenter]);
      StyleServices.DrawText(DC, LDetails, Text, R, [tfSingleLine, tfVerticalCenter, tfCalcRect]);
    finally
      DeleteObject(Font);
    end;
  finally
    RestoreDC(DC, SaveIndex);
  end;

  if Focused then
  begin
    GetTextExtentPoint32(DC, Text, Length(Text), sSize);
    R := Rect(R.Left - 2, R.Top + 2, R.Right + 2, R.Bottom + 1);
    DrawFocusRect(DC, R);
  end;
end;

procedure TButtonWnd.DrawRadioButton(DC: HDC; R: TRect; State: TButtonState);
var
  LDetails: TThemedElementDetails;
  sColor: TColor;
  sSize: TSize;
  Font: HFONT;
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(DC);
  try
    Font := CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
    try
      SelectObject(DC, Font);
      if not Enabled then
        begin
          if IsButtonChecked(Handle) then
            LDetails := StyleServices.GetElementDetails
              (tbRadioButtonCheckedDisabled)
          else
            LDetails := StyleServices.GetElementDetails
              (tbRadioButtonUncheckedDisabled)
        end
      else
        begin
          if State = BSNormal then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails
                  (tbRadioButtonCheckedNormal)
              else
                LDetails := StyleServices.GetElementDetails
                  (tbRadioButtonUncheckedNormal);
            end
          else if State = BSHot then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails(tbRadioButtonCheckedHot)
              else
                LDetails := StyleServices.GetElementDetails
                  (tbRadioButtonUncheckedHot);
            end
          else if State = BSPressed then
            begin
              if IsButtonChecked(Handle) then
                LDetails := StyleServices.GetElementDetails
                  (tbRadioButtonCheckedPressed)
              else
                LDetails := StyleServices.GetElementDetails
                  (tbRadioButtonUncheckedPressed);
            end;
        end;

      sColor := StyleServices.GetStyleColor(scWindow);
      FillRectangle(DC, R, sColor);
      // if not StyleServices.GetElementSize(DC, LDetails, R, esActual, sSize) then
      //begin
      sSize.cx := GetSystemMetrics(SM_CXMENUCHECK);
      sSize.cy := GetSystemMetrics(SM_CYMENUCHECK);
      //end;

      StyleServices.DrawElement(DC, LDetails, Rect(0, 0, sSize.Width, R.Height));
      R := Rect(sSize.cx + 5, 0, R.Width, R.Height);
      StyleServices.DrawText(DC, LDetails, Text, R,
        [tfSingleLine, tfVerticalCenter]);
      StyleServices.DrawText(DC, LDetails, Text, R, [tfSingleLine, tfVerticalCenter,
        tfCalcRect]);

    finally
      DeleteObject(Font);
    end;
  finally
    RestoreDC(DC, SaveIndex);
  end;

  if Focused then
    begin
      GetTextExtentPoint32(DC, Text, Length(Text), sSize);
      R := Rect(R.Left - 2, R.Top + 2, R.Right + 2, R.Bottom + 1);
      DrawFocusRect(DC, R);
    end;
end;

function TButtonWnd.GetBoxRect: TRect;
var
  DC: HDC;
  sSize: TSize;
begin
  DC := GetDC(Handle);
  GetTextExtentPoint32(DC, Text, Length(Text) - 1, sSize);
  Result := Rect(0, sSize.Height div 2 + 1, Width - 0, Height - 0);
  ReleaseDC(Handle, DC);
  DeleteDC(DC);
end;

function TButtonWnd.GetCaptionRect: TRect;
const
  FCaptionMargin = 12;
var
  DC: HDC;
  sSize: TSize;
begin
  DC := GetDC(Handle);
  try
    GetTextExtentPoint32(DC, Text, Length(Text) - 1, sSize);
    Result := Rect(FCaptionMargin, 0, FCaptionMargin + sSize.cx, sSize.Height);
  finally
    ReleaseDC(Handle, DC);
    DeleteDC(DC);
  end;
end;

procedure TButtonWnd.PaintGroupeBox(DC: HDC);
var
  R, CaptionRect: TRect;
  LDetails: TThemedElementDetails;
  SaveIndex: Integer;
  Font: HFONT;

begin
  if not StyleServices.Available then
    exit;

  Font := CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
  try
    SelectObject(DC, Font);

    CaptionRect := GetCaptionRect;
    R := GetBoxRect;

    if Enabled then
      LDetails := StyleServices.GetElementDetails(tbGroupBoxNormal)
    else
      LDetails := StyleServices.GetElementDetails(tbGroupBoxDisabled);
    SaveIndex := SaveDC(DC);
    try
      ExcludeClipRect(DC, CaptionRect.Left, CaptionRect.Top, CaptionRect.Right,
        CaptionRect.Bottom);
      StyleServices.DrawElement(DC, LDetails, R);
    finally
      RestoreDC(DC, SaveIndex);
    end;

    StyleServices.DrawText(DC, LDetails, Text, CaptionRect, [tfSingleLine, tfVerticalCenter, tfLeft]);
  finally
    DeleteObject(Font);
  end;
end;

procedure TButtonWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  PS: TPaintStruct;
  DC: HDC;
begin
  CurrentBtn := Handle;
  uMsg := Message.Msg;
  //Addlog('TButtonWnd '+IntToStr(uMsg));
  case uMsg of

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
        // Message.Result := CallOrgWndProc(Message);
      end;

    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_KILLFOCUS:
      begin
        FMouseEnter := False;
        FDown := False;
        Message.Result := CallOrgWndProc(Message);
        // Refresh;
      end;

    WM_NCPAINT:
      begin
        { Unlike others Controls , GroupeBox should be painted
          on WM_NCPAINT Event .
          if you try painting GroupeBox on WM_PAINT/WM_ERASEBKGND
          the visible controls that are placed on the GroupeBox
          will be invisible .(Ex: See TFontDialog : GroupeBox & Static Font ).
        }
        if IsButtonGroupeBox(Handle) then
          begin
            BeginPaint(Handle, PS);
            PaintGroupeBox(PS.HDC);
            EndPaint(Handle, PS);
            Message.Result := 1;
          end
        else
          Message.Result := CallOrgWndProc(Message);
      end;

    WM_PAINT:
      begin
        if IsButtonGroupeBox(Handle) then
          begin
            { Do not Paint GroupeBox here !! }
            exit;
          end;

        BeginPaint(Handle, PS);
        try
            DC := GetDC(Handle);
            try
              if IsButtonRadioBox(Handle) then
                begin
                  if FDown then
                    DrawRadioButton(DC, ClientRect, BSPressed)
                  else if FMouseEnter then
                    DrawRadioButton(DC, ClientRect, BSHot)
                  else
                    DrawRadioButton(DC, ClientRect, BSNormal);
                end
              else if IsButtonCheckBox(Handle) then
                begin
                  if FDown then
                    DrawCheckBox(DC, ClientRect, BSPressed)
                  else if FMouseEnter then
                    DrawCheckBox(DC, ClientRect, BSHot)
                  else
                    DrawCheckBox(DC, ClientRect, BSNormal);
                end
              else if IsButton(Handle) then
                begin
                  if FDown then
                    DrawButton(DC, ClientRect, BSPressed)
                  else if FMouseEnter then
                    DrawButton(DC, ClientRect, BSHot)
                  else if Focused then
                    DrawButton(DC, ClientRect, BSFocused)
                  else
                    DrawButton(DC, ClientRect, BSNormal);
                end;

            finally
              ReleaseDC(Handle, DC);
            end;
        finally
          EndPaint(Handle, PS);
        end;

      end;

    WM_LBUTTONDOWN:
      begin
        FDown := True;
        Refresh;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_LBUTTONUP:
      begin
        FDown := False;
        Refresh;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_MOUSELEAVE:
      begin
        if FMouseEnter then
          begin
            FMouseEnter := False;
            Refresh;
          end;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_MOUSEMOVE:
      begin
        if not FMouseEnter then
          begin
            FMouseEnter := True;
            Refresh;
          end;
        Message.Result := CallOrgWndProc(Message);

      end;

    WM_ENABLE:
      begin
        { Check first if Window is visible
          if you dont check ..the InVisible window will be visible .
        }
        if Visible then
          begin
            SetRedraw(False);
            Message.Result := CallOrgWndProc(Message);
            SetRedraw(True);
            InvalidateRect(Handle, nil, False);
          end
        else
          Message.Result := CallOrgWndProc(Message);
      end;
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
