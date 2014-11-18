{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ComboBoxWnd                                                                      }
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
{ The Original Code is uComboBoxWnd.pas.                                                           }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.ComboBoxWnd;
{
  BugFix: The white bakground color that apear when combobox has an edit control .
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Themes,
  System.Types,
  System.SysUtils,
  Vcl.Styles.ControlWnd;

type
  TComboListBoxWnd = class(TControlWnd)
  protected
    procedure WndProc(var Message: TMessage); override;
  end;

  TComboBoxWnd = class(TControlWnd)
  private
    FDown: Boolean;
    FMouseEnter: Boolean;
    FListBrush: HBRUSH;
    FStaticBrush: HBRUSH;
    FEditBrush: HBRUSH;
    FListBoxHandle: THandle;
    FListBoxWnd: TComboListBoxWnd;
    FMouseOnButton: Boolean;
    FListHandle: HWND;
    FEditHandle: HWND;
    //procedure DrawComboBox(DC: HDC; State: TButtonState);
    procedure PaintBorder(Canvas: TCanvas);
    function DroppedDown: Boolean;
    function GetButtonRect: TRect;
    procedure DrawItem(Canvas: TCanvas; Index: Integer; const R: TRect;
      Selected: Boolean);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    procedure WMPaint(var Message: TMessage);
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property ButtonRect: TRect read GetButtonRect;
  end;

var
  CurrentCB: HWND;

implementation



{ TComboBoxWnd }
function GetCBInfo(CBHandle: HWND): TComboBoxInfo;
begin
  FillChar(Result, SizeOf(TComboBoxInfo), #0);
  Result.cbSize := SizeOf(TComboBoxInfo);
  GetComboBoxInfo(CBHandle, Result);
end;

function TComboBoxWnd.GetButtonRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -2, -2);
  Result.Left := Result.Right - GetSystemMetrics(SM_CXVSCROLL) + 1;
  if (Style and CBS_DROPDOWN <> CBS_DROPDOWN) and
    (Style and CBS_DROPDOWNLIST <> CBS_DROPDOWNLIST) then
    Result := Rect(0, 0, 0, 0);
end;

function TComboBoxWnd.DroppedDown: Boolean;
begin
  Result := LongBool(SendMessage(Handle, CB_GETDROPPEDSTATE, 0, 0))
end;

procedure TComboBoxWnd.PaintBorder(Canvas: TCanvas);
var
  R, ControlRect, EditRect, ListRect: TRect;
  DrawState: TThemedComboBox;
  BtnDrawState: TThemedComboBox;
  Details: TThemedElementDetails;
  Buffer: TBitmap;
begin
  if not StyleServices.Available then
    Exit;
  FEditHandle := FindWindowEx(Handle, 0, 'EDIT', nil);
  if not Enabled then
    BtnDrawState := tcDropDownButtonDisabled
  else if DroppedDown then
    BtnDrawState := tcDropDownButtonPressed
  else if FMouseOnButton then
    BtnDrawState := tcDropDownButtonHot
  else
    BtnDrawState := tcDropDownButtonNormal;

  if not Enabled then
    DrawState := tcBorderDisabled
  else if Focused then
    DrawState := tcBorderFocused
  else if MouseInControl then
    DrawState := tcBorderHot
  else
    DrawState := tcBorderNormal;

  Buffer := TBitmap.Create;
  try
    Buffer.SetSize(Width, Height);
    R := Rect(0, 0, Buffer.Width, Buffer.Height);
    // draw border + client in buffer
    Details := StyleServices.GetElementDetails(DrawState);
    if (Style and CBS_DROPDOWN <> CBS_DROPDOWN) and
      (Style and CBS_DROPDOWNLIST <> CBS_DROPDOWNLIST) then
      begin
        FListHandle := GetCBInfo(Handle).hwndList;
        GetWindowRect(FListHandle, ListRect);
        GetWindowRect(Handle, ControlRect);
        R.Bottom := ListRect.Top - ControlRect.Top;
        StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);
        R := Rect(0, Height - (ControlRect.Bottom - ListRect.Bottom),
          Width, Height);
        with Buffer.Canvas do
          begin
            Brush.Style := bsSolid;
            Brush.Color := StyleServices.GetSystemColor(clBtnFace);
            FillRect(R);
          end;
        R := Rect(0, 0, Buffer.Width, Buffer.Height);
        R.Bottom := ListRect.Top - ControlRect.Top;
      end
    else
      StyleServices.DrawElement(Buffer.Canvas.Handle, Details, R);

    // draw button in buffer
    if ButtonRect.Width > 0 then
      begin
        Details := StyleServices.GetElementDetails(BtnDrawState);
        StyleServices.DrawElement(Buffer.Canvas.Handle, Details, ButtonRect);
      end;
    // calculation of exclude area for drawing buffer
    if (SendMessage(Handle, CB_GETCURSEL, 0, 0) >= 0) and (FEditHandle = 0) then
      begin
        R := ClientRect;
        InflateRect(R, -3, -3);
        R.Right := ButtonRect.Left - 2;
        ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
      end
    else if FEditHandle <> 0 then
      begin
        GetWindowRect(Handle, R);
        GetWindowRect(FEditHandle, EditRect);
        OffsetRect(EditRect, -R.Left, -R.Top);
        with EditRect do
          ExcludeClipRect(Canvas.Handle, Left, Top, Right, Bottom);
      end;
    // draw buffer
    Canvas.Draw(0, 0, Buffer);
  finally
    Buffer.Free;
  end;
end;

constructor TComboBoxWnd.Create(AHandle: THandle);
begin
  inherited Create(AHandle);
  FMouseEnter := False;
  FDown := False;
  FListBrush := CreateSolidBrush(StyleServices.GetStyleColor(scListBox));
  FListBoxHandle := 0;
  FListHandle := 0;
  FEditHandle := 0;
  FListBoxWnd := nil;
  FStaticBrush := 0;
  FEditBrush := 0;
end;

destructor TComboBoxWnd.Destroy;
begin
  inherited;
end;

//procedure TComboBoxWnd.DrawComboBox(DC: HDC; State: TButtonState);
//var
//  LDetails: TThemedElementDetails;
//  LInfo: TComboBoxInfo;
//  R: TRect;
//  Font: HFont;
//  sColor: TColor;
//begin
//  LInfo := GetCBInfo(Handle);
//  R := LInfo.rcButton;
//  if R.Bottom <> -1 then
//    R := Rect(0, 0, Width, R.Height + 2);
//  LDetails := StyleServices.GetElementDetails(TThemedComboBox.tcBackground);
//  StyleServices.DrawElement(DC, LDetails, R);
//  if State = BSNormal then
//
//    LDetails := StyleServices.GetElementDetails
//      (TThemedComboBox.tcDropDownButtonNormal)
//  else if State = BSHot then
//    LDetails := StyleServices.GetElementDetails
//      (TThemedComboBox.tcDropDownButtonHot)
//  else if State = BSPressed then
//    LDetails := StyleServices.GetElementDetails
//      (TThemedComboBox.tcDropDownButtonPressed);
//
//  LInfo := GetCBInfo(Handle);
//  R := LInfo.rcButton;
//  { Check if ComboboxButtonRect is Valid .
//    if you dont check before drawing ..
//    App will raise Memory access Violation Error
//  }
//  if R.Bottom <> -1 then
//    begin
//      inc(R.Top);
//      Dec(R.Right);
//      Dec(R.Bottom);
//      Dec(R.Left, 2);
//      StyleServices.DrawElement(DC, LDetails, R);
//    end;
//
//  R := ClientRect;
//  inc(R.Left, 10);
//  Font := CreateFont(13, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 'Tahoma');
//  try
//    SelectObject(DC, Font);
//    if IsWindowEnabled(Handle) then
//      LDetails := StyleServices.GetElementDetails(TThemedEdit.teEditTextNormal)
//    else
//      LDetails := StyleServices.GetElementDetails(TThemedEdit.teEditTextDisabled);
//
//    StyleServices.GetElementColor(LDetails, ecTextColor, sColor);
//    SetTextColor(DC, sColor);
//
//    StyleServices.DrawText(DC, LDetails, Text, R,
//      [tfSingleLine, tfLeft, tfVerticalCenter]);
//  finally
//    DeleteObject(Font);
//  end;
//end;

procedure TComboBoxWnd.DrawItem(Canvas: TCanvas; Index: Integer; const R: TRect;
  Selected: Boolean);
var
  sColor: TColor;
  Rect: TRect;
  SaveIndex: Integer;
  LDetails: TThemedElementDetails;
begin
  sColor := StyleServices.GetStyleColor(scEdit);
  Canvas.Brush.Color := sColor;
  Canvas.FillRect(R);
  Rect := R;
  inc(Rect.Left, 2);

  SaveIndex := SaveDC(Canvas.Handle);
  try
    SetBkMode(Canvas.Handle, TRANSPARENT);
    if Enabled then
      LDetails := StyleServices.GetElementDetails(TThemedEdit.teEditTextNormal)
    else
      LDetails := StyleServices.GetElementDetails(TThemedEdit.teEditTextDisabled);
    StyleServices.GetElementColor(LDetails, ecTextColor, sColor);
    SetTextColor(Canvas.Handle, sColor);
    DrawText(Canvas.Handle, Text, Length(Text), Rect, DT_LEFT or DT_VCENTER or
      DT_SINGLELINE);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
end;

procedure TComboBoxWnd.WMPaint(var Message: TMessage);
var
  R: TRect;
  Canvas: TCanvas;
  PS: TPaintStruct;
  DC: HDC;
  SaveIndex: Integer;
  ItemIndex: Integer;
begin
  BeginPaint(Handle, PS);
  try
    Canvas := TCanvas.Create;
    try
      DC := GetDC(Handle);
      try
        Canvas.Handle := DC;
        SaveIndex := SaveDC(Canvas.Handle);
        try
          PaintBorder(Canvas);
        finally
          RestoreDC(Canvas.Handle, SaveIndex);
        end;
        // if (Style and CBS_DROPDOWN =CBS_DROPDOWN) or
        // (Style and CBS_DROPDOWNLIST = CBS_DROPDOWNLIST) then
        begin
          R := ClientRect;
          InflateRect(R, -3, -3);
          R.Right := ButtonRect.Left - 1;

          SaveIndex := SaveDC(Canvas.Handle);
          try
            ItemIndex := SendMessage(Handle, CB_GETCURSEL, 0, 0);
            if ItemIndex < 0 then
              ItemIndex := 0;
            IntersectClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
            DrawItem(Canvas, ItemIndex, R, Focused);
          finally
            RestoreDC(Canvas.Handle, SaveIndex);
          end;
        end;
      finally
       ReleaseDC(Handle, DC);
      end;
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  finally
    EndPaint(Handle, PS);
  end;
end;

function ComboBox_GetLBText(hwndCtl: HWND; Index: Integer;
  lpszBuffer: Pointer): LResult;
begin
  Result := SendMessage(hwndCtl, CB_GETLBTEXT, Index, Integer(lpszBuffer));
end;

procedure TComboBoxWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  WPARAM: UINT_PTR;
  LPARAM: UINT_PTR;
  P: TPoint;
  sColor: TColor;
  LDetails: TThemedElementDetails;
  DIS: TDrawItemStruct;
begin

  uMsg := Message.Msg;
  WPARAM := Message.WPARAM;
  LPARAM := Message.LPARAM;
  case uMsg of

    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_SHOWWINDOW:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_DRAWITEM:
      begin
        DIS := PDrawItemStruct(LPARAM)^;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_CTLCOLORLISTBOX:
      begin
        Message.Result := CallOrgWndProc(Message);
        {
          Add this line :
          Message.Result := FListBrush;
          if you want to color ListBox of Combobox
        }
      end;

    WM_DESTROY:
      begin
        if Assigned(FListBoxWnd) then
          FreeAndNil(FListBoxWnd);
        DeleteObject(FListBrush);
        DeleteObject(FEditBrush);
        DeleteObject(FStaticBrush);
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_NOTIFY:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    CB_SETCURSEL:
      begin
        Message.Result := CallOrgWndProc(Message);
        Refresh;
      end;

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
        Exit;
      end;

    WM_KILLFOCUS:
      begin
        Message.Result := CallOrgWndProc(Message);
        Refresh;
      end;

    WM_CTLCOLOREDIT:
      begin
        { Change Edit Control BackGround Color }
        sColor := StyleServices.GetStyleColor(scEdit);
        SetBkColor(WPARAM, sColor);
        if FEditBrush = 0 then
          FEditBrush := CreateSolidBrush(sColor);
        Message.Result := FEditBrush;

        if IsWindowEnabled(LPARAM) then
          LDetails := StyleServices.GetElementDetails
            (TThemedEdit.teEditTextNormal)
        else
          LDetails := StyleServices.GetElementDetails
            (TThemedEdit.teEditTextDisabled);

        StyleServices.GetElementColor(LDetails, ecTextColor, sColor);
        { Change Edit Control Text Color }
        SetTextColor(WPARAM, sColor);
      end;

    WM_CTLCOLORSTATIC:
      begin
        sColor := StyleServices.GetStyleColor(scComboBox);
        SetBkColor(WPARAM, sColor);
        if FStaticBrush = 0 then
          FStaticBrush := CreateSolidBrush(sColor);

        if IsWindowEnabled(LPARAM) then
          LDetails := StyleServices.GetElementDetails
            (TThemedTextLabel.ttlTextLabelNormal)
        else
          LDetails := StyleServices.GetElementDetails
            (TThemedTextLabel.ttlTextLabelDisabled);

        StyleServices.GetElementColor(LDetails, ecTextColor, sColor);

        { Change Static Control Text Color }
        SetTextColor(WPARAM, sColor);
        { Transparent Static BackGound }
        Message.Result := FStaticBrush;

      end;

    WM_MOUSEMOVE:
      begin
        P.X := GET_X_LPARAM(LPARAM);
        P.Y := GET_Y_LPARAM(LPARAM);
        // LInfo := GetCBInfo(Handle);
        { Check if Mouse is on DropDown Button }
        // if LInfo.rcButton.Contains(P) then
        if ButtonRect.Contains(P) then
          if not FMouseOnButton then
            begin
              FMouseEnter := True;
              FMouseOnButton := True;
              Refresh;
            end;
        SetRedraw(False);
        Message.Result := CallOrgWndProc(Message);
        SetRedraw(True);
      end;

    WM_LBUTTONDOWN:
      begin
        if not Assigned(FListBoxWnd) then
          begin

            FListBoxHandle := GetCBInfo(Handle).hwndList;
            if FListBoxHandle <> 0 then
              begin
                { Only for the Future Use .. }
                // FListBoxWnd := TListBoxWnd.Create(FListBoxHandle);
                // FListBoxWnd.ParentHandle := Handle;

              end;
          end;
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
        { if State has really changed
          Redraw only when control need to be painted .
        }
        if FMouseOnButton then
          begin
            FMouseOnButton := False;
            FMouseEnter := False;
            Refresh;
          end;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_PAINT:
      begin
        (*
          BeginPaint(Handle, PS);
          DC := GetDC(Handle);
          if FDown then
          DrawComboBox(DC, BSPressed)
          else if FMouseEnter then
          DrawComboBox(DC, BSHot)
          else
          DrawComboBox(DC, BSNormal);

          ReleaseDC(Handle, DC);
          EndPaint(Handle, PS);
        *)
        WMPaint(Message);
        Message.Result := 0;
      end
  else
    Message.Result := CallOrgWndProc(Message);

  end;
end;

{ TListBoxWnd }

procedure TComboListBoxWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
begin
  inherited;
  uMsg := Message.Msg;
  case uMsg of
    WM_CREATE: Message.Result := CallOrgWndProc(Message);
  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;

end.
