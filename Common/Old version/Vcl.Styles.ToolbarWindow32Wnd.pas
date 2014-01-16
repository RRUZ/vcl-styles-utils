{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ToolbarWindow32Wnd                                                               }
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
{ The Original Code is uToolbarWindow32Wnd.pas.                                                    }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ToolbarWindow32Wnd;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Themes,
  System.Types,
  System.SysUtils,
  Vcl.Styles.ControlWnd,
  Winapi.CommCtrl,
  Vcl.ImgList,
  Vcl.GraphUtil;

type
  TToolbarWindow32Wnd = class(TControlWnd)
  private
    FImages: TCustomImageList;
    FDisabledImages: TCustomImageList;
    procedure ApplyImageList;
    function DropDownWidth(AButtonIndex: Integer): Integer;
    function GetButtonCount: Integer;
    function GetItemInfo(Index: Integer; Text: PChar; TextLen: Integer) : TTBButtonInfo;
    function GetItemRect(Index: Integer): TRect;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;

  end;

implementation

{ TToolbarWindow32Wnd }

constructor TToolbarWindow32Wnd.Create(AHandle: THandle);
begin
  inherited;
  FImages := nil;
  FDisabledImages := nil;
end;

function TToolbarWindow32Wnd.GetButtonCount: Integer;
begin
  Result := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
end;

function TToolbarWindow32Wnd.GetItemInfo(Index: Integer; Text: PChar;
  TextLen: Integer): TTBButtonInfo;
var
  TB: TTBButton;
begin
  FillChar(TB, SizeOf(TB), 0);
  SendMessage(Handle, TB_GETBUTTON, Index, IntPtr(@TB));
  FillChar(Result, SizeOf(Result), 0);
  Result.cbSize := SizeOf(TTBButtonInfo);
  Result.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_IMAGE or TBIF_TEXT;
  Result.cchText := TextLen;
  Result.pszText := Text;
  SendMessage(Handle, TB_GETBUTTONINFO, TB.idCommand, lParam(@Result));
  Result.fsStyle := TB.fsStyle;
  SendMessage(Handle, TB_GETBUTTONTEXT, TB.idCommand, lParam(Result.pszText));
end;

function TToolbarWindow32Wnd.GetItemRect(Index: Integer): TRect;
begin
  Result := TRect.Empty;
  if not BOOL(SendMessage(Handle, TB_GETITEMRECT, Index, lParam(@Result))) then
    Result := TRect.Empty;
end;

procedure TToolbarWindow32Wnd.ApplyImageList;
var
  H: Cardinal;
begin
  H := SendMessage(Handle, TB_GETIMAGELIST, 0, 0);
  if (H <> 0) and (FImages = nil) then
    begin
      FImages := TImageList.Create(nil);
      FImages.ShareImages := True;
      FImages.Handle := H;
    end;
  H := SendMessage(Handle, TB_GETDISABLEDIMAGELIST, 0, 0);
  if (H <> 0) and (FDisabledImages = nil) then
    begin
      FDisabledImages := TImageList.Create(nil);
      FDisabledImages.ShareImages := True;
      FDisabledImages.Handle := H;
    end;
end;

destructor TToolbarWindow32Wnd.Destroy;
begin
  if Assigned(FImages) then
    FreeAndNil(FImages);
  if Assigned(FDisabledImages) then
    FreeAndNil(FDisabledImages);
  inherited;
end;

function TToolbarWindow32Wnd.DropDownWidth(AButtonIndex: Integer): Integer;
var
  R: TRect;
begin
  if BOOL(SendMessage(Handle, TB_GETITEMDROPDOWNRECT, AButtonIndex, lParam(@R)))
  then
    Result := R.Right - R.Left
  else
    Result := 15; // default width when runtime themes are enabled
end;

procedure TToolbarWindow32Wnd.WndProc(var Message: TMessage);
const
  BufSize = 255;
var
  uMsg: UINT;
  BtnText: string;
  LDetails: TThemedElementDetails;
  PS: TPaintStruct;
  DC: HDC;
  ArrRect: TRect;
  i: Integer;
  Buffer: array [0 .. BufSize - 1] of Char;
  Info: TTBButtonInfo;
  R, iRect, imgRect, TxtRect: TRect;
  LDropDownWidth: Integer;
  Canvas: TCanvas;
  P: TPoint;
  BtnDisabled: Boolean;
  BtnPressed: Boolean;
  BtnHot: Boolean;
  LtstToolBar: Boolean;
  DoNotDraw: Boolean;
begin

  uMsg := Message.Msg;

  case uMsg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
      end;

    WM_PAINT:
      begin
        SetRedraw(False);
        Message.Result := CallOrgWndProc(Message);
        SetRedraw(True);
        BeginPaint(Handle, PS);
        try
          DC := PS.HDC;
          Canvas := TCanvas.Create;
          try
            Canvas.Handle := DC;
            if (Style and TBSTYLE_FLAT <> TBSTYLE_FLAT) and (Style <> $5603915D)
            then
              begin
                LDetails.Element := teToolBar;
                LDetails.Part := 0;
                LDetails.State := 0;
                if StyleServices.HasTransparentParts(LDetails) then
                  StyleServices.DrawParentBackground(Handle, DC, LDetails, False);
                StyleServices.DrawElement(DC, LDetails, ClientRect);
              end
            else
              begin
                FillRectangle(DC, ClientRect,
                  StyleServices.GetStyleColor(scWindow));
              end;

            ApplyImageList;
            FImages.Masked := True;
            FImages.BkColor := clNone;
            for i := 0 to GetButtonCount - 1 do
            begin
              //BtnDisabled := False;
              //BtnPressed := False;
              R := GetItemRect(i);
              imgRect := Rect(0, 0, FImages.Width, FImages.Height);
              imgRect := CenteredRect(R, imgRect);

              if Style and TBSTYLE_WRAPABLE = TBSTYLE_WRAPABLE then
                Dec(imgRect.Top, 10);
              FillChar(Buffer, BufSize, Char(0));
              Info := GetItemInfo(i, @Buffer, BufSize);
              BtnDisabled := Info.fsState and TBSTATE_ENABLED <> TBSTATE_ENABLED;
              BtnPressed := Info.fsState and TBSTATE_PRESSED = TBSTATE_PRESSED;
              if BtnDisabled then
                LDetails := StyleServices.GetElementDetails(ttbButtonDisabled)
              else
                LDetails := StyleServices.GetElementDetails(ttbButtonNormal);
              BtnHot := False;
              if MouseInControl and not BtnPressed then
                begin
                  GetCursorPos(P);
                  ScreenToClient(Handle, P);
                  if R.Contains(P) and not BtnDisabled then
                    begin
                      BtnHot := True;
                      LDetails := StyleServices.GetElementDetails(ttbButtonHot);
                    end;
                end
              else if BtnPressed then
                begin
                  LDetails := StyleServices.GetElementDetails(ttbButtonPressed)
                end;
              if Info.fsStyle and TBSTYLE_SEP = TBSTYLE_SEP then
                LDetails := StyleServices.GetElementDetails(ttbSeparatorNormal);

              if Info.fsState and TBSTATE_CHECKED = TBSTATE_CHECKED then
                LDetails := StyleServices.GetElementDetails(ttbButtonChecked);
              //DoNotDraw := False;
              DoNotDraw := (not Info.iImage > 0) and (not Info.cchText > 0);

              if (LDetails.State <> Integer(ttbButtonNormal)) and
                (LDetails.State <> Integer(ttbButtonDisabled)) and
                (Style and TBSTYLE_FLAT = TBSTYLE_FLAT) then
                if (Info.fsState and TBSTATE_HIDDEN <> TBSTATE_HIDDEN) and
                  not DoNotDraw and (Info.fsStyle <> 112) then
                  StyleServices.DrawElement(DC, LDetails, R);

              LtstToolBar := False;
              if Style = $5600B84D then
                LtstToolBar := True;

              //LDropDownWidth := 0;
              if (Info.fsStyle = 128) or
                (Info.fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN) then
                if not LtstToolBar then
                  begin
                    LDropDownWidth := DropDownWidth(i);
                    if BtnDisabled then
                      LDetails := StyleServices.GetElementDetails
                        (ttbDropDownButtonGlyphDisabled)
                    else if BtnPressed then
                      LDetails := StyleServices.GetElementDetails
                        (ttbDropDownButtonGlyphPressed)
                    else if BtnHot then
                      LDetails := StyleServices.GetElementDetails
                        (ttbDropDownButtonGlyphHot)
                    else
                      LDetails := StyleServices.GetElementDetails
                        (ttbDropDownButtonGlyphNormal);
                    ArrRect := Rect(R.Right - LDropDownWidth, R.Top, R.Right,
                      R.Bottom);
                    StyleServices.DrawElement(DC, LDetails, ArrRect);
                    if LDropDownWidth > 0 then
                      begin
                        imgRect := Rect(0, 0, FImages.Width, FImages.Height);
                        iRect := R;
                        iRect.Right := iRect.Right - LDropDownWidth;
                        imgRect := CenteredRect(iRect, imgRect);
                        imgRect.Left := imgRect.Left + 2;
                      end;
                  end
                else
                  begin
                    Canvas.Pen.Color := clBlack;
                    DrawArrow(Canvas, sdRight,
                      Point(R.Right - 8, R.Top + R.Height div 2 - 3), 3);
                  end;
              if LtstToolBar then
                begin
                  imgRect := Rect(0, 0, FImages.Width, FImages.Height);
                  imgRect := RectVCenter(imgRect, R);
                  imgRect.Left := imgRect.Left + 2;
                end;
              if Info.iImage > -1 then
                begin
                  if BtnDisabled then
                    FImages.DrawingStyle := dsSelected
                  else
                    FImages.DrawingStyle := dsNormal;
                  FImages.Draw(Canvas, imgRect.Left, imgRect.Top, Info.iImage);
                end;
              TxtRect := R;
              if not LtstToolBar then
                TxtRect := Rect(R.Left, R.Top + imgRect.Height, R.Right, R.Bottom)
              else
                begin
                  if Info.iImage > 0 then
                    TxtRect.Left := TxtRect.Left + 16
                  else
                    TxtRect.Left := R.Left;
                  iRect := TxtRect;
                  StyleServices.DrawText(DC, LDetails, Info.pszText, TxtRect,
                    [tfLeft, tfVerticalCenter, tfSingleLine, tfCalcRect]);
                  if R.Width > TxtRect.Width + 16 then
                    begin
                      StyleServices.DrawText(DC, LDetails, Info.pszText, R,
                        [tfCenter, tfVerticalCenter, tfSingleLine]);

                    end;
                end;
              if (Info.cchText > 0) and (Info.pszText <> '') then
                begin
                  BtnText := '';
                  BtnText := String(Info.pszText);
                  if (BtnText <> '') and not LtstToolBar then
                    DrawTextCentered(DC, LDetails, TxtRect, BtnText);
                end;
            end;
          finally
            Canvas.Free;
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
