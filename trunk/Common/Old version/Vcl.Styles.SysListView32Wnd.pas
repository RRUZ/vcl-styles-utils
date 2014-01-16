{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.SysListView32Wnd                                                                 }
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
{ The Original Code is uSysListView32Wnd.pas.                                                      }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.SysListView32Wnd;
{
  +HeaderSection is themed as hot when mouse is on it .
}
{ Update (29/10/2013):
  + The ScrollBar of SysListView32 control is themed .
  + The Header of SysListView32 control is themed .
  + BugFix : when clicking on Column , background color change .
}

interface

{.$DEFINE UseNativeScollBar}

uses
  Winapi.Windows,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Themes,
  System.SysUtils,
  Vcl.Styles.ControlWnd,
  Winapi.CommCtrl,
  {$IFDEF UseNativeScollBar }
  Vcl.Styles.NativeScrollBar
  {$ELSE}
  Vcl.Styles.ScrollBarWnd
  {$ENDIF};

type
  TSysListView32Wnd = class(TScrollBarWnd)
  private type
    THeaderWnd = class(TControlWnd)
    private
      FListView: TSysListView32Wnd;
      FPressedSection: integer;
      FHeaderLBtnDown: Boolean;
      FMouseDown: Boolean;
      FPrevSection: integer;
      procedure PaintHeader(DC: HDC);
      function GetSectionFromPoint(Pt: TPoint): integer;
      procedure DrawHeaderSection(Canvas: TCanvas; R: TRect; Index: integer;
        const Text: string; IsPressed, IsBackground: Boolean);
      //Property ListView: TSysListView32Wnd read FListView write FListView;
    protected
      procedure WndProc(var Message: TMessage); override;
    public
      constructor Create(AHandle: THandle); override;
      Destructor Destroy; override;
    end;

  private
    FHeaderWnd: THeaderWnd;
    procedure UpdateColors;
    procedure SetSelectedColumn(iCol: integer);
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

implementation

uses
   System.Types;

{ TSysListView32Wnd }

constructor TSysListView32Wnd.Create(AHandle: THandle);
begin
  inherited;
  FHeaderWnd := nil;
end;

destructor TSysListView32Wnd.Destroy;
begin
  if Assigned(FHeaderWnd) then
    FreeAndNil(FHeaderWnd);
  inherited;
end;

procedure TSysListView32Wnd.SetSelectedColumn(iCol: integer);
begin
  //if ListView_GetSelectedColumn(Handle) > -1 then
    ListView_SetSelectedColumn(Handle, iCol);
end;

procedure TSysListView32Wnd.UpdateColors;
var
  sColor: TColor;
begin
  sColor := StyleServices.GetStyleColor(scListView);
  ListView_SetBkColor(Handle, sColor);
  ListView_SetTextBkColor(Handle, sColor);
  ListView_SetTextColor(Handle, FontColor);
end;

procedure TSysListView32Wnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  HeaderWindow: HWND;
begin
  uMsg := Message.Msg;
  case uMsg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        UpdateColors;
        SetSelectedColumn(-1);
      end;

    LVM_UPDATE:
      begin
        Message.Result := CallOrgWndProc(Message);
        UpdateColors;
        SetSelectedColumn(-1);
      end;

    WM_NOTIFY:
      begin
        HeaderWindow := ListView_GetHeader(Handle);
        // GetWindow(Handle, GW_CHILD);
        if (HeaderWindow <> 0) and (not Assigned(FHeaderWnd)) then
          begin
            FHeaderWnd := THeaderWnd.Create(HeaderWindow);
            FHeaderWnd.FListView := Self;
          end;
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_ERASEBKGND:
      begin
        UpdateColors;
        SetSelectedColumn(-1);
        Message.Result := 1;
      end
  else
    inherited;
  end;

end;

{ TSysListView32Wnd.THeaderWnd }
constructor TSysListView32Wnd.THeaderWnd.Create(AHandle: THandle);
begin
  inherited;
  FPrevSection := 0;
  FPressedSection := -1;
end;

destructor TSysListView32Wnd.THeaderWnd.Destroy;
begin
  inherited;
end;

procedure TSysListView32Wnd.THeaderWnd.DrawHeaderSection(Canvas: TCanvas;
  R: TRect; Index: integer; const Text: string;
  IsPressed, IsBackground: Boolean);
var
  item: THDItem;
  ImageList: HIMAGELIST;
  DrawState: TThemedHeader;
  IconWidth, IconHeight: integer;
  Details: TThemedElementDetails;
  isItemHot: Boolean;
  P: TPoint;
begin
  FillChar(item, sizeof(item), 0);
  item.mask := HDI_FORMAT;
  Header_GetItem(Handle, Index, item);
  GetCursorPos(P);
  ScreenToClient(Handle, P);
  isItemHot := R.Contains(P);

  DrawState := thHeaderItemNormal;
  if IsBackground then
    DrawState := thHeaderItemNormal
  else
  if isItemHot then
  begin
    if not FMouseDown then
      DrawState := thHeaderItemHot;
    if IsPressed then
      DrawState := thHeaderItemPressed;
  end
  else
  if IsPressed then
    DrawState := thHeaderItemPressed
  else
    DrawState := thHeaderItemNormal;

  Details := StyleServices.GetElementDetails(DrawState);
  StyleServices.DrawElement(Canvas.Handle, Details, R);

  ImageList := SendMessage(Handle, HDM_GETIMAGELIST, 0, 0);
  item.mask := HDI_FORMAT or HDI_IMAGE;
  InflateRect(R, -2, -2);
  if (ImageList <> 0) and Header_GetItem(Handle, Index, item) then
    begin
      if item.fmt and HDF_IMAGE = HDF_IMAGE then
        ImageList_Draw(ImageList, item.iImage, Canvas.Handle, R.Left, R.Top,
          ILD_TRANSPARENT);
      ImageList_GetIconSize(ImageList, IconWidth, IconHeight);
      inc(R.Left, IconWidth + 5);
    end;
  DrawTextCentered(Canvas.Handle, Details, R, Text);
end;

function TSysListView32Wnd.THeaderWnd.GetSectionFromPoint(Pt: TPoint): integer;
var
  i: integer;
  R: TRect;
begin
  Result := -1;
  for i := 0 to Header_GetItemCount(Handle) - 1 do
    begin
      R := TRect.Empty;
      Header_GetItemRect(Handle, i, @R);
      if R.Contains(Pt) then
        Exit(i);
    end;
end;

procedure TSysListView32Wnd.THeaderWnd.PaintHeader(DC: HDC);
var
  Canvas: TCanvas;
  R, HeaderR: TRect;
  PS: TPaintStruct;
  HeaderDC: HDC;
  i, ColumnIndex, RightOffset: integer;
  SectionOrder: array of integer;
  item: THDItem;
  Buffer: array [0 .. 255] of char;
  FHeaderHandle: HWND;
begin
  FHeaderHandle := Handle;
  if DC = 0 then
  begin
    BeginPaint(FHeaderHandle, PS);
    HeaderDC := GetDC(Handle);
  end
  else
    HeaderDC := DC;
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := HeaderDC;
      RightOffset := 0;

      for i := 0 to Header_GetItemCount(FHeaderHandle) - 1 do
        begin
          SetLength(SectionOrder, Header_GetItemCount(FHeaderHandle));
          Header_GetOrderArray(FHeaderHandle,
            Header_GetItemCount(FHeaderHandle), Pointer(SectionOrder));
          ColumnIndex := SectionOrder[i];
          Header_GetItemRect(FHeaderHandle, ColumnIndex, @R);
          FillChar(item, sizeof(item), 0);
          item.mask := HDI_TEXT;
          item.pszText := @Buffer;
          item.cchTextMax := Length(Buffer);
          Header_GetItem(FHeaderHandle, ColumnIndex, item);
          DrawHeaderSection(Canvas, R, ColumnIndex, item.pszText,
            FPressedSection = ColumnIndex, False);

          if RightOffset < R.Right then
            RightOffset := R.Right;
        end;

      GetWindowRect(FHeaderHandle, HeaderR);
      R := Rect(RightOffset, 0, HeaderR.Width + 2, HeaderR.Height);
      if not IsRectEmpty(R) then
        DrawHeaderSection(Canvas, R, -1, '', False, True);

      if DC <> 0 then
        ReleaseDC(FHeaderHandle, DC);
    finally
      Canvas.Handle := 0;
      Canvas.Free;
    end;
  finally
    if DC = 0 then
      begin
        EndPaint(FHeaderHandle, PS);
        ReleaseDC(Handle, HeaderDC);
      end;
  end;
end;

procedure TSysListView32Wnd.THeaderWnd.WndProc(var Message: TMessage);
var
  uMsg: UINT;
  lParam: UINT_PTR;
  Info: THDHitTestInfo;
  P: TPoint;
  LSection: integer;
begin
  uMsg := Message.Msg;
  lParam := Message.lParam;
  case uMsg of
    WM_CREATE:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      begin
        FMouseDown := True;
        Info.Point.X := TWMMouse(Message).XPos;
        Info.Point.Y := TWMMouse(Message).YPos;
        SendMessage(Handle, HDM_HITTEST, 0, IntPtr(@Info));

        if (Info.Flags and HHT_ONDIVIDER = 0) and
          (Info.Flags and HHT_ONDIVOPEN = 0) then
          FPressedSection := Info.item
        else
          FPressedSection := -1;
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE);
        FHeaderLBtnDown := True;
      end;

    WM_LBUTTONUP, WM_RBUTTONUP:
      begin
        FMouseDown := False;
        FPressedSection := -1;
        RedrawWindow(Handle, nil, 0, RDW_INVALIDATE);
        FHeaderLBtnDown := False;
        FListView.UpdateColors;
        FListView.SetSelectedColumn(-1);
      end;

    WM_ERASEBKGND:
      begin
        Message.Result := 1;
        Exit;
      end;

    WM_MOUSEMOVE:
      begin
        Message.Result := CallOrgWndProc(Message);
        P.X := GET_X_LPARAM(lParam);
        P.Y := GET_Y_LPARAM(lParam);
        LSection := GetSectionFromPoint(P);
        if (LSection <> -1) and (not FMouseDown) then
          if FPrevSection <> LSection then
            begin
              FPrevSection := LSection;
              Refresh;
            end;
      end;

    WM_PAINT:
      begin
        PaintHeader(0);
        Exit;
      end

  end;
  Message.Result := CallOrgWndProc(Message);
end;

end.
