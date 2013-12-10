{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.PopupWnd                                                                         }
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
{ The Original Code is uPopupWnd.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.PopupWnd;

{
  +Add support to Menu item icon (icons are painted if you use a Vcl Menu control (TMenu)).
  +BugFix :If Menu is a Vcl control (TMenu) then , in some situation the item text will not painted correctly.
  +BugFix : ItemDiasabled in some situation is draw as normal item .
  +BugFix : If PopupMenu is droped from MainMenu , the first item is highlighted.
  +BugFix : if item was selected using mouse then reselected using keyboard , the item does not appears hot.
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,

  Vcl.Graphics,
  Vcl.Forms,
  Vcl.Menus,
  Vcl.Themes,
  System.Types,
  Vcl.Styles.ControlWnd;

type
  TPopupWnd = class
  private
    FKeyIndex: Integer;
    FPaintFirstItemFromMenu: Boolean;
    FPreviousHotItemIndex: Integer;
    FHandle: THandle;
    FOldProc: Pointer;
    FFirstShow, FParentSubItemPainted: Boolean;
    FProcInstance: Pointer;
    MenuItem: TMenuItem;
    function GetHMenu(MenuWindow: HWND): HMENU;
    function CallOrgWndProc(Message: TMessage): LRESULT;
    procedure PopupWndProc(var Message: TMessage);
    procedure DrawNormalItem(DC: HDC; hWin: HWND; Menu: HMENU; ItemIndex: Integer; const Erase: Boolean = False);
    procedure DrawHotItem(DC: HDC; hWin: HWND; Menu: HMENU; ItemIndex: Integer; const Erase: Boolean = False);
  protected
    property Handle: THandle read FHandle write FHandle;
  public
    constructor Create(AHandle: THandle); overload;
    Destructor Destroy; override;
  end;

implementation

{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
type
  TMenuParent = record
    Menu: HMENU;
    Parent: HWND;
    Item: Integer;
  end;

var
  MenuParentArray: array of TMenuParent;

const
  { The Undocumented Messages }
  MN_SETHMENU                 = $01E0;
  MN_GETHMENU                 = $01E1;
  MN_SIZEWINDOW               = $01E2;
  MN_OPENHIERARCHY            = $01E3;
  MN_CLOSEHIERARCHY           = $01E4;
  MN_SELECTITEM               = $01E5;
  MN_CANCELMENUS              = $01E6;
  MN_SELECTFIRSTVALIDITEM     = $01E7;
  MN_GETPPOPUPMENU            = $01EA;
  MN_FINDMENUWINDOWFROMPOINT  = $01EB;
  MN_SHOWPOPUPWINDOW          = $01EC;
  MN_BUTTONDOWN               = $01ED;
  MN_MOUSEMOVE                = $01EE;
  MN_BUTTONUP                 = $01EF;
  MN_SETTIMERTOOPENHIERARCHY  = $01F0;
  MN_DBLCLK                   = $001F1;

  { MARLETT Font Char Const }
  MARLETT_RESTORE_CHAR    = Char(50);
  MARLETT_MINIMIZE_CHAR   = Char(48);
  MARLETT_CLOSE_CHAR      = Char(114);
  MARLETT_MAXIMIZE_CHAR   = Char(49);

function FindMenuItem(Menu: HMENU): TMenuItem;
var
  i, j: Integer;
  PopupMenu: TPopupMenu;
  Form: TForm;
  MI: TMenuItem;
begin
  //MI := nil;
  Result := nil;
  for i := 0 to Application.ComponentCount - 1 do
    begin
      if Application.Components[i] is TForm then
        begin
          Form := TForm(Application.Components[i]);
          for j := 0 to Form.ComponentCount - 1 do
            begin
              if Form.Components[j] is TMenuItem then
                begin
                  MI := TMenuItem(Form.Components[j]);
                  if MI.Handle = Menu then
                    Exit(MI);
                end
              else if Form.Components[j] is TPopupMenu then
                begin
                  PopupMenu := TPopupMenu(Form.Components[j]);
                  if PopupMenu.Handle = Menu then
                      Exit(PopupMenu.Items);
                end;
            end;
        end;
    end;
end;

function GetMenuItemBitmap(Menu: HMENU; ItemIndex: Integer): HBITMAP;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := info.hbmpItem;
  if Result = 0 then
    Result := info.hbmpUnchecked;
end;

function IsItemDisabled(Menu: HMENU; ItemIndex: Integer): Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fState and MFS_DISABLED = MFS_DISABLED) or
    (info.fState and MF_DISABLED = MF_DISABLED) or
    (info.fState and MF_GRAYED = MF_GRAYED);
end;

function IsItemChecked(Menu: HMENU; ItemIndex: Integer): Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function IsItemContainsSubMenu(Menu: HMENU; ItemIndex: Integer): Boolean;
begin
  Result := GetSubMenu(Menu, ItemIndex) > 0;
end;

function IsItemSeparator(Menu: HMENU; ItemIndex: Integer): Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, sizeof(info), Char(0));
  info.cbSize := sizeof(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;

function GetMenuItemText(h_Menu: HWND; ItemPos: Integer): AnsiString;
var
  // Buffer: PAnsiChar;
  Buffer: PChar;
  StrSize: Integer;
  info: MenuItemInfo;
begin
  { Note:
    The GetMenuString function has been superseded.
    Use the GetMenuItemInfo function to retrieve the menu item text.
  }

  Result := '';
  if h_Menu <= 0 then
    Exit;

  FillChar(info, sizeof(MenuItemInfo), Char(0));
  info.cbSize := sizeof(MenuItemInfo);
  info.fMask := MIIM_STRING or MIIM_FTYPE;
  info.dwTypeData := nil;
  GetMenuItemInfo(h_Menu, ItemPos, True, info);
  if not(info.fType and MFT_OWNERDRAW = MFT_OWNERDRAW) then
    begin
      { The Size neede for the Buffer . }
      StrSize := info.cch * 2 + 2;
      GetMem(Buffer, StrSize);
      try
        info.dwTypeData := Buffer;
        { inc cch to get the last char . }
        inc(info.cch);
        GetMenuItemInfo(h_Menu, ItemPos, True, info);
        Result := String(Buffer);
      finally
       FreeMem(Buffer, StrSize);
      end;
      exit;
    end
  else
  begin
    { if the item is owner draw then we need another way to get
      the item text since , when setting an item to ownerdraw windows
      will destroy the dwTypeData that hold the text . }
    FillChar(info, sizeof(MenuItemInfo), Char(0));
    info.cbSize := sizeof(MenuItemInfo);
    info.fMask := MIIM_DATA;
    GetMenuItemInfo(h_Menu, ItemPos, True, info);
    Result := String(PChar(info.dwItemData));
  end;
end;

function BmpToIcon(hBmp: HBITMAP): HICON;
var
  Bmp: BITMAP;
  hbmMask: HBITMAP;
  DC: HDC;
  ii: ICONINFO;
  Icon: HICON;
begin
  FillChar(Bmp, sizeof(BITMAP), Char(0));
  GetObject(hBmp, sizeof(BITMAP), @Bmp);
  DC := GetDC(0);
  hbmMask := CreateCompatibleBitmap(DC, Bmp.bmWidth, Bmp.bmHeight);
  ii.fIcon := True;
  ii.hbmColor := hBmp;
  ii.hbmMask := hbmMask;
  Icon := CreateIconIndirect(ii);
  DeleteObject(hbmMask);
  Result := Icon;
end;

procedure DrawSpecialChar(DC: HDC; Sign: Char; DestRect: TRect;
  const Bold: Boolean = False; const Disabled: Boolean = False);
var
  LogFont: TLogFont;
  pOldFont: HGDIOBJ;
  AFont: HFONT;
  oldColor: COLORREF;
  OldMode: Integer;
begin

  LogFont.lfHeight := DestRect.Height;
  LogFont.lfWidth := 0;
  LogFont.lfEscapement := 0;
  LogFont.lfOrientation := 0;
  if Bold then
    LogFont.lfWeight := FW_BOLD
  else
    LogFont.lfWeight := FW_NORMAL;
  LogFont.lfItalic := 0;
  LogFont.lfUnderline := 0;
  LogFont.lfStrikeOut := 0;
  LogFont.lfCharSet := DEFAULT_CHARSET;
  LogFont.lfOutPrecision := OUT_DEFAULT_PRECIS;
  LogFont.lfClipPrecision := CLIP_DEFAULT_PRECIS;
  LogFont.lfQuality := DEFAULT_QUALITY;
  LogFont.lfPitchAndFamily := DEFAULT_PITCH;
  LogFont.lfFaceName := 'Marlett';
  AFont := CreateFontIndirect(LogFont);

  oldColor := 0;
  if Disabled then
    oldColor := SetTextColor(DC, GetSysColor(COLOR_GRAYTEXT));

  pOldFont := SelectObject(DC, AFont);
  OldMode := SetBkMode(DC, TRANSPARENT);

  DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
  SetBkMode(DC, OldMode);
  SelectObject(DC, pOldFont);
  SelectObject(DC, oldColor);
  DeleteObject(AFont);
end;


{ TPopupWnd }
constructor TPopupWnd.Create(AHandle: THandle);
begin
  inherited Create;
  FKeyIndex := -1;
  FParentSubItemPainted := False;
  FPaintFirstItemFromMenu := False;
  FPreviousHotItemIndex := -1;
  FFirstShow := False;
  FOldProc := Pointer(GetWindowLongPtr(AHandle, GWL_WNDPROC));
  FProcInstance := MakeObjectInstance(PopupWndProc);
  FHandle := AHandle;
  SetWindowLongPtr(FHandle, GWL_WNDPROC, LONG_PTR(FProcInstance));
  MenuItem := nil;
end;

destructor TPopupWnd.Destroy;
begin
  FreeObjectInstance(FProcInstance);
  { We must set WndProc to the Old Proc !! }
  SetWindowLongPtr(Handle, GWL_WNDPROC, LONG_PTR(FOldProc));

  inherited;
end;

{
  it's better to use one function to draw Normal & Hot item , instead of two functions..
  but , to keep my code readable , i used two separate function:
  one to draw the Normal item state (DrawNormalItem).
  the second to draw the hot item state (DrawHotItem).
}
procedure TPopupWnd.DrawNormalItem(DC: HDC; hWin: HWND; Menu: HMENU;
  ItemIndex: Integer; const Erase: Boolean = False);
var
  LDetails, PBGDetails, LSDetails: TThemedElementDetails;
  R, TextRect, WindowRect, ClientRect, TmpRect: TRect;
  P: TPoint;
  Canvas: TCanvas;
  Bmp, TmpBmp: TBitmap;
  Text: string;
  hBmp: HBITMAP;
  hBmpDC: HDC;
  W, H: Integer;
  Size: TSize;
  Sign: Char;
  ImageIndex: Integer;
  Icon: TIcon;

begin
  if (ItemIndex >= 0) and (ItemIndex <= GetMenuItemCount(Menu) - 1) then
    begin
      LSDetails.State := 0;
      if IsItemContainsSubMenu(Menu, ItemIndex) then
        begin
          LSDetails := StyleServices.GetElementDetails(tmPopupSubMenuNormal);
          LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
          if IsItemDisabled(Menu, ItemIndex) then
            begin
              LSDetails := StyleServices.GetElementDetails(tmPopupSubMenuDisabled);
              LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);
            end;
        end
      else
      if IsItemSeparator(Menu, ItemIndex) then
          LDetails := StyleServices.GetElementDetails(tmPopupSeparator)
      else
      begin
        LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
        if IsItemDisabled(Menu, ItemIndex) then
            LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);
      end;

      GetWindowRect(hWin, WindowRect);
      GetClientRect(hWin, ClientRect);

      GetMenuItemRect(0, Menu, ItemIndex, R);
      P := Point(R.Left - WindowRect.Left, R.Top - WindowRect.Top);
      R := Rect(P.X + 2, P.Y, R.Width, P.Y + R.Height);
      inc(R.Right, 3);
      { conserve the border }
      if ItemIndex = 0 then
        inc(R.Top, 1);

      { Draw the BackGround first }
      PBGDetails := StyleServices.GetElementDetails(tmPopupBackground);
      if Erase then
        begin
          PBGDetails := StyleServices.GetElementDetails(tmPopupBorders);
          TmpRect := R;
          R := Rect(0, R.Top, ClientRect.Width, R.Bottom);
        end;
      { Copy the BackGround to the item
        So the item will looks clean .
      }
      Canvas := TCanvas.Create;
      try
        Canvas.Handle := DC;
        Bmp := TBitmap.Create;
        try
          TmpBmp := TBitmap.Create;
          try
            Bmp.SetSize(ClientRect.Width, ClientRect.Height);
            TmpBmp.SetSize(R.Width, R.Height);
            StyleServices.DrawElement(Bmp.Canvas.Handle, PBGDetails, ClientRect);
            TmpBmp.Canvas.CopyRect(Rect(0, 0, R.Width, R.Height), Bmp.Canvas, R);
            Canvas.Draw(R.Left, R.Top, TmpBmp);
            if Erase then
              R := TmpRect;
          finally
            TmpBmp.Free;
          end;
        finally
          Bmp.Free;
        end;
      finally
        Canvas.Free;
      end;

      if LDetails.State = Integer(tmPopupSeparator) then
        begin
          { Draw Separator }
          TextRect := R;
          inc(TextRect.Left, 25);
          StyleServices.DrawElement(DC, LDetails, TextRect);
        end;
      { Draw Text }
      TextRect := R;
      inc(TextRect.Left, 30);
      if (Assigned(MenuItem)) then
        Text := MenuItem.Items[ItemIndex].Caption
      else
        Text := String(GetMenuItemText(Menu, ItemIndex));

      StyleServices.DrawText(DC, LDetails, Text, TextRect,
        [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs]);

      if LSDetails.State <> 0 then
        begin
          { Draw the Sub Menu Glyph (Only if Exists) }

          { Note : there is a bug in the DrawElement function :
            if you try to draw tmPopupSubMenuNormal at (X,Y) point
            DrawElement function will draw the Glyph at (X,Z) Point .
            To Solve this problem, we need to draw first to a clean bitmap
            then ,draw this bitmap to our target DC .
            I have encountered some problems when trying using bitmap mask
            so i used transparency instead ..
          }

          StyleServices.GetElementSize(DC, LSDetails, esActual, Size);
          hBmp := CreateBmp(Size.Width, Size.Height);
          hBmpDC := GetBmpDc(hBmp);
          FillRect(hBmpDC, Rect(0, 0, Size.Width, Size.Height), CreateSolidBrush(clFuchsia));
          StyleServices.DrawElement(hBmpDC, LSDetails, Rect(0, 0, 0, 0));
          ReleaseBmpDc(hBmpDC);

          DrawBitmap(DC, hBmp, Point(R.Right - Size.Width, R.Top), Rect(0, 0, Size.Width, Size.Height), False, True, 255, clFuchsia);
          DeleteObject(hBmp);
        end;

      { Draw Items Icons }
      hBmp := GetMenuItemBitmap(Menu, ItemIndex);
      if hBmp <> 0 then
        begin
          if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
            begin
              { For the System Menu : the bitmap(Close,Max,Min,..)
                are only a character .
                We can draw this character using MARLETT or WEBDINGS Font .
              }
              case hBmp of
                HBMMENU_POPUP_RESTORE:
                  Sign := MARLETT_RESTORE_CHAR;
                HBMMENU_POPUP_MINIMIZE:
                  Sign := MARLETT_MINIMIZE_CHAR;
                HBMMENU_POPUP_MAXIMIZE:
                  Sign := MARLETT_MAXIMIZE_CHAR;
                HBMMENU_POPUP_CLOSE:
                  Sign := MARLETT_CLOSE_CHAR;
              else
                Sign := Char(0);
              end;
              if Sign <> #0 then
                begin
                  TmpRect := Rect(0, 0, R.Width - 10, R.Height - 10);
                  TmpRect := CenteredRect(R, TmpRect);
                  TmpRect.Left := R.Left + 10;
                  DrawSpecialChar(DC, Sign, TmpRect);
                  exit;
                end;
            end;
          W := GetBitmapWidth(hBmp);
          H := GetBitmapHeight(hBmp);
          TmpRect := Rect(0, 0, W, H);
          TmpRect := CenteredRect(R, TmpRect);
          TmpRect.Left := R.Left + 5;
          DrawIconEX(DC, TmpRect.Left, TmpRect.Top, BmpToIcon(hBmp), W, H, 0, 0, DI_NORMAL);
          { if there is bitmap then there is no need to draw Checked bitmap }
          exit;

        end;
      if Assigned(MenuItem) then
        begin
          Icon := TIcon.Create;
          try
            if (ItemIndex >= 0) then
            begin
              ImageIndex := MenuItem.Items[ItemIndex].ImageIndex;
              if (ImageIndex <> -1) and (Assigned(MenuItem.GetParentMenu.Images))
              then
                begin
                  MenuItem.GetParentMenu.Images.GetIcon(ImageIndex, Icon);
                  TmpRect := Rect(0, 0, Icon.Width, Icon.Height);
                  TmpRect := CenteredRect(R, TmpRect);
                  TmpRect.Left := R.Left + 5;
                  // DrawIconEX(DC, TmpRect.Left, TmpRect.Top, Icon.Handle, Icon.Width,
                  // Icon.Height, 0, 0, DI_NORMAL);
                  Canvas := TCanvas.Create;
                  try
                    Canvas.Handle := DC;
                    MenuItem.GetImageList.Draw(Canvas, TmpRect.Left, TmpRect.Top, ImageIndex);
                  finally
                    Canvas.Free;
                  end;
                end;
            end;
          finally
            Icon.Free;
          end;
          exit;
        end;
      { If item is checked and there is no bitmap associated with this item ..
        then draw the check Glyph . }
      if IsItemChecked(Menu, ItemIndex) then
        begin
          if IsItemDisabled(Menu, ItemIndex) then
            LSDetails := StyleServices.GetElementDetails(tmPopupCheckDisabled)
          else
            LSDetails := StyleServices.GetElementDetails(tmPopupCheckNormal);
          StyleServices.GetElementSize(DC, LSDetails, esActual, Size);
          TmpRect := Rect(0, 0, Size.Width, Size.Height);
          TmpRect := CenteredRect(R, TmpRect);
          TmpRect := Rect(R.Left + 5, TmpRect.Top, R.Left + 5 + Size.Width, TmpRect.Bottom);
          StyleServices.DrawElement(DC, LSDetails, TmpRect);
        end;
    end;
end;

procedure TPopupWnd.DrawHotItem(DC: HDC; hWin: HWND; Menu: HMENU;
  ItemIndex: Integer; const Erase: Boolean = False);
var
  LDetails, PBGDetails, LSDetails: TThemedElementDetails;
  R, TextRect, WindowRect, ClientRect, TmpRect: TRect;
  P: TPoint;
  Canvas: TCanvas;
  Bmp, TmpBmp: TBitmap;
  Text: string;
  hBmp: HBITMAP;
  hBmpDC: HDC;
  W, H: Integer;
  Sign: Char;
  Size: TSize;
  ImageIndex: Integer;
  Icon: TIcon;
begin
  if (ItemIndex >= 0) and (ItemIndex <= GetMenuItemCount(Menu) - 1) then
    begin
      LSDetails.State := 0;
      if IsItemContainsSubMenu(Menu, ItemIndex) then
        begin
          LSDetails := StyleServices.GetElementDetails(tmPopupSubMenuNormal);
          LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
          if IsItemDisabled(Menu, ItemIndex) then
            begin
              LSDetails := StyleServices.GetElementDetails
                (tmPopupSubMenuDisabled);
              LDetails := StyleServices.GetElementDetails
                (tmPopupItemDisabledHot);
            end;
        end
      else
      if IsItemSeparator(Menu, ItemIndex) then
          LDetails := StyleServices.GetElementDetails(tmPopupSeparator)
      else
      begin
        LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
        if IsItemDisabled(Menu, ItemIndex) then
            LDetails := StyleServices.GetElementDetails(tmPopupItemDisabledHot);
      end;

      if (LDetails.State <> Integer(tmPopupSeparator)) then
        begin
          GetWindowRect(hWin, WindowRect);
          GetClientRect(hWin, ClientRect);

          GetMenuItemRect(0, Menu, ItemIndex, R);
          P := Point(R.Left - WindowRect.Left, R.Top - WindowRect.Top);
          R := Rect(P.X + 2, P.Y, R.Width, P.Y + R.Height);
          if ItemIndex = 0 then
            inc(R.Top, 1);

          { Draw the BackGround First . }
          PBGDetails := StyleServices.GetElementDetails(tmPopupBackground);
          if Erase then
            begin
              PBGDetails := StyleServices.GetElementDetails(tmPopupBorders);
              TmpRect := R;
              R := Rect(0, R.Top, ClientRect.Width, R.Bottom);
            end;
          Canvas := TCanvas.Create;
          try
            Canvas.Handle := DC;
            Bmp := TBitmap.Create;
            try
              TmpBmp := TBitmap.Create;
              try
                Bmp.SetSize(ClientRect.Width, ClientRect.Height);
                TmpBmp.SetSize(R.Width, R.Height);
                StyleServices.DrawElement(Bmp.Canvas.Handle, PBGDetails, ClientRect);
                TmpBmp.Canvas.CopyRect(Rect(0, 0, R.Width, R.Height), Bmp.Canvas, R);
                Canvas.Draw(R.Left, R.Top, TmpBmp);
                if Erase then
                  R := TmpRect;
              finally
                TmpBmp.Free;
              end;
            finally
              Bmp.Free;
            end;
          finally
            Canvas.Free;
          end;
          inc(R.Right, 3);
          { highlight Item (Hot Item) }
          StyleServices.DrawElement(DC, LDetails, R);

          { Draw Item Text }
          TextRect := R;
          inc(TextRect.Left, 30);
          if (Assigned(MenuItem)) and (ItemIndex >= 0) and
            (ItemIndex <= GetMenuItemCount(Menu) - 1) then
            Text := MenuItem.Items[ItemIndex].Caption
          else
            Text := String(GetMenuItemText(Menu, ItemIndex));
          StyleServices.DrawText(DC, LDetails, Text, TextRect, [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs]);

          if LSDetails.State <> 0 then
            begin
              { Draw the Sub Menu Glyph (Only if Exists) }

              { Note : there is a bug in the DrawElement function :
                if you try to draw tmPopupSubMenuNormal at (X,Y) point
                DrawElement function will draw the Glyph at (X,Z) Point .
                To Solve this problem, we need to draw first to a clean bitmap
                then ,draw this bitmap to our target DC .
                I have encountered some problems when trying using bitmap mask
                so i used transparency instead ..
              }
              StyleServices.GetElementSize(DC, LSDetails, esActual, Size);
              hBmp := CreateBmp(Size.Width, Size.Height);
              try
                hBmpDC := GetBmpDc(hBmp);
                FillRect(hBmpDC, Rect(0, 0, Size.Width, Size.Height),
                  CreateSolidBrush(clFuchsia));
                StyleServices.DrawElement(hBmpDC, LSDetails, Rect(0, 0, 0, 0));
                ReleaseBmpDc(hBmpDC);

                DrawBitmap(DC, hBmp, Point(R.Right - Size.Width, R.Top),
                  Rect(0, 0, Size.Width, Size.Height), False, True, 255,
                  clFuchsia);
              finally
                DeleteObject(hBmp);
              end;
            end;
        end;

      { Draw Item Bitmap }
      hBmp := GetMenuItemBitmap(Menu, ItemIndex);
      if hBmp <> 0 then
        begin
          if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
            begin
              case hBmp of
                HBMMENU_POPUP_RESTORE:
                  Sign := MARLETT_RESTORE_CHAR;
                HBMMENU_POPUP_MINIMIZE:
                  Sign := MARLETT_MINIMIZE_CHAR;
                HBMMENU_POPUP_MAXIMIZE:
                  Sign := MARLETT_MAXIMIZE_CHAR;
                HBMMENU_POPUP_CLOSE:
                  Sign := MARLETT_CLOSE_CHAR;
              else
                Sign := Char(0);
              end;
              if Sign <> #0 then
                begin
                  TmpRect := Rect(0, 0, R.Width - 10, R.Height - 10);
                  TmpRect := CenteredRect(R, TmpRect);
                  TmpRect.Left := R.Left + 10;
                  DrawSpecialChar(DC, Sign, TmpRect);
                  exit;
                end;
            end;
          W := GetBitmapWidth(hBmp);
          H := GetBitmapHeight(hBmp);
          P := Point(R.Left, R.Top);
          TmpRect := Rect(0, 0, W, H);
          TmpRect := CenteredRect(R, TmpRect);
          TmpRect.Left := R.Left + 5;

          DrawIconEX(DC, TmpRect.Left, TmpRect.Top, BmpToIcon(hBmp), W, H, 0, 0,
            DI_NORMAL);
          exit;
        end;

      if Assigned(MenuItem) then
        begin
          Icon := TIcon.Create;
          //ImageIndex := -1;
          if (ItemIndex >= 0) then
            begin
              ImageIndex := MenuItem.Items[ItemIndex].ImageIndex;
              if (ImageIndex <> -1) and (Assigned(MenuItem.GetParentMenu.Images))
              then
                begin
                  MenuItem.GetParentMenu.Images.GetIcon(ImageIndex, Icon);
                  TmpRect := Rect(0, 0, Icon.Width, Icon.Height);
                  TmpRect := CenteredRect(R, TmpRect);
                  TmpRect.Left := R.Left + 5;
                  // DrawIconEX(DC, TmpRect.Left, TmpRect.Top, Icon.Handle, Icon.Width,
                  // Icon.Height, 0, 0, DI_NORMAL);
                  Canvas := TCanvas.Create;
                  try
                  Canvas.Handle := DC;
                  MenuItem.GetImageList.Draw(Canvas, TmpRect.Left, TmpRect.Top,
                    ImageIndex);
                  finally
                    Canvas.Free;
                  end;
                end;
            end;
          Icon.Free;
          exit;
        end;
      if IsItemChecked(Menu, ItemIndex) then
        begin
          if IsItemDisabled(Menu, ItemIndex) then
            LSDetails := StyleServices.GetElementDetails(tmPopupCheckDisabled)
          else
            LSDetails := StyleServices.GetElementDetails(tmPopupCheckNormal);
          StyleServices.GetElementSize(DC, LSDetails, esActual, Size);
          TmpRect := Rect(0, 0, Size.Width, Size.Height);
          TmpRect := CenteredRect(R, TmpRect);
          TmpRect := Rect(R.Left + 5, TmpRect.Top, R.Left + 5 + Size.Width,
            TmpRect.Bottom);
          StyleServices.DrawElement(DC, LSDetails, TmpRect);
        end;
    end;
end;


function TPopupWnd.CallOrgWndProc(Message: TMessage): LRESULT;
begin
  Result := CallWindowProc(FOldProc, Handle, Message.Msg, Message.wParam,
    Message.lParam);
end;

function TPopupWnd.GetHMenu(MenuWindow: HWND): HMENU;
begin
  Result := SendMessage(MenuWindow, MN_GETHMENU, 0, 0);
end;

procedure TPopupWnd.PopupWndProc(var Message: TMessage);
var
  DC: HDC;
  R: TRect;
  LDetails: TThemedElementDetails;
  Menu: HMENU;
  i, L, ParentItem: Integer;
  ParentPopup: HWND;
  uMsg: UINT;
  wParam: UINT_PTR;
  lParam: UINT_PTR;

  procedure SetRedraw(Window: HWND; Value: Boolean);
  begin
    SendMessage(Window, WM_SETREDRAW, Winapi.Windows.wParam(Value), 0);
  end;

begin
  uMsg := Message.Msg;
  wParam := Message.wParam;
  lParam := Message.lParam;

  case uMsg of

    MN_SELECTITEM:
      { The undocumented MN_SELECTITEM Message:
        this is the most importants message ,Windows sends this message every time when the user
        select an item (not clicking,only select) ...
        wParam=Current Item Index .
        lparam= may be it's unused (not sure).
      }
      begin
        ParentPopup := 0;
        ParentItem := -1;
        Menu := GetHMenu(Handle);
        if Integer(wParam) > GetMenuItemCount(Menu) then
          begin
            { Make sure that wParam hold a valid Item Index .
              if not .. then mose is not on the PopupMenu
              => Remove Item highlight .
            }
            SetRedraw(Handle, True);
            DC := GetDC(Handle);
            if FPreviousHotItemIndex <> -1 then
              DrawNormalItem(DC, Handle, Menu, FPreviousHotItemIndex);
            FPreviousHotItemIndex := -1;
            ReleaseDC(Handle, DC);
            exit;
          end;

        L := Length(MenuParentArray);
        if L <> 0 then
          begin
            for i := 0 to L do
              begin
                { Look for SubMenu Parent }
                if MenuParentArray[i].Menu = GetHMenu(Handle) then
                  begin
                    ParentPopup := MenuParentArray[i].Parent;
                    ParentItem := MenuParentArray[i].Item;
                    Break;
                  end;
              end;
          end;

        if (ParentPopup = Handle) then
          begin
            { Allow Redraw the Current PopupMenu }
            SendMessage(Handle, WM_SETREDRAW, 1, 0);
          end
        else if (ParentPopup <> Handle) and (ParentPopup <> 0) and (FFirstShow)
        then
          begin
            {
              if user jump so fast from the parent PopupMenu to the
              Child PopupMenu (SubMenu) , the hot item of parent Popup menu
              will be draw as a normal item (not hot)..
              So we need to repaint the hot item that drop the child popup menu.
            }
            if not FParentSubItemPainted then
              begin
                SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
                FParentSubItemPainted := True;
              end;
            { Don't Redraw the parent of the Current PopupMenu }
            SetRedraw(ParentPopup, False);
          end;
        { if Item can drop a sub Popup Menu }
        if (IsItemContainsSubMenu(Menu, wParam)) and FFirstShow then
          begin
            L := Length(MenuParentArray);
            if L = 0 then
              SetLength(MenuParentArray, 1);
            for i := 0 to L do
              { Avoid duplication }
              if MenuParentArray[i].Menu <> GetHMenu(Handle) then
                begin
                  L := L + 1;
                  SetLength(MenuParentArray, L);
                  MenuParentArray[L - 1].Menu := GetSubMenu(Menu, wParam);
                  MenuParentArray[L - 1].Parent := Handle;
                  MenuParentArray[L - 1].Item := wParam;
                  Break;
                end;
          end;
        { If all Items are painted }
        if FFirstShow then
          begin
            SetRedraw(Handle, False);
            { Calling the Original Message in order to Open,Hide SubMenu .
              if you find another way to Open,Show,Hide SubMenu ..
              i will be very happy if you contact me :)
            }
            Message.Result := CallOrgWndProc(Message);
            SetRedraw(Handle, True);
          end;

        DC := GetDC(Handle);
        { Avoid drawing when there is no need to . }
        if Integer(wParam) <> FPreviousHotItemIndex then
          begin
            if not FFirstShow then
              begin
                DrawNormalItem(DC, Handle, Menu, wParam);
              end
            else
              begin
                { Draw Previous Item (Normal Item) }
                DrawNormalItem(DC, Handle, Menu, FPreviousHotItemIndex);
                { Draw Current Item (Hot Item) }
                DrawHotItem(DC, Handle, Menu, wParam);
                FPreviousHotItemIndex := wParam;
              end;
          end;

        ReleaseDC(Handle, DC);
        { Check if all items are painted. }
        if Integer(wParam) = GetMenuItemCount(Menu) - 1 then
          FFirstShow := True;
      end;

    WM_KEYDOWN:
      begin
        Menu := GetHMenu(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;
        case wParam of
          VK_DOWN:
            begin
              if FPaintFirstItemFromMenu then
                begin
                  if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                    FKeyIndex := -1;
                  inc(FKeyIndex);
                  { If the Current Item is Separator then
                    find the next valid item .
                  }
                  if IsItemSeparator(Menu, FKeyIndex) then
                    begin
                      for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                        begin
                          if (not IsItemSeparator(Menu, i)) then
                            begin
                              FKeyIndex := i;
                              Break;
                            end;
                        end;
                    end;

                  SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
                  Message.Result := 0;
                end;
            end;
          VK_UP:
            begin
              if FKeyIndex <= 0 then
                FKeyIndex := GetMenuItemCount(Menu);

              Dec(FKeyIndex);
              { If the Current Item is Separator then
                find the next valid item .
              }
              if IsItemSeparator(Menu, FKeyIndex) then
                 for i := FKeyIndex downto 0 do
                    begin
                      if not IsItemSeparator(Menu, i) then
                        begin
                          FKeyIndex := i;
                          Break;
                        end;
                    end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;
        else
          begin
            Message.Result := CallOrgWndProc(Message);
            { Calling the Default Message will cause
              the WM_PAINT Message to be Sent to the PopupMenu Window }
          end;
        end;
      end;

    WM_PAINT:
      begin
        { We need to call the original WM_PAINT Message
          if you don't call the original WM_PAINT Message ,
          the PopupMenu will be freezed .

          Use SetRedraw to Enable or Disable Painting change
          i call SetRedraw(False) to prevent the original painting ,
          then i use SetRedraw(True) in order to allow my painting .
        }
        SetRedraw(Handle, False);
        Message.Result := CallOrgWndProc(Message);
        SetRedraw(Handle, True);
      end;

    WM_ERASEBKGND:
      begin
        SendMessage(Handle, WM_PRINT, wParam, lParam);
        Message.Result := 1;
      end;

    WM_PRINTCLIENT:
      begin
        SendMessage(Handle, WM_PRINT, wParam, lParam);
      end;

    WM_PRINT:
      begin
        {
          WM_PRINT:
          Windows Sends WM_PRINT message to draw the Background
          (its the first drawing message ).
        }
        GetClientRect(Handle, R);
        if wParam <> 0 then
          DC := HDC(wParam)
        else
          DC := GetDC(Handle);

        { Draw the Popup BackGround }
        LDetails := StyleServices.GetElementDetails(TThemedMenu.tmPopupBorders);
        StyleServices.DrawElement(DC, LDetails, R);

        if DC <> HDC(wParam) then
          ReleaseDC(Handle, DC);

        if not FFirstShow then
          begin
            { If the Popup Items are not painted..then draw all Items. }
            Menu := GetHMenu(Handle);
            for i := 0 to GetMenuItemCount(Menu) - 1 do
              PostMessage(Handle, MN_SELECTITEM, i, 0);
          end;
      end;

    WM_WINDOWPOSCHANGED:
      begin
        Menu := GetHMenu(Handle);
        Message.Result := CallOrgWndProc(Message);
        SetTimer(Handle, $93, 100, nil);
        if not Assigned(MenuItem) then
          MenuItem := FindMenuItem(Menu);
      end;

    WM_TIMER:
      begin
        Message.Result := CallOrgWndProc(Message);
        if wParam = $93 then
          begin
            if FFirstShow then
              begin
                { If PopupMenu is droped from MainMenu ,
                  MainMenu will send WM_KEYDOWN message
                  to the PopupMenu that cause the PopupMenu
                  to paint the first item as a hot item instead of
                  a normal item .
                  I use a timer to solve this problem .
                }
                FPaintFirstItemFromMenu := True;
                KillTimer(Handle, $93);
              end;
          end;
      end;

    MN_BUTTONDOWN:
      { The undocumented MN_BUTTONDOWN Message:
        Send when the mouse is down in the Popup Item .
        wParam=Item Index .
      }
      begin
        Menu := GetHMenu(Handle);
        if not IsItemDisabled(Menu, wParam) then
          begin
            SetRedraw(Handle, False);
            Message.Result := CallOrgWndProc(Message);
            SetRedraw(Handle, True);
          end
        else
          FKeyIndex := wParam;
      end;

    WM_NCCALCSIZE:
      begin
        { No Border !! }
      end;

    WM_NCPAINT:
      begin
        { Do Nothing !! }
      end;

    MN_BUTTONUP:
      begin
        Message.Result := CallOrgWndProc(Message);
      end;

    WM_DESTROY:
      begin
        MenuItem := nil;
        SetLength(MenuParentArray, 0);
        MenuParentArray := nil;
        Message.Result := CallOrgWndProc(Message);
      end;

  else
    Message.Result := CallOrgWndProc(Message);
  end;
end;


end.
