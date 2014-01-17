{ ************************************************************************************************** }
{ }
{ Unit Vcl.Styles.Utils.Menus }
{ unit for the VCL Styles Utils }
{ http://code.google.com/p/vcl-styles-utils/ }
{ }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the }
{ License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF }
{ ANY KIND, either express or implied. See the License for the specific language governing rights }
{ and limitations under the License. }
{ }
{ }
{ Portions created by Safafi Mahdi [SMP3]   e-mail SMP@LIVE.FR }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V. }
{ All Rights Reserved. }
{ }
{ ************************************************************************************************** }
unit Vcl.Styles.Utils.Menus;

interface

uses
  System.Classes,
  System.Types,
  System.SysUtils,
  Winapi.Windows,
  Winapi.Messages,
  Winapi.UxTheme,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Graphics,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.GraphUtil,
  Vcl.Controls,
  Vcl.Menus,
  System.Math,
  Vcl.Styles.Utils.SysStyleHook;

const
  { The Undocumented Messages }
  MN_SETHMENU = $01E0;
  MN_GETHMENU = $01E1;
  MN_SIZEWINDOW = $01E2;
  MN_OPENHIERARCHY = $01E3;
  MN_CLOSEHIERARCHY = $01E4;
  MN_SELECTITEM = $01E5;
  MN_CANCELMENUS = $01E6;
  MN_SELECTFIRSTVALIDITEM = $01E7;
  MN_GETPPOPUPMENU = $01EA;
  MN_FINDMENUWINDOWFROMPOINT = $01EB;
  MN_SHOWPOPUPWINDOW = $01EC;
  MN_BUTTONDOWN = $01ED;
  MN_MOUSEMOVE = $01EE;
  MN_BUTTONUP = $01EF;
  MN_SETTIMERTOOPENHIERARCHY = $01F0;
  MN_DBLCLK = $001F1;

  { MARLETT Font Char Const }
  MARLETT_RESTORE_CHAR = Char(50);
  MARLETT_MINIMIZE_CHAR = Char(48);
  MARLETT_CLOSE_CHAR = Char(114);
  MARLETT_MAXIMIZE_CHAR = Char(49);

type
  TSysPopupStyleHook = class;
  TSysPopupItemState = set of (isHot, isDisabled, isChecked, isDefault);
  TSysPopupItemStyle = (isNormal, isSep, isDropDown);

  TSysPopupStyleHook = class(TSysStyleHook)
  private type
{$REGION 'TSysPopupItem'}
    TSysPopupItem = class
    private
      FIndex: integer;
      FMenu: HMENU;
      FHandle: HWND;
      FSysParent: TSysControl;
      function GetItemRect: TRect;
      function IsItemDisabled: Boolean;
      function IsItemContainsSubMenu: Boolean;
      function IsItemSeparator: Boolean;
      function isItemChecked: Boolean;
      function isItemDefault: Boolean;
      function GetItemText: String;
      function GetVCLMenuItem: TMenuItem;
      function GetItemBitmap: HBITMAP;
      function IsItemRadioCheck: Boolean;
    public
      constructor Create(SysParent: TSysControl; Index: integer;
        Menu: HMENU); virtual;
      Destructor Destroy; override;
      property ItemRect: TRect read GetItemRect;
      property Disabled: Boolean read IsItemDisabled;
      property Separator: Boolean read IsItemSeparator;
      property HasSubMenu: Boolean read IsItemContainsSubMenu;
      property Checked: Boolean read isItemChecked;
      property RadioCheck: Boolean read IsItemRadioCheck;
      property DefaultItem: Boolean read isItemDefault;
      property Text: String read GetItemText;
      property VCLMenuItem: TMenuItem read GetVCLMenuItem;
      property Bitmap: HBITMAP read GetItemBitmap;
    end;
{$ENDREGION}

  var
    FItemsPainted: Boolean;
    FParentSubItemPainted: Boolean;
    FPreviousHotItemIndex: integer;
    FPaintFirstItemFromMenu: Boolean;
    FKeyIndex: integer;
    FSysPopupItem: TSysPopupItem;
    FCount: integer;
    FMenu: HMENU;
    function GetMenuFromHandle(AHandle: HWND): HMENU;
    function GetItemsCount: integer;
    procedure MNSELECTITEM(var Message: TMessage); message MN_SELECTITEM;
    procedure WMPRINT(var Message: TMessage); message WM_PRINT;
    function GetSysPopupItem(Index: integer): TSysPopupItem;
    function GetRightToLeft: Boolean;
  protected
    procedure EraseItem(Canvas: TCanvas; Index: integer;
      ItemRect: TRect); virtual;
    procedure DoDrawItem(Canvas: TCanvas; Index: integer);
    procedure DrawItem(Canvas: TCanvas; Index: integer; ItemRect: TRect;
      ItemText: String; State: TSysPopupItemState;
      Style: TSysPopupItemStyle); Virtual;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure UpdateColors; override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Menu: HMENU read FMenu;
    property Items[Index: integer]: TSysPopupItem read GetSysPopupItem;
    property Count: integer read FCount;
    property RightToLeft: Boolean read GetRightToLeft;
  end;

implementation

uses
  Vcl.Styles.Utils.SysControls;

{ TSysPopupStyleHook }

constructor TSysPopupStyleHook.Create(AHandle: THandle);
begin
  inherited;
{$IF CompilerVersion > 23}
  StyleElements := [seFont, seClient, seBorder];
{$ELSE}
  OverridePaint := True;
  OverridePaintNC := True;
  OverrideFont := True;
{$IFEND}
  FPreviousHotItemIndex := -1;
  FKeyIndex := -1;
  FItemsPainted := False;
  FSysPopupItem := nil;
end;

destructor TSysPopupStyleHook.Destroy;
begin
  if Assigned(FSysPopupItem) then
    FreeAndNil(FSysPopupItem);
  inherited;
end;

procedure TSysPopupStyleHook.DoDrawItem(Canvas: TCanvas; Index: integer);
var
  LItemRect: TRect;
  P: TPoint;
  State: TSysPopupItemState;
  Style: TSysPopupItemStyle;
  LText: String;
  SaveIndex: integer;
  Item: TSysPopupItem;
begin
  if (Index < 0) or (Index > Count - 1) then
    Exit;
  Item := Items[Index];
  LItemRect := Item.ItemRect;
  P := Point(LItemRect.Left, LItemRect.Top);
  ScreenToClient(Handle, P);
  LItemRect := Rect(P.X, P.Y, P.X + LItemRect.Width, P.Y + LItemRect.Height);
  if LItemRect.Left < 2 then
    LItemRect.Left := 2;
  inc(LItemRect.Right, 4);
  if LItemRect.Top < 2 then
    inc(LItemRect.Top, 2);
  { Item State }
  State := [];
  if index <> FPreviousHotItemIndex then
    Include(State, isHot);
  if Item.Disabled then
    Include(State, isDisabled);
  if Item.Checked then
    Include(State, isChecked);
  if Item.DefaultItem then
    Include(State, isDefault);
  { Item Style }
  Style := isNormal;
  if Item.Separator then
    Style := isSep;
  if Item.HasSubMenu then
    Style := isDropDown;

  LText := '';
  if Style <> isSep then
    LText := Item.Text;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    EraseItem(Canvas, Index, LItemRect);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;

  SaveIndex := SaveDC(Canvas.Handle);
  try
    DrawItem(Canvas, Index, LItemRect, LText, State, Style);
  finally
    RestoreDC(Canvas.Handle, SaveIndex)
  end;
end;

procedure TSysPopupStyleHook.DrawItem(Canvas: TCanvas; Index: integer;
  ItemRect: TRect; ItemText: String; State: TSysPopupItemState;
  Style: TSysPopupItemStyle);
var
  Detail: TThemedMenu;
  LDetails: TThemedElementDetails;
  LTextFormat: TTextFormat;
  DC: HDC;
  LSize: TSize;
  MI: TMenuItem;
  ImageIndex: integer;
  LImageRect, R: TRect;
  LImageWidth: integer;
  LTextRect: TRect;
  hBmp: HBITMAP;
  BmpHeight, BmpWidth: integer;
  Icon: HICON;
  DisplayCheckedGlyph: Boolean;
  Sign: Char;
  SysItem: TSysPopupItem;
  sShortCut: String;
  Bmp: TBitmap;
  procedure DrawSubMenu(const ItemRect: TRect);
  var
    Bmp: TBitmap;
    LSubMenuDetails: TThemedElementDetails;
    LSubMenuDetail: TThemedMenu;
    SubMenuSize: TSize;
    LSubMenuRect: TRect;
  begin

    LSubMenuRect := Rect(0, 0, 0, 0);
    LSubMenuDetail := tmPopupSubMenuNormal;
    if isDisabled in State then
      LSubMenuDetail := tmPopupSubMenuDisabled;
    LSubMenuDetails := StyleServices.GetElementDetails(LSubMenuDetail);
    StyleServices.GetElementSize(DC, LSubMenuDetails, esActual, SubMenuSize);
    if not RightToLeft then
      LSubMenuRect := Rect(ItemRect.Right - SubMenuSize.cx, ItemRect.Top,
        ItemRect.Right, ItemRect.Top + SubMenuSize.cy)
    else
      LSubMenuRect := Rect(ItemRect.Left + 4, ItemRect.Top,
        ItemRect.Left + 4 + SubMenuSize.Width, ItemRect.Bottom);
    Bmp := TBitmap.Create;
    try
      Bmp.SetSize(SubMenuSize.Width, SubMenuSize.Height);
      Bmp.Canvas.Brush.Color := clFuchsia;
      Bmp.Canvas.FillRect(Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      StyleServices.DrawElement(Bmp.Canvas.Handle, LSubMenuDetails,
        Rect(0, 0, SubMenuSize.Width, SubMenuSize.Height));
      if RightToLeft then
        begin
          RotateBitmap(Bmp, DegToRad(180), False, clFuchsia);
          inc(LSubMenuRect.Top, (Bmp.Height div 2) - 2);
        End
      else
        Dec(LSubMenuRect.Left, 4);

      TransparentBlt(DC, LSubMenuRect.Left, LSubMenuRect.Top, SubMenuSize.Width,
        SubMenuSize.Height, Bmp.Canvas.Handle, 0, 0, SubMenuSize.Width,
        SubMenuSize.Height, clFuchsia);
    finally
      Bmp.Free;
    end;
    Dec(LTextRect.Right, LSubMenuRect.Width);
  end;
  procedure DrawVisualSysBitmap(Bmp: HBITMAP);
  var
    Theme: HTHEME;
    LRect, R: TRect;
    iPart, iState: integer;
    LSize: TSize;
  begin
    Theme := OpenThemeData(0, 'Menu');
    iPart := MENU_SYSTEMCLOSE;
    case hBmp of
      HBMMENU_POPUP_RESTORE:
        iPart := MENU_SYSTEMRESTORE;
      HBMMENU_POPUP_MINIMIZE:
        iPart := MENU_SYSTEMMINIMIZE;
      HBMMENU_POPUP_MAXIMIZE:
        iPart := MENU_SYSTEMMAXIMIZE;
      HBMMENU_POPUP_CLOSE:
        iPart := MENU_SYSTEMCLOSE;

    end;
    iState := integer(isDisabled in State) + 1;

    Winapi.UxTheme.GetThemePartSize(Theme, DC, iPart, iState, nil,
      TS_TRUE, LSize);
    LRect := Rect(0, 0, LSize.Width, LSize.Height);
    R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + 30, ItemRect.Bottom);
    RectCenter(LRect, R);
    Winapi.UxTheme.DrawThemeBackground(Theme, DC, iPart, iState, LRect, nil);
    CloseThemeData(Theme);
  end;

  procedure DrawSpecialChar(DC: HDC; Sign: Char; DestRect: TRect;
    const Bold: Boolean = False; const Disabled: Boolean = False);
  var
    LogFont: TLogFont;
    pOldFont: HGDIOBJ;
    AFont: HFONT;
    oldColor: COLORREF;
    OldMode: integer;
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
      oldColor := GetSysColor(COLOR_GRAYTEXT);

    oldColor := SetTextColor(DC, oldColor);
    pOldFont := SelectObject(DC, AFont);
    OldMode := SetBkMode(DC, Transparent);

    Winapi.Windows.DrawText(DC, Sign, 1, DestRect, DT_LEFT or DT_SINGLELINE);
    SetBkMode(DC, OldMode);
    SelectObject(DC, pOldFont);
    SelectObject(DC, oldColor);
    DeleteObject(AFont);
  end;

begin
  DisplayCheckedGlyph := True;
  LTextRect := ItemRect;
  { Fast access . }
  SysItem := Items[Index]; // Do not destroy !!
  DC := Canvas.Handle;
  R := ItemRect;
  Detail := tmPopupItemNormal;
  if isHot in State then
    Detail := tmPopupItemHot;
  if isDisabled in State then
    Detail := tmPopupItemDisabled;
  if Style = isSep then
    begin
      Detail := tmPopupSeparator;
      inc(R.Left, 25);
    end;

  LDetails := StyleServices.GetElementDetails(Detail);

  if (Detail <> tmPopupItemNormal) and (Detail <> tmPopupItemDisabled) then
    StyleServices.DrawElement(DC, LDetails, R);

  if Style = isDropDown then
    DrawSubMenu(ItemRect);

  LImageWidth := 0;
  MI := SysItem.VCLMenuItem;
  if MI <> nil then
    MI := SysItem.VCLMenuItem.Items[Index];
  if MI <> nil then
    begin
      { Draw Vcl PopupMenu Bitmap }
      ImageIndex := MI.ImageIndex;
      if (MI.GetParentMenu.Images <> nil) then
        with MI.GetParentMenu do
          begin
            LImageWidth := Images.Width;
            if (ImageIndex < 0) and (MI.Bitmap <> nil) then
              begin
                Bmp := MI.Bitmap;
               // LImageWidth := Bmp.Width;
                 LImageRect := Rect(0, 0, Bmp.Width, Bmp.Height);
                RectVCenter(LImageRect, ItemRect);
                if not RightToLeft then
                  OffsetRect(LImageRect, 4, 0)
                else
                  begin
                    LImageRect.Left := ItemRect.Right - Images.Width - 4;
                    LImageRect.Right := ItemRect.Right;
                  end;
                 Canvas.Draw( LImageRect.Left, LImageRect.Top,Bmp);

              end
            else if (ImageIndex > -1) then
              begin
                DisplayCheckedGlyph := False;
                LImageRect := Rect(0, 0, Images.Width, Images.Height);
                RectVCenter(LImageRect, ItemRect);
                if not RightToLeft then
                  OffsetRect(LImageRect, 4, 0)
                else
                  begin
                    LImageRect.Left := ItemRect.Right - Images.Width - 4;
                    LImageRect.Right := ItemRect.Right;
                  end;
                Images.Draw(Canvas, LImageRect.Left, LImageRect.Top,
                  ImageIndex);
              end;
          end;
    end
  else if SysItem.Bitmap > 0 then
    begin
      hBmp := SysItem.Bitmap;
      if hBmp < HBMMENU_POPUP_MINIMIZE + 1 then
        begin
          { Draw System PopupMenu Bitmap }
          DisplayCheckedGlyph := False;
          // DrawVisualSysBitmap(hBmp);
          case hBmp of
            HBMMENU_POPUP_RESTORE:
              Sign := MARLETT_RESTORE_CHAR;
            HBMMENU_POPUP_MINIMIZE, HBMMENU_MBAR_MINIMIZE_D:
              Sign := MARLETT_MINIMIZE_CHAR;
            HBMMENU_POPUP_MAXIMIZE:
              Sign := MARLETT_MAXIMIZE_CHAR;
            HBMMENU_POPUP_CLOSE, HBMMENU_MBAR_CLOSE_D:
              Sign := MARLETT_CLOSE_CHAR;
          else
            Sign := Char(0);
          end;
          if Sign <> #0 then
            begin
              LImageRect := Rect(0, 0, 10, 10);
              R := Rect(ItemRect.Left, ItemRect.Top, ItemRect.Left + 30,
                ItemRect.Bottom);
              RectCenter(LImageRect, ItemRect);
              if not RightToLeft then
                LImageRect.Left := ItemRect.Left + 10
              else
                begin
                  LImageRect.Left := ItemRect.Right - 10 - 4;
                  LImageRect.Right := ItemRect.Right;
                end;
              DrawSpecialChar(DC, Sign, LImageRect, False,
                (isDisabled in State));
            end;
        end
      else
        begin
          { Draw PopupMenu Bitmap }
          DisplayCheckedGlyph := False;
          BmpWidth := GetBitmapWidth(hBmp);
          BmpHeight := GetBitmapHeight(hBmp);
          LImageRect := Rect(0, 0, BmpWidth, BmpHeight);
          RectVCenter(LImageRect, ItemRect);
          if not RightToLeft then
            OffsetRect(LImageRect, 4, 0)
          else
            begin
              LImageRect.Left := ItemRect.Right - BmpWidth - 4;
              LImageRect.Right := ItemRect.Right;
            end;
          Icon := BmpToIcon(hBmp);
          DrawIconEX(DC, LImageRect.Left, LImageRect.Top, Icon, BmpWidth,
            BmpHeight, 0, 0, DI_NORMAL);
          DeleteObject(Icon);
        end;
    end;

  if (SysItem.Checked) and (DisplayCheckedGlyph) then
    begin
      Detail := TThemedMenu(integer(tmPopupCheckNormal) +
        integer(SysItem.Disabled));
      if SysItem.RadioCheck then
        Detail := TThemedMenu(integer(tmPopupBulletNormal) +
          integer(SysItem.Disabled));
      LDetails := StyleServices.GetElementDetails(Detail);
      StyleServices.GetElementSize(DC, LDetails, esActual, LSize);
      LImageRect := Rect(0, 0, LSize.Width, LSize.Height);
      RectVCenter(LImageRect, ItemRect);
      if not RightToLeft then
        OffsetRect(LImageRect, 4, 0)
      else
        begin
          LImageRect.Left := ItemRect.Right - LSize.Width - 4;
          LImageRect.Right := ItemRect.Right;
        end;
      StyleServices.DrawElement(DC, LDetails, LImageRect);
    end;

  { Draw Text }
  LTextFormat := [tfLeft, tfVerticalCenter, tfSingleLine, tfExpandTabs,
    tfHidePrefix];
  if not RightToLeft then
    inc(LTextRect.Left, 30)
  else
    begin
      LTextRect.Left := ItemRect.Left;
      LTextRect.Right := ItemRect.Right - 30;
      Exclude(LTextFormat, tfLeft);
      Include(LTextFormat, tfRtlReading);
      Include(LTextFormat, tfRight);
    end;

  if LImageWidth > 0 then
    begin
      if not RightToLeft then
        LTextRect.Left := ItemRect.Left + LImageWidth + 4 + 4
      else
        begin
          LTextRect.Left := ItemRect.Left;
          LTextRect.Right := ItemRect.Right - LImageWidth - 8;
        end;
    end;

  LDetails := StyleServices.GetElementDetails(tmPopupItemNormal);
  if isHot in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemHot);
  if isDisabled in State then
    LDetails := StyleServices.GetElementDetails(tmPopupItemDisabled);

  if SysItem.DefaultItem then
    Canvas.Font.Style := [fsBold];

  DrawText(Canvas.Handle, LDetails, ItemText, LTextRect, LTextFormat);

  { Draw ShortCut Text . }
  if MI <> nil then
    begin
      if MI.ShortCut <> 0 then
        begin
          sShortCut := ShortCutToText(MI.ShortCut);
          LTextRect := ItemRect;
          if RightToLeft then
            begin
              LTextRect.Left := ItemRect.Left + 14;
              LTextRect.Right := LTextRect.Left + Canvas.TextWidth(sShortCut);
            end
          else
            begin
              LTextRect.Left := ItemRect.Right - 14 -
                Canvas.TextWidth(sShortCut);
              LTextRect.Right := ItemRect.Right;
            end;
          DrawText(Canvas.Handle, LDetails, sShortCut, LTextRect, LTextFormat);
        end;
    end;
end;

procedure TSysPopupStyleHook.EraseItem(Canvas: TCanvas; Index: integer;
  ItemRect: TRect);
var
  Bmp: TBitmap;
begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(SysControl.Width, SysControl.Height);
  PaintBackground(Bmp.Canvas);
  BitBlt(Canvas.Handle, ItemRect.Left, ItemRect.Top, ItemRect.Width,
    ItemRect.Height, Bmp.Canvas.Handle, ItemRect.Left, ItemRect.Top, SRCCOPY);
  Bmp.Free;
end;

function TSysPopupStyleHook.GetItemsCount: integer;
begin
  Result := GetMenuItemCount(FMenu);
end;

function TSysPopupStyleHook.GetMenuFromHandle(AHandle: HWND): HMENU;
begin
  Result := SendMessage(AHandle, MN_GETHMENU, 0, 0);
end;

function TSysPopupStyleHook.GetRightToLeft: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_TYPE;
  GetMenuItemInfo(FMenu, 0, True, info);
  Result := ((info.fType and MFT_RIGHTORDER) = MFT_RIGHTORDER) or
    ((info.fType and MFT_RIGHTJUSTIFY) = MFT_RIGHTJUSTIFY);
end;

function TSysPopupStyleHook.GetSysPopupItem(Index: integer): TSysPopupItem;
begin
  Result := nil;
  if (Index > -1) and (index <= Count) then
    begin
      if Assigned(FSysPopupItem) then
        FreeAndNil(FSysPopupItem);
      FSysPopupItem := TSysPopupItem.Create(SysControl, Index, FMenu);
      Result := FSysPopupItem;
    end;
end;

procedure TSysPopupStyleHook.PaintBackground(Canvas: TCanvas);
var
  LDetails: TThemedElementDetails;
begin
  LDetails := StyleServices.GetElementDetails(tmPopupBorders);
  StyleServices.DrawElement(Canvas.Handle, LDetails, SysControl.ClientRect);
end;

procedure TSysPopupStyleHook.UpdateColors;
begin
  inherited;
end;
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}

type
  TSubMenuItemInfo = record
    Menu: HMENU;
    WindowHandle: HWND;
    ItemIndex: integer;
  end;

var
  SubMenuItemInfoArray: array of TSubMenuItemInfo;

procedure TSysPopupStyleHook.MNSELECTITEM(var Message: TMessage);
var
  DC: HDC;
  Canvas: TCanvas;
  Index: integer;
  i: Word;
  L: integer;
  ParentItem: integer;
  ParentPopup: HWND;
  LMenu: HMENU;
begin
  { The undocumented MN_SELECTITEM Message:
    This is the most importants message ,
    Windows sends this message every time when the user
    select an item (not clicking,only select) ...
    wParam=Current Item Index .
    lparam= may be it's unused (not sure).
  }

  Handled := False;
  Canvas := TCanvas.Create;
  ParentPopup := 0;
  ParentItem := -1;

  DC := GetDC(Handle);
  try
    Canvas.Handle := DC;
    Index := integer(Message.WParam);

    { Out of index . }
    if (Index > FCount - 1) or (Index < 0) then
      begin
        { Make sure that wParam hold a valid Item Index .
          if not .. then mouse is not on the PopupMenu
          => Remove Item highlight .
        }
        SetRedraw(True);
        if (FPreviousHotItemIndex > -1) and (FPreviousHotItemIndex < FCount)
        then
          DoDrawItem(Canvas, FPreviousHotItemIndex);
        FPreviousHotItemIndex := -1;
        Handled := True;
        Exit;
      end;

    if not FItemsPainted then
      begin
        { Items are not painted completely . }
        FPreviousHotItemIndex := Index;
        DoDrawItem(Canvas, Index);
        if (Index = Count - 1) then
          FItemsPainted := True;
        Handled := True;
        Exit;
      end;

    L := Length(SubMenuItemInfoArray);
    if L <> 0 then
      begin
        for i := 0 to L - 1 do
          begin
            { Look for SubMenu Parent }
            LMenu := SubMenuItemInfoArray[i].Menu;
            if LMenu = FMenu then
              begin
                ParentPopup := SubMenuItemInfoArray[i].WindowHandle;
                ParentItem := SubMenuItemInfoArray[i].ItemIndex;
                Break;
              end;
          end;
      end;

    if (ParentPopup = Handle) then
      SetRedraw(True) { Allow drawing the current PopupMenu }
    else if ((ParentPopup <> Handle) and (FItemsPainted) and (ParentPopup <> 0))
    then
      begin
        {
          if user jump so fast from the parent PopupMenu to the
          Child PopupMenu (SubMenu) , the hot item of parent Popup menu
          will be draw as a normal item (not hot)..
          So we need to repaint the hot item that drop the child popup menu.
        }
        if (not FParentSubItemPainted) and (ParentItem > -1) then
          begin
            SendMessage(ParentPopup, MN_SELECTITEM, ParentItem, 0);
            FParentSubItemPainted := True;
          end;
        { Don't Redraw the parent of the Current PopupMenu }
        SetRedraw(ParentPopup, False);
      end;

    { if Item can drop a sub Popup Menu }
    if Items[Index].HasSubMenu then
      begin
        L := Length(SubMenuItemInfoArray);
        if L = 0 then
          SetLength(SubMenuItemInfoArray, 1);
        for i := 0 to L do
          { Avoid duplication }
          if SubMenuItemInfoArray[i].Menu <> GetMenuFromHandle(Handle) then
            begin
              inc(L);
              SetLength(SubMenuItemInfoArray, L);
              SubMenuItemInfoArray[L - 1].Menu := GetSubMenu(FMenu, Index);
              SubMenuItemInfoArray[L - 1].WindowHandle := Handle;
              SubMenuItemInfoArray[L - 1].ItemIndex := Index;
              Break;
            end;
      end;

    { If all Items are painted }
    if FItemsPainted then
      begin
        { In order to show / hide SubMenu ,we need to
          process the default message handler . }
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
      end;

    if FPreviousHotItemIndex <> Index then
      begin
        { Draw Item normal . }
        DoDrawItem(Canvas, FPreviousHotItemIndex);
        { Draw Item hot . }
        DoDrawItem(Canvas, Index);
        FPreviousHotItemIndex := Index;
      end;

  finally
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;
  Handled := True;
end;

procedure TSysPopupStyleHook.WMPRINT(var Message: TMessage);
var
  DC: HDC;
  i: integer;
  Canvas: TCanvas;
begin

  FMenu := GetMenuFromHandle(Handle);
  FCount := GetItemsCount;

  if Message.WParam <> 0 then
    DC := HDC(Message.WParam)
  else
    DC := GetDC(Handle);

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := DC;
    PaintBackground(Canvas);
  finally
    Canvas.Free;
    if DC <> HDC(Message.WParam) then
      ReleaseDC(Handle, DC);
  end;

  if Count > -1 then
    begin
      for i := 0 to Count - 1 do
        PostMessage(Handle, MN_SELECTITEM, i, 0);
    end;
  Handled := True;
end;

function IsItemSeparator(Menu: HMENU; ItemIndex: integer): Boolean;
var
  info: TMenuItemInfo;
begin
  {
    Use this function instead of Items[Index].Separator .
    ==> Fast access in WM_KEYDOWN .
  }
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(Menu, ItemIndex, True, info);
  Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
end;

procedure TSysPopupStyleHook.WndProc(var Message: TMessage);
var
  i: integer;
begin
  case Message.Msg of
    MN_SELECTITEM, WM_PRINT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit; { Do not Dispatch . }
          end;
      end;

    WM_PAINT:
      begin
        if not OverridePaint then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
        Exit; { Do not Dispatch . }
      end;

    WM_WINDOWPOSCHANGED:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        SetTimer(Handle, $93, 100, nil);
      end;

    WM_TIMER:
      begin
        if (FItemsPainted) and (Message.WParam = $93) then
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

    MN_BUTTONDOWN:
      begin
        SetRedraw(False);
        Message.Result := CallDefaultProc(Message);
        SetRedraw(True);
      end;

    WM_KEYDOWN:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        FMenu := GetMenuFromHandle(Handle);
        if FPreviousHotItemIndex <> -1 then
          FKeyIndex := FPreviousHotItemIndex;
        case Message.WParam of
          VK_DOWN:
            if FPaintFirstItemFromMenu then
              begin
                if FKeyIndex >= GetMenuItemCount(Menu) - 1 then
                  FKeyIndex := -1;
                inc(FKeyIndex);
                { If the Current Item is Separator then
                  find the next valid item .
                }
                if IsItemSeparator(Menu, FKeyIndex) then
                  for i := FKeyIndex to GetMenuItemCount(Menu) - 1 do
                    if (not IsItemSeparator(Menu, i)) then
                      begin
                        FKeyIndex := i;
                        Break;
                      end;
                SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
                Message.Result := 0;
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
                  if not IsItemSeparator(Menu, i) then
                    begin
                      FKeyIndex := i;
                      Break;
                    end;
              SendMessage(Handle, MN_SELECTITEM, FKeyIndex, 0);
              Message.Result := 0;
            end;
        else
          { Calling the Default Message will cause
            the WM_PAINT Message to be Sent to the PopupMenu Window }
          Message.Result := CallDefaultProc(Message);
        end;
        Exit;
      end;

    WM_ERASEBKGND:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Message.Result := 1;
        Exit; { Do not Dispatch . }
      end;

    WM_PRINTCLIENT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        SendMessage(Handle, WM_PRINT, Message.WParam, Message.lParam);
        Exit;
      end;

    WM_NCCALCSIZE, WM_NCPAINT:
      begin
        if (not OverridePaint) or (not OverridePaintNC) then
          begin
            Message.Result := CallDefaultProc(Message);
            Exit;
          end;
        if not StyleServicesEnabled then
          begin
            Handled := False;
            Exit;
          end;
        Exit; { Do not Dispatch . }
      end;

    WM_DESTROY:
      begin
        SetLength(SubMenuItemInfoArray, 0);
        SubMenuItemInfoArray := nil;
        Handled := False;
      end;

  end;
  inherited;
end;

{ TSysPopupItem }

constructor TSysPopupStyleHook.TSysPopupItem.Create(SysParent: TSysControl;
  Index: integer; Menu: HMENU);
begin
  FMenu := Menu;
  FHandle := SysParent.Handle;
  FSysParent := SysParent;
  FIndex := Index;
end;

destructor TSysPopupStyleHook.TSysPopupItem.Destroy;
begin

  inherited;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemBitmap: HBITMAP;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_CHECKMARKS or MIIM_BITMAP;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := info.hbmpItem;
  if Result = 0 then
    Result := info.hbmpUnchecked;
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  if (FMenu > 0) and (FIndex > -1) then
    GetMenuItemRect(0, FMenu, FIndex, Result);
end;

function TSysPopupStyleHook.TSysPopupItem.GetItemText: String;
var
  Buffer: PChar;
  StrSize: integer;
  info: MenuItemInfo;
begin
  if VCLMenuItem <> nil then
    begin
      Result := VCLMenuItem[FIndex].Caption;
      Exit;
    end;
  { Note:
    The GetMenuString function has been superseded.
    Use the GetMenuItemInfo function to retrieve the menu item text.
  }

  Result := '';

  FillChar(info, SizeOf(MenuItemInfo), Char(0));
  info.cbSize := SizeOf(MenuItemInfo);
  info.fMask := MIIM_STRING or MIIM_FTYPE;
  info.dwTypeData := nil;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  if not(info.fType and MFT_OWNERDRAW = MFT_OWNERDRAW) then
    begin
      { The Size neede for the Buffer . }
      StrSize := info.cch * 2 + 2;
      GetMem(Buffer, StrSize);
      try
        info.dwTypeData := Buffer;
        { inc cch to get the last char . }
        inc(info.cch);
        GetMenuItemInfo(FMenu, FIndex, True, info);
        Result := String(Buffer);
      finally
        FreeMem(Buffer, StrSize);
      end;
      Exit;
    end
  else
    begin
      { if the item is owner draw then we need another way to get
        the item text since , when setting an item to ownerdraw windows
        will destroy the dwTypeData that hold the text . }
      FillChar(info, SizeOf(MenuItemInfo), Char(0));
      info.cbSize := SizeOf(MenuItemInfo);
      info.fMask := MIIM_DATA;
      GetMenuItemInfo(FMenu, FIndex, True, info);
      Result := String(PChar(info.dwItemData));
    end;
end;

function TSysPopupStyleHook.TSysPopupItem.GetVCLMenuItem: TMenuItem;
var
  i, j: integer;
  PopupMenu: TPopupMenu;
  Form: TForm;
  MI: TMenuItem;
begin
  // MI := nil;
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
                  if MI.Handle = FMenu then
                    Exit(MI);
                end
              else if Form.Components[j] is TPopupMenu then
                begin
                  PopupMenu := TPopupMenu(Form.Components[j]);
                  if PopupMenu.Handle = FMenu then
                    Exit(PopupMenu.Items);
                end;
            end;
        end;
    end;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemDisabled: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DISABLED = MFS_DISABLED) or
    (info.fState and MF_DISABLED = MF_DISABLED) or
    (info.fState and MF_GRAYED = MF_GRAYED);
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemRadioCheck: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fType and MFT_RADIOCHECK) = MFT_RADIOCHECK;
end;

function TSysPopupStyleHook.TSysPopupItem.isItemChecked: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_CHECKED) = MFS_CHECKED;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemContainsSubMenu: Boolean;
begin
  Result := (GetSubMenu(FMenu, FIndex) > 0);
end;

function TSysPopupStyleHook.TSysPopupItem.isItemDefault: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_STATE;
  GetMenuItemInfo(FMenu, FIndex, True, info);
  Result := (info.fState and MFS_DEFAULT) = MFS_DEFAULT;
end;

function TSysPopupStyleHook.TSysPopupItem.IsItemSeparator: Boolean;
var
  info: TMenuItemInfo;
begin
  FillChar(info, SizeOf(info), Char(0));
  info.cbSize := SizeOf(TMenuItemInfo);
  info.fMask := MIIM_FTYPE;
  Result := False;
  if (FIndex > -1) and (FIndex < GetMenuItemCount(FMenu) - 1) then
    begin
      GetMenuItemInfo(FMenu, FIndex, True, info);
      Result := (info.fType and MFT_SEPARATOR) = MFT_SEPARATOR;
    end;
end;

initialization

SubMenuItemInfoArray := nil;

if StyleServices.Available then
  TSysStyleManager.RegisterSysStyleHook('#32768', TSysPopupStyleHook);

finalization

TSysStyleManager.UnRegisterSysStyleHook('#32768', TSysPopupStyleHook);

end.
