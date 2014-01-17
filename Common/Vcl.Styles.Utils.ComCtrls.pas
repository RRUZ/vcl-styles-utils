{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Utils.ComCtrls                                                                   }
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
{                                                                                                  }
{ Portions created by Safafi Mahdi [SMP3]   e-mail SMP@LIVE.FR                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.                    }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Utils.ComCtrls;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  Winapi.CommCtrl,
  System.Classes,
  System.Types,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Graphics,
  System.SysUtils,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Forms,
  Vcl.ImgList,
  Vcl.GraphUtil,
  Vcl.ComCtrls,
  Vcl.Styles.Utils.Forms,
  Vcl.Controls;

type
  TSysListViewStyleHook = class(TSysScrollingStyleHook)
  private type
{$REGION 'TSysHeaderStyleHook'}
    TSysHeaderStyleHook = class(TMouseTrackSysControlStyleHook)
    private type
{$REGION 'TSysSection'}
      TSysSection = class
      private
        FIndex: Integer;
        FColumnIndex: Integer;
        FImageIndex: Integer;
        FImageListHandle: THandle;
        FText: String;
        FSectionRect: TRect;
        FHeaderHandle: THandle;
        FHasSplitButton: Boolean;
        FTextFormat: TTextFormat;
        FBitmapOnRight: Boolean;
        FShowImage: Boolean;
        FDropDownRect: TRect;
      protected
        procedure DoGetSectionInfo;
      public
        constructor Create(SysParent: TSysControl; Index: Integer); virtual;
        Destructor Destroy; override;
        property Text: string read FText;
        property ImageListHandle: THandle read FImageListHandle;
        property ImageIndex: Integer read FImageIndex;
        property SectionRect: TRect read FSectionRect;
        property ColumnIndex: Integer read FColumnIndex;
        property ShowImage: Boolean read FShowImage;
        property BitmapOnRight: Boolean read FBitmapOnRight;
        property TextFormat: TTextFormat read FTextFormat;
        property HasSplitButton: Boolean read FHasSplitButton;
        property DropDownRect: TRect read FDropDownRect;
      end;
{$ENDREGION}
    private
      FPressedSection: Integer;
      FMouseDown: Boolean;
      FSysSection: TSysSection;
      FListViewStyleHook: TSysListViewStyleHook;
      function GetButtonsCount: Integer;
      function GetItem(Index: Integer): TSysSection;
    protected
      procedure MouseLeave; override;
      procedure WndProc(var Message: TMessage); override;
      procedure Paint(Canvas: TCanvas); override;
      procedure PaintBackground(Canvas: TCanvas); override;

    public
      constructor Create(AHandle: THandle); override;
      Destructor Destroy; override;
      property ButtonsCount: Integer read GetButtonsCount;
      property Items[Index: Integer]: TSysSection read GetItem;
    end;
{$ENDREGION}
  private
    FHeaderHandle: THandle;
    FHeaderStyleHook: TSysHeaderStyleHook;
  protected
    procedure Scroll(Kind: TScrollBarKind; ScrollType: TSysScrollingType;
      Pos, Delta: Integer); override;
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;

  public
    procedure SetSelectedColumn(iCol: Integer);
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property HeaderHandle: THandle read FHeaderHandle write FHeaderHandle;
  end;

  TSysTreeViewStyleHook = class(TSysScrollingStyleHook)
  protected
    procedure UpdateColors; override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  TSysTabControlStyleHook = class(TMouseTrackSysControlStyleHook)
  private
    FHotTabIndex: Integer;
    function GetDisplayRect: TRect;
    function GetTabCount: Integer;
    function GetTabIndex: Integer;
    function GetImages: TCustomImageList;
    function GetTabRect(Index: Integer): TRect;
    function GetTabPosition: TTabPosition;
    function GetTabs(Index: Integer): string;
    procedure AngleTextOut(Canvas: TCanvas; Angle: Integer; X, Y: Integer;
      const Text: string);
  protected
    procedure DrawTab(Canvas: TCanvas; Index: Integer);
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property DisplayRect: TRect read GetDisplayRect;
    property TabCount: Integer read GetTabCount;
    property TabIndex: Integer read GetTabIndex;
    property Images: TCustomImageList read GetImages;
    property TabRect[Index: Integer]: TRect read GetTabRect;
    property TabPosition: TTabPosition read GetTabPosition;
    property Tabs[Index: Integer]: string read GetTabs;
  end;

type
  TSysToolbarButtonState = set of (bsEnabled, bsPressed, bsChecked, bsHidden);
  TSysToolbarButtonStyle = set of (bsBtn, bsSep, bsCheck, bsGroup, bsCheckGroup,
    bsDropDown);

  TSysToolbarStyleHook = class(TMouseTrackSysControlStyleHook)
  private type
   {$REGION 'TSysToolbarButton'}
    TSysToolbarButton = class
    private
      FParent: TSysControl;
      FIndex: Integer;
      FText: String;
      FImageIndex: Integer;
      FState: TSysToolbarButtonState;
      FStyle: TSysToolbarButtonStyle;
      function GetItemRect: TRect;
      procedure DoGetItemInfo;
      function GetDropDownWidth: Integer;
    public
      constructor Create(SysParent: TSysControl; Index: Integer); virtual;
      Destructor Destroy; override;
      property ItemRect: TRect read GetItemRect;
      property Parent: TSysControl read FParent;
      property Text: String Read FText;
      Property ImageIndex: Integer read FImageIndex;
      property State: TSysToolbarButtonState read FState;
      property Style: TSysToolbarButtonStyle read FStyle;
      property DropDownWidth: Integer read GetDropDownWidth;
    end;
   {$ENDREGION}
  var
    FImages: TImageList;
    FDisabledImages: TImageList;
    FSysToolbarButton: TSysToolbarButton;
    FButtonsPainted: Boolean;
    function GetItem(Index: Integer): TSysToolbarButton;
    function GetCount: Integer;
    function IsToolbarTransparent: Boolean;
    function IsToolbarFlat: Boolean;
    function GetShowText: Boolean;
    function IsToolbarList: Boolean;
    function IsToolbarWrapable: Boolean;
  protected
    procedure ApplyImageList;
    procedure PaintBackground(Canvas: TCanvas); override;
    procedure Paint(Canvas: TCanvas); override;
    procedure PaintNC(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
    property Items[index: Integer]: TSysToolbarButton read GetItem;
    property Count: Integer read GetCount;
    Property Flat: Boolean Read IsToolbarFlat;
    Property Transparent: Boolean Read IsToolbarTransparent;
    property ShowText: Boolean read GetShowText;
    property List: Boolean read IsToolbarList;
    property Wrapable: Boolean read IsToolbarWrapable;
  end;

implementation

uses
  Vcl.Styles.Utils.SysControls;


{ TSysListViewStyleHook }

constructor TSysListViewStyleHook.Create(AHandle: THandle);
begin
  inherited;
  FHeaderStyleHook := nil;
  FHeaderHandle := 0;
  {$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder];
  {$ELSE}
  OverridePaint := False;
  OverridePaintNC := True;
  OverrideFont := True;
  {$IFEND}
  OverrideEraseBkgnd := True;
end;

procedure TSysListViewStyleHook.Scroll(Kind: TScrollBarKind;
  ScrollType: TSysScrollingType; Pos, Delta: Integer);
var
  R: TRect;
begin
  if ScrollType = skTracking then
  begin
    if Kind = sbVertical then
    begin
      if ListView_GetView(Handle) = LVS_REPORT then
      begin
        R := Rect(0, 0, 0, 0);
        ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
        Delta := Delta * R.Height;
      end;
      ListView_Scroll(Handle, 0, Delta);
    end;
    if Kind = sbHorizontal then
    begin
      if ListView_GetView(Handle) = LVS_LIST then
      begin
        R := TRect.Empty;
        ListView_GetItemRect(Handle, 0, R, LVIR_BOUNDS);
        Delta := Delta * R.Width;
      end;
      ListView_Scroll(Handle, Delta, 0);
    end;
  end
  else
    inherited;
end;

procedure TSysListViewStyleHook.SetSelectedColumn(iCol: Integer);
begin
  ListView_SetSelectedColumn(Handle, iCol);
end;

destructor TSysListViewStyleHook.Destroy;
begin
  if Assigned(FHeaderStyleHook) then
    FreeAndNil(FHeaderStyleHook);
  inherited;
end;

procedure TSysListViewStyleHook.UpdateColors;
begin
  inherited;
  if OverrideEraseBkgnd then
    Color := StyleServices.GetStyleColor(scListView)
  else
    Color := clWhite;
  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clWindowText;

  ListView_SetBkColor(Handle, Color);
  ListView_SetTextBkColor(Handle, Color);
  ListView_SetTextColor(Handle, ColorToRGB (FontColor));

end;

procedure TSysListViewStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_CREATE, LVM_UPDATE:
      begin
        Message.Result := CallDefaultProc(Message);
        UpdateColors;
        SetSelectedColumn(-1);
        Exit;
      end;
    WM_ERASEBKGND:
      begin
        UpdateColors;
        SetSelectedColumn(-1);
        Message.Result := 1;
        Exit;
      end;
    WM_NOTIFY:
      begin
        HeaderHandle := ListView_GetHeader(Handle);
        // GetWindow(Handle, GW_CHILD);
        if (HeaderHandle <> 0) and (not Assigned(FHeaderStyleHook)) then
        begin
          FHeaderStyleHook := TSysHeaderStyleHook.Create(HeaderHandle);
          FHeaderStyleHook.FListViewStyleHook := Self;
        end;
        Message.Result := CallDefaultProc(Message);
        Exit;
      end;
  else
    inherited;
  end;

end;

{ TSysListViewStyleHook.TSysHeaderStyleHook }

constructor TSysListViewStyleHook.TSysHeaderStyleHook.Create(AHandle: THandle);
begin
  inherited;
  {$IF CompilerVersion > 23}
  StyleElements := [seClient];
  {$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := False;
  {$IFEND}
  FPressedSection := -1;
  FSysSection := nil;
end;

destructor TSysListViewStyleHook.TSysHeaderStyleHook.Destroy;
begin
  if Assigned(FSysSection) then
    FreeAndNil(FSysSection);
  inherited;
end;

function TSysListViewStyleHook.TSysHeaderStyleHook.GetButtonsCount: Integer;
begin
  Result := Header_GetItemCount(Handle);
end;

function TSysListViewStyleHook.TSysHeaderStyleHook.GetItem(Index: Integer)
  : TSysSection;
begin
  Result := nil;
  if (Index > -1) and (index < ButtonsCount) then
  begin
    if Assigned(FSysSection) then
      FreeAndNil(FSysSection);
    FSysSection := TSysSection.Create(SysControl, Index);
    Result := FSysSection;
  end;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.MouseLeave;
begin
  Invalidate;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.Paint(Canvas: TCanvas);
var
  i: Integer;
  Bmp: TBitmap;
  LImageList: TImageList;
  R, TxtRect, ImgRect: TRect;
  LSectionRect: TRect;
  LTextFormat: TTextFormat;
  LText: String;
  LSplitDetails, LDetails: TThemedElementDetails;
  DC: HDC;
  SectionHot: Boolean;
  LDropDownRect: TRect;
  P: TPoint;
begin
  Bmp := TBitmap.Create;
  Bmp.SetSize(SysControl.Width, SysControl.Height);
  Bmp.Canvas.Brush.Color := Color;
  R := Rect(0, 0, Bmp.Width, Bmp.Height);
  Bmp.Canvas.FillRect(R);
  DC := Bmp.Canvas.Handle;

  LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
  StyleServices.DrawElement(DC, LDetails, R);

  for i := 0 to ButtonsCount - 1 do
  begin

    with Items[i] do
    begin
      LSectionRect := SectionRect;
      LTextFormat := TextFormat;
      LText := Text;
      LDropDownRect := DropDownRect;
    end;
    SectionHot := False;
    if (MouseInControl) and (not FMouseDown) then
    begin
      GetCursorPos(P);
      ScreenToClient(Handle, P);
      if LSectionRect.Contains(P) then
        SectionHot := True;
    end;

    LDetails := StyleServices.GetElementDetails(thHeaderItemNormal);
    if SectionHot then
      LDetails := StyleServices.GetElementDetails(thHeaderItemHot);
    if FPressedSection = i then
      LDetails := StyleServices.GetElementDetails(thHeaderItemPressed);
    StyleServices.DrawElement(DC, LDetails, LSectionRect);

    TxtRect := LSectionRect;
    inc(TxtRect.Left, 4);

    if Items[i].HasSplitButton then
    begin
      LSplitDetails := StyleServices.GetElementDetails
        (ttbDropDownButtonGlyphHot);;
      R := LDropDownRect;
      if SectionHot then
      begin
        StyleServices.DrawElement(DC, LSplitDetails, R);
        with Bmp.Canvas do
        begin
          Pen.Color := StyleServices.GetSystemColor(clBtnShadow);
          MoveTo(R.Left, 3);
          LineTo(R.Left, R.Height - 3);
          Pen.Color := StyleServices.GetSystemColor(clBtnHighLight);
          MoveTo(R.Left - 1, 3);
          LineTo(R.Left - 1, R.Height - 3);
        end;
      end;
      dec(TxtRect.Right, R.Width);
    end;

    if (Items[i].ShowImage) and (Items[i].ImageListHandle > 0) then
    begin
      LImageList := TImageList.Create(nil);
      try
        LImageList.Handle := Items[i].ImageListHandle;
        LImageList.Masked := True;
        LImageList.BkColor := clNone; { Transparent bitmap }
        R := LSectionRect;
        ImgRect := Rect(0, 0, LImageList.Width, LImageList.Height);
        ImgRect := RectCenter(ImgRect, R);
        if not Items[i].BitmapOnRight then
        begin
          ImgRect.Left := R.Left + 2;
          ImgRect.Right := ImgRect.Left + 2 + LImageList.Width;
          inc(TxtRect.Left, ImgRect.Width + 2);
        end
        else
        begin
          ImgRect.Left := LSectionRect.Right - LImageList.Width - 2;
          ImgRect.Right := LSectionRect.Right;
          TxtRect.Right := TxtRect.Right - ImgRect.Width - 2;
        end;
        LImageList.Draw(Bmp.Canvas, ImgRect.Left, ImgRect.Top,
          Items[i].ImageIndex);
      finally
        LImageList.Free;
      end;
    end;

    include(LTextFormat, tfSingleLine);
    include(LTextFormat, tfVerticalCenter);
    StyleServices.DrawText(DC, LDetails, LText, TxtRect, LTextFormat);
  end;
  Canvas.Draw(0, 0, Bmp);
  Bmp.Free;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.PaintBackground
  (Canvas: TCanvas);
begin
  // inherited;
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.Paint(Canvas: TCanvas);
begin
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.PaintBackground(Canvas: TCanvas);
begin
  { Leave this block clean . }
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.WndProc
  (var Message: TMessage);
var
  Info: THDHitTestInfo;
begin
  case Message.Msg of

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
      end;

    WM_LBUTTONUP, WM_RBUTTONUP:
      begin
        FMouseDown := False;
        FPressedSection := -1;
      end;

  end;
  inherited;

end;

{ TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection }

constructor TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.Create
  (SysParent: TSysControl; Index: Integer);
begin
  FTextFormat := [];
  FIndex := Index;
  FText := '';
  FImageListHandle := 0;
  FImageIndex := -1;
  FColumnIndex := -1;
  FSectionRect := TRect.Empty;
  FDropDownRect := TRect.Empty;
  FHasSplitButton := False;
  FShowImage := False;
  FHeaderHandle := SysParent.Handle;
  DoGetSectionInfo;
end;

destructor TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.Destroy;
begin

  inherited;
end;

procedure TSysListViewStyleHook.TSysHeaderStyleHook.TSysSection.
  DoGetSectionInfo;
var
  SectionOrder: array of Integer;
  R: TRect;
  item: THDItem;
  Buffer: array [0 .. 255] of Char;
  LRtlReading: Boolean;
begin
  FillChar(Buffer, 255, Char(0));
  SetLength(SectionOrder, Header_GetItemCount(FHeaderHandle));
  Header_GetOrderArray(FHeaderHandle, Header_GetItemCount(FHeaderHandle),
    Pointer(SectionOrder));
  FColumnIndex := SectionOrder[FIndex];
  Header_GetItemRect(FHeaderHandle, ColumnIndex, @R);
  FSectionRect := R;
  FillChar(item, sizeof(item), 0);
  item.mask := HDI_TEXT or HDI_FORMAT or HDI_IMAGE;
  item.pszText := @Buffer;
  item.cchTextMax := Length(Buffer);
  if Header_GetItem(FHeaderHandle, FColumnIndex, item) then
  begin
    with item do
    begin
      FImageIndex := iImage;
      FText := String(pszText);
      FHasSplitButton := (fmt and HDF_SPLITBUTTON = HDF_SPLITBUTTON);
      LRtlReading := (fmt and HDF_RTLREADING = HDF_RTLREADING);
      FTextFormat := [];
      if (fmt and HDF_LEFT = HDF_LEFT) then
        include(FTextFormat, tfLeft)
      else if (fmt and HDF_RIGHT = HDF_RIGHT) then
        include(FTextFormat, tfRight)
      else if (fmt and HDF_CENTER = HDF_CENTER) then
        include(FTextFormat, tfCenter);

      if LRtlReading then
        include(FTextFormat, tfRtlReading);
      FBitmapOnRight := (fmt and HDF_BITMAP_ON_RIGHT = HDF_BITMAP_ON_RIGHT);

      FShowImage := (FImageIndex > -1) and (fmt and HDF_BITMAP = HDF_BITMAP);
    end;
  end;
  R := TRect.Empty;
  if Header_GetItemDropDownRect(FHeaderHandle, FIndex, R) then
    FDropDownRect := R;
  FImageListHandle := Header_GetImageList(FHeaderHandle);
end;


{ TSysTreeViewStyleHook }

constructor TSysTreeViewStyleHook.Create(AHandle: THandle);
begin
  inherited;
  {$IF CompilerVersion > 23}
  StyleElements := [seFont, seBorder];
  {$ELSE}
  OverrideFont:=True;
  OverridePaintNC:=True;
  {$IFEND}
  OverrideEraseBkgnd := True;
end;

destructor TSysTreeViewStyleHook.Destroy;
begin

  inherited;
end;

procedure TSysTreeViewStyleHook.UpdateColors;
begin
  inherited;
  if OverrideEraseBkgnd then
    Color := StyleServices.GetStyleColor(scTreeView)
  else
    Color := clWhite;
  if OverrideFont then
    FontColor := StyleServices.GetSystemColor(clWindowText)
  else
    FontColor := clWindowText;
end;

procedure TSysTreeViewStyleHook.WndProc(var Message: TMessage);
begin
  case Message.Msg of
    WM_ERASEBKGND:
      begin
        UpdateColors;
        if (TreeView_GetBkColor(Handle) <> COLORREF(Color)) then
        begin
          // SetWindowTheme(Handle, '', '');
          TreeView_SetBkColor(Handle, Color);
          TreeView_SetTextColor(Handle, FontColor);
        end;
        Message.Result := CallDefaultProc(Message);
        exit;
      end;
  else
    inherited;
  end;
end;

{ TSysTabControlStyleHook }

procedure TSysTabControlStyleHook.AngleTextOut(Canvas: TCanvas;
  Angle, X, Y: Integer; const Text: string);
var
  SaveIndex: Integer;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    SetBkMode(Canvas.Handle, TRANSPARENT);
    Canvas.Font.Orientation := Angle;
    Canvas.TextOut(X, Y, Text);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;

end;

constructor TSysTabControlStyleHook.Create(AHandle: THandle);
begin
  inherited;
  {$IF CompilerVersion > 23}
  StyleElements := [seClient, seFont];
  {$ELSE}
  OverridePaint := True;
  OverridePaintNC := False;
  OverrideFont := True;

  {$IFEND}
  FHotTabIndex := -1;
end;

destructor TSysTabControlStyleHook.Destroy;
begin

  inherited;
end;


function TSysTabControlStyleHook.GetDisplayRect: TRect;
begin
  Result := Rect(0, 0, 0, 0);
  Result := SysControl.ClientRect;
  SendMessage(Handle, TCM_ADJUSTRECT, 0, IntPtr(@Result));
  Inc(Result.Top, 2);

end;

function TSysTabControlStyleHook.GetImages: TCustomImageList;
begin
  Result := nil;
end;

function TSysTabControlStyleHook.GetTabCount: Integer;
begin
  Result := SendMessage(Handle, TCM_GETITEMCOUNT, 0, 0);
end;

function TSysTabControlStyleHook.GetTabIndex: Integer;
begin
  Result := SendMessage(Handle, TCM_GETCURSEL, 0, 0);
end;

function TSysTabControlStyleHook.GetTabPosition: TTabPosition;
begin
  Result := tpTop;
end;

function TSysTabControlStyleHook.GetTabRect(Index: Integer): TRect;
begin
  Result := Rect(0, 0, 0, 0);
  TabCtrl_GetItemRect(Handle, Index, Result);
end;

function TSysTabControlStyleHook.GetTabs(Index: Integer): string;
var
  TCItem: TTCItem;
  Buffer: array [0 .. 254] of Char;
begin
  FillChar(TCItem, Sizeof(TCItem), 0);

  TCItem.mask := TCIF_TEXT;
  TCItem.pszText := @Buffer;
  TCItem.cchTextMax := Sizeof(Buffer);
  if SendMessageW(Handle, TCM_GETITEMW, Index, IntPtr(@TCItem)) <> 0 then
    Result := TCItem.pszText
  else
    Result := '';

end;

procedure TSysTabControlStyleHook.Paint(Canvas: TCanvas);
var
  R: TRect;
  I, SaveIndex: Integer;
  Details: TThemedElementDetails;
begin
  SaveIndex := SaveDC(Canvas.Handle);
  try
    R := DisplayRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    PaintBackground(Canvas);
  finally
    RestoreDC(Canvas.Handle, SaveIndex);
  end;
  { Draw tabs }
  for I := 0 to TabCount - 1 do
  begin
    // if I = TabIndex then
    // Continue;
    DrawTab(Canvas, I);
  end;
  case TabPosition of
    tpTop:
      InflateRect(R, SysControl.Width - R.Right, SysControl.Height - R.Bottom);
    tpLeft:
      InflateRect(R, SysControl.Width - R.Right, SysControl.Height - R.Bottom);
    tpBottom:
      InflateRect(R, R.Left, R.Top);
    tpRight:
      InflateRect(R, R.Left, R.Top);
  end;

  if StyleServices.Available then
  begin
    Details := StyleServices.GetElementDetails(ttPane);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;
  { Draw active tab }
  if TabIndex >= 0 then
    DrawTab(Canvas, TabIndex);

end;

procedure TSysTabControlStyleHook.DrawTab(Canvas: TCanvas; Index: Integer);
var
  R, LayoutR, GlyphR: TRect;
  ImageWidth, ImageHeight, ImageStep, TX, TY: Integer;
  DrawState: TThemedTab;
  Details: TThemedElementDetails;
  ThemeTextColor: TColor;
  FImageIndex: Integer;
begin
  if (Images <> nil) and (Index < Images.Count) then
  begin
    ImageWidth := Images.Width;
    ImageHeight := Images.Height;
    ImageStep := 3;
  end
  else
  begin
    ImageWidth := 0;
    ImageHeight := 0;
    ImageStep := 0;
  end;

  R := TabRect[Index];
  if R.Left < 0 then
    Exit;

  if TabPosition in [tpTop, tpBottom] then
  begin
    if Index = TabIndex then
      InflateRect(R, 0, 2);
  end
  else if Index = TabIndex then
    Dec(R.Left, 2)
  else
    Dec(R.Right, 2);

  // Canvas.Font.Assign(TCustomTabControl(Control).Font);
  LayoutR := R;
  DrawState := ttTabDontCare;
  case TabPosition of
    tpTop:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemHot
        else
          DrawState := ttTabItemNormal;
      end;
    tpLeft:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemLeftEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemLeftEdgeHot
        else
          DrawState := ttTabItemLeftEdgeNormal;
      end;
    tpBottom:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemBothEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemBothEdgeHot
        else
          DrawState := ttTabItemBothEdgeNormal;
      end;
    tpRight:
      begin
        if Index = TabIndex then
          DrawState := ttTabItemRightEdgeSelected
        else if (Index = FHotTabIndex) and MouseInControl then
          DrawState := ttTabItemRightEdgeHot
        else
          DrawState := ttTabItemRightEdgeNormal;
      end;
  end;

  if StyleServices.Available then
  begin
    Details := StyleServices.GetElementDetails(DrawState);
    StyleServices.DrawElement(Canvas.Handle, Details, R);
  end;

  { Image }

  FImageIndex := Index;

  if (Images <> nil) and (FImageIndex >= 0) and (FImageIndex < Images.Count)
  then
  begin
    GlyphR := LayoutR;
    case TabPosition of
      tpTop, tpBottom:
        begin
          GlyphR.Left := GlyphR.Left + ImageStep;
          GlyphR.Right := GlyphR.Left + ImageWidth;
          LayoutR.Left := GlyphR.Right;
          GlyphR.Top := GlyphR.Top + (GlyphR.Bottom - GlyphR.Top) div 2 -
            ImageHeight div 2;
          if (TabPosition = tpTop) and (Index = TabIndex) then
            OffsetRect(GlyphR, 0, -1)
          else if (TabPosition = tpBottom) and (Index = TabIndex) then
            OffsetRect(GlyphR, 0, 1);
        end;
      tpLeft:
        begin
          GlyphR.Bottom := GlyphR.Bottom - ImageStep;
          GlyphR.Top := GlyphR.Bottom - ImageHeight;
          LayoutR.Bottom := GlyphR.Top;
          GlyphR.Left := GlyphR.Left + (GlyphR.Right - GlyphR.Left) div 2 -
            ImageWidth div 2;
        end;
      tpRight:
        begin
          GlyphR.Top := GlyphR.Top + ImageStep;
          GlyphR.Bottom := GlyphR.Top + ImageHeight;
          LayoutR.Top := GlyphR.Bottom;
          GlyphR.Left := GlyphR.Left + (GlyphR.Right - GlyphR.Left) div 2 -
            ImageWidth div 2;
        end;
    end;
    if StyleServices.Available then
      StyleServices.DrawIcon(Canvas.Handle, Details, GlyphR, Images.Handle,
        FImageIndex);
  end;

  { Text }
  if StyleServices.Available then
  begin
    if (TabPosition = tpTop) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, -1)
    else if (TabPosition = tpBottom) and (Index = TabIndex) then
      OffsetRect(LayoutR, 0, 1);

    if TabPosition = tpLeft then
    begin
      TX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 -
        Canvas.TextHeight(Tabs[Index]) div 2;
      TY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 +
        Canvas.TextWidth(Tabs[Index]) div 2;
      if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor)
      then
        Canvas.Font.Color := ThemeTextColor;
      AngleTextOut(Canvas, 900, TX, TY, Tabs[Index]);
    end
    else if TabPosition = tpRight then
    begin
      TX := LayoutR.Left + (LayoutR.Right - LayoutR.Left) div 2 +
        Canvas.TextHeight(Tabs[Index]) div 2;
      TY := LayoutR.Top + (LayoutR.Bottom - LayoutR.Top) div 2 -
        Canvas.TextWidth(Tabs[Index]) div 2;
      if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor)
      then
        Canvas.Font.Color := ThemeTextColor;
      AngleTextOut(Canvas, -900, TX, TY, Tabs[Index]);
    end
    else
      StyleServices.DrawText(Canvas.Handle, Details, Tabs[Index], LayoutR,
        [tfSingleLine, tfVerticalCenter, tfCenter, tfNoClip]);
    // DrawControlText(Canvas, Details, Tabs[Index], LayoutR,
    // DT_VCENTER or DT_CENTER or DT_SINGLELINE or DT_NOCLIP);
  end;
end;

procedure TSysTabControlStyleHook.PaintBackground(Canvas: TCanvas);
begin
  inherited;

end;

procedure TSysTabControlStyleHook.PaintNC(Canvas: TCanvas);
begin
  inherited;

end;

procedure TSysTabControlStyleHook.WndProc(var Message: TMessage);
begin
  inherited;
end;

{ TSysToolbarStyleHook }

{$REGION 'TSysToolbarStyleHook'}
  constructor TSysToolbarStyleHook.Create(AHandle: THandle);
  begin
    inherited;
    {$IF CompilerVersion > 23}
    StyleElements := [seClient, seFont];
    {$ELSE}
    OverridePaint:=True;
    OverrideFont:=True;
    {$IFEND}

    OverrideEraseBkgnd := False;
    FImages := nil;
    FDisabledImages := nil;
    FSysToolbarButton := nil;
    FButtonsPainted := False;
  end;

  destructor TSysToolbarStyleHook.Destroy;
  begin
    if Assigned(FImages) then
      FreeAndNil(FImages);
    if Assigned(FDisabledImages) then
      FreeAndNil(FDisabledImages);
    if Assigned(FSysToolbarButton) then
      FreeAndNil(FSysToolbarButton);
    inherited;
  end;

  function TSysToolbarStyleHook.GetItem(Index: Integer): TSysToolbarButton;
  begin
    Result := nil;
    if (Index > -1) and (index <= Count) then
    begin
      if Assigned(FSysToolbarButton) then
        FreeAndNil(FSysToolbarButton);
      FSysToolbarButton := TSysToolbarButton.Create(SysControl, Index);
      Result := FSysToolbarButton;
    end;
  end;

  function TSysToolbarStyleHook.GetShowText: Boolean;
  begin
    Result := (SysControl.Style and BTNS_SHOWTEXT = BTNS_SHOWTEXT);
  end;

  function TSysToolbarStyleHook.IsToolbarFlat: Boolean;
  begin
    { MSDN :
      In a flat toolbar, both the toolbar and the buttons are transparent
      and hot-tracking is enabled.
    }
    Result := (SysControl.Style and TBSTYLE_FLAT = TBSTYLE_FLAT)
  end;

  function TSysToolbarStyleHook.IsToolbarList: Boolean;
  begin
    Result := (SysControl.Style and TBSTYLE_LIST = TBSTYLE_LIST);
  end;

  function TSysToolbarStyleHook.IsToolbarTransparent: Boolean;
  begin
    { MSDN:
      In a transparent toolbar, the toolbar is transparent but the buttons are not.
    }
    Result := (SysControl.Style and TBSTYLE_TRANSPARENT = TBSTYLE_TRANSPARENT)
  end;

  function TSysToolbarStyleHook.IsToolbarWrapable: Boolean;
  begin
    Result := (SysControl.Style and TBSTYLE_WRAPABLE = TBSTYLE_WRAPABLE)
  end;

  function TSysToolbarStyleHook.GetCount: Integer;
  begin
    Result := SendMessage(Handle, TB_BUTTONCOUNT, 0, 0);
  end;

  procedure TSysToolbarStyleHook.ApplyImageList;
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

  procedure TSysToolbarStyleHook.Paint(Canvas: TCanvas);
  var
    i: Integer;
    ItemRect, R, R2: TRect;
    LDetails: TThemedElementDetails;
    DC: HDC;
    LButtonHot: Boolean;
    P: TPoint;
    LStyle: TSysToolbarButtonStyle;
    LState: TSysToolbarButtonState;
    Bmp: TBitmap;
    imgRect, TxtRect: TRect;
    LText: String;
    LImageIndex, LDropDownWidth: Integer;
    TxtFlags: DWORD;
    TxtFormat: TTextFormat;
  begin

    Bmp := TBitmap.Create;
    try
      ApplyImageList;
      if Assigned(FImages) then
      begin
        FImages.Masked := True;
        FImages.BkColor := clNone; { Transparent bitmap }
      end;
      imgRect := Rect(0, 0, 0, 0);
      TxtRect := Rect(0, 0, 0, 0);
      Bmp.SetSize(SysControl.Width, SysControl.Height);
      R := Rect(0, 0, Bmp.Width, Bmp.Height);
      // Bmp.Canvas.Brush.Color := StyleServices.GetStyleColor(scWindow);
      // Bmp.Canvas.FillRect(R);
      DC := Bmp.Canvas.Handle;
      DrawParentBackground(DC);

      TxtFlags := 0;
      if (SysControl.Style and TBSTYLE_NOPREFIX = TBSTYLE_NOPREFIX) then
        TxtFlags := DT_NOPREFIX;

      if Flat or Transparent then
      begin
        { Dont paint the toolbar background => the toolbar is transparent . }
      end
      else
      begin
        { Toolbar is not transparent }
        LDetails.Element := teToolBar;
        LDetails.Part := 0;
        LDetails.State := 0;
        if StyleServices.HasTransparentParts(LDetails) then
          StyleServices.DrawParentBackground(Handle, DC, LDetails, False);
        StyleServices.DrawElement(DC, LDetails, R);
      end;
    except
      Bmp.Free;
      exit;
    end;

    try
      { Draw toolbar buttons }
      for i := 0 to Count - 1 do
      begin
        if i = Count - 1 then
          FButtonsPainted := True;

        ItemRect := Items[i].ItemRect;
        with Items[i] do
        begin
          LState := State;
          LStyle := Style;
          LText := Text;
          LImageIndex := ImageIndex;
          LDropDownWidth := DropDownWidth;
        end;

        LButtonHot := False;
        if not(bsHidden in LState) then
        begin
          if MouseInControl then
          begin
            GetCursorPos(P);
            ScreenToClient(Handle, P);
            if ItemRect.Contains(P) then
              LButtonHot := True;
          end;

          if (bsEnabled in LState) then
            LDetails := StyleServices.GetElementDetails(ttbButtonNormal)
          else
            LDetails := StyleServices.GetElementDetails(ttbButtonDisabled);
          if (LButtonHot) and (bsEnabled in LState) then
          begin
            LDetails := StyleServices.GetElementDetails(ttbButtonHot);
          end;
          if (bsPressed in LState) and (bsEnabled in LState) then
            LDetails := StyleServices.GetElementDetails(ttbButtonPressed);

          if bsChecked in LState then
            LDetails := StyleServices.GetElementDetails(ttbButtonChecked);

          if not(bsSep in LStyle) then
          begin
            if Flat then
            begin
              // Bmp.Canvas.FillRect(ItemRect);
              DrawParentBackground(DC, @ItemRect);
              if (LButtonHot or (bsPressed in LState) or (bsChecked in LState))
                and (bsEnabled in LState) then
              begin
                StyleServices.DrawElement(DC, LDetails, ItemRect);
              end;
            end
            else
              StyleServices.DrawElement(DC, LDetails, ItemRect);
          end
          else
          begin
            LDetails := StyleServices.GetElementDetails(ttbSeparatorNormal);
            StyleServices.DrawElement(DC, LDetails, ItemRect);
          end;

          if not(bsSep in LStyle) then
          begin
            R := ItemRect;
            imgRect := TRect.Empty;
            if Assigned(FImages) then
              imgRect := Rect(0, 0, FImages.Width, FImages.Height);
            imgRect := CenteredRect(R, imgRect);

            if bsDropDown in LStyle then
            begin
              { If button is DropDown then draw the button glyph. }
              R := ItemRect;
              R := Rect(R.Right - LDropDownWidth, R.Top, R.Right, R.Bottom);
              if bsEnabled in LState then
                LDetails := StyleServices.GetElementDetails
                  (ttbDropDownButtonGlyphNormal)
              else
                LDetails := StyleServices.GetElementDetails
                  (ttbDropDownButtonGlyphDisabled);
              if (LButtonHot and (bsEnabled in LState)) then
                LDetails := StyleServices.GetElementDetails
                  (ttbDropDownButtonGlyphHot);
              if ((bsPressed in LState) and (bsEnabled in LState)) then
                LDetails := StyleServices.GetElementDetails
                  (ttbDropDownButtonGlyphPressed);
              StyleServices.DrawElement(DC, LDetails, R);

              { Adjust bitmap position }

              if Assigned(FImages) then
                imgRect := Rect(0, 0, FImages.Width, FImages.Height);
              R := ItemRect;
              R.Right := R.Right - LDropDownWidth;
              imgRect := CenteredRect(R, imgRect);
              inc(imgRect.Left, 2);
            end;

            { Adjust bitmap & Text positions }
            if Wrapable then
            begin
              R := Rect(0, 0, 0, 0);
              if (ShowText and not List) then
              begin
                Winapi.Windows.DrawText(DC, LText, -1, R,
                  DT_CENTER or DT_CALCRECT);
              end;
              imgRect.Offset(0, -R.Height);
            end
            else if List then
            begin
              R := Rect(0, 0, 0, 0);
              if ShowText then
              begin
                Winapi.Windows.DrawText(DC, LText, -1, R,
                  DT_CENTER or DT_CALCRECT or TxtFlags);
              end;
              imgRect := Rect(0, 0, FImages.Width, FImages.Height);
              R2 := ItemRect;
              dec(R2.Right, R.Width + 2);
              imgRect := CenteredRect(R2, imgRect);
            end;

            { Draw Bitmap }
            if (LImageIndex > -1) and (Assigned(FImages)) then
            begin
              if bsEnabled in LState then
                FImages.DrawingStyle := Vcl.ImgList.TDrawingStyle.dsNormal
              else
                FImages.DrawingStyle := Vcl.ImgList.TDrawingStyle.dsSelected;
              FImages.Draw(Bmp.Canvas, imgRect.Left, imgRect.Top, LImageIndex);
            end;

            { Draw Text }
            TxtRect := Rect(0, 0, 0, 0);
            if ShowText then
            begin
              if not List then
              begin
                { Text appear under the button bitmap }
                if (imgRect.Width > 0) and (LImageIndex > -1) then
                  TxtRect := Rect(ItemRect.Left, imgRect.Bottom, ItemRect.Right,
                    ItemRect.Bottom)
                else
                  TxtRect := ItemRect;
                if LText <> '' then
                  DrawTextCentered(DC, LDetails, TxtRect, LText, TxtFlags);
              end
              else
              begin
                { List }
                { Text appear to the right of the button bitmap }
                if (imgRect.Width > 0) and (LImageIndex > -1) then
                  TxtRect := Rect(imgRect.Right + 2, ItemRect.Top, ItemRect.Right,
                    ItemRect.Bottom)
                else
                  TxtRect := ItemRect;
                TxtFormat := [tfCenter, tfVerticalCenter, tfSingleLine, tfLeft];
                if TxtFlags <> 0 then
                  Include(TxtFormat, tfNoPrefix);
                if LText <> '' then
                  StyleServices.DrawText(DC, LDetails, LText, TxtRect, TxtFormat);
              end;
            end;
          end;
        end;
      end;
      Canvas.Draw(0, 0, Bmp);
    finally
      Bmp.Free;
    end;
  end;

  procedure TSysToolbarStyleHook.PaintBackground(Canvas: TCanvas);
  begin
    inherited;

  end;

  procedure TSysToolbarStyleHook.PaintNC(Canvas: TCanvas);
  begin
    inherited;
  end;

  procedure TSysToolbarStyleHook.WndProc(var Message: TMessage);
  begin
    inherited;
  end;
{$ENDREGION}

{$REGION 'TSysToolbarButton'}
  { TSysToolbarStyleHook.TSysToolbarButton }

  constructor TSysToolbarStyleHook.TSysToolbarButton.Create
    (SysParent: TSysControl; Index: Integer);
  begin
    FIndex := Index;
    FParent := SysParent;
    FText := '';
    FImageIndex := -1;
    FState := [];
    FStyle := [];
    DoGetItemInfo;
  end;

  destructor TSysToolbarStyleHook.TSysToolbarButton.Destroy;
  begin
    inherited;
  end;

  Procedure TSysToolbarStyleHook.TSysToolbarButton.DoGetItemInfo;
  const
    BufferSize = 255;
  var
    TB: TTBButton;
    Buffer: array [0 .. BufferSize - 1] of Char;
    BtnInfo: TTBButtonInfo;
  begin
    FillChar(Buffer, BufferSize, Char(0));
    FillChar(TB, SizeOf(TB), 0);
    SendMessage(FParent.Handle, TB_GETBUTTON, FIndex, IntPtr(@TB));
    FillChar(BtnInfo, SizeOf(BtnInfo), Char(0));
    BtnInfo.cbSize := SizeOf(TTBButtonInfo);
    BtnInfo.dwMask := TBIF_STATE or TBIF_STYLE or TBIF_IMAGE or TBIF_TEXT;
    BtnInfo.cchText := BufferSize;
    BtnInfo.pszText := @Buffer;
    SendMessage(FParent.Handle, TB_GETBUTTONINFO, TB.idCommand, lParam(@BtnInfo));
    BtnInfo.fsStyle := TB.fsStyle;
    SendMessage(FParent.Handle, TB_GETBUTTONTEXT, TB.idCommand,
      lParam(BtnInfo.pszText));
    FText := String(Buffer);
    FImageIndex := BtnInfo.iImage;
    with BtnInfo do
    begin
      { Button State }
      if fsState and TBSTATE_ENABLED = TBSTATE_ENABLED then
        Include(FState, bsEnabled);
      if fsState and TBSTATE_PRESSED = TBSTATE_PRESSED then
        Include(FState, bsPressed);
      if fsState and TBSTATE_CHECKED = TBSTATE_CHECKED then
        Include(FState, bsChecked);
      if fsState and TBSTATE_HIDDEN = TBSTATE_HIDDEN then
        Include(FState, bsHidden);

      { Button Style }
      if fsStyle and TBSTYLE_BUTTON = TBSTYLE_BUTTON then
        Include(FStyle, bsBtn);
      if fsStyle and TBSTYLE_SEP = TBSTYLE_SEP then
        Include(FStyle, bsSep);
      if fsStyle and TBSTYLE_CHECK = TBSTYLE_CHECK then
        Include(FStyle, bsCheck);
      if fsStyle and TBSTYLE_GROUP = TBSTYLE_GROUP then
        Include(FStyle, bsGroup);
      if fsStyle and TBSTYLE_CHECKGROUP = TBSTYLE_CHECKGROUP then
        Include(FStyle, bsCheckGroup);
      if (fsStyle and TBSTYLE_DROPDOWN = TBSTYLE_DROPDOWN) or
        (fsStyle and BTNS_WHOLEDROPDOWN = BTNS_WHOLEDROPDOWN) then
        Include(FStyle, bsDropDown);

    end;

  end;

  function TSysToolbarStyleHook.TSysToolbarButton.GetItemRect: TRect;
  begin
    Result := TRect.Empty;
    if not BOOL(SendMessage(FParent.Handle, TB_GETITEMRECT, FIndex,
      lParam(@Result))) then
      Result := TRect.Empty;
  end;

  function TSysToolbarStyleHook.TSysToolbarButton.GetDropDownWidth: Integer;
  var
    R: TRect;
  begin
    if BOOL(SendMessage(FParent.Handle, TB_GETITEMDROPDOWNRECT, FIndex,
      lParam(@R))) then
      Result := R.Right - R.Left
    else
      Result := 15; // default width when runtime themes are enabled
  end;

{$ENDREGION}

initialization


if StyleServices.Available then
begin
  with TSysStyleManager do
  begin
    RegisterSysStyleHook('#32770', TSysDialogStyleHook);
    RegisterSysStyleHook('ToolbarWindow32', TSysToolbarStyleHook);
    RegisterSysStyleHook('SysListView32', TSysListViewStyleHook);
    RegisterSysStyleHook('SysTabControl32', TSysTabControlStyleHook);
    RegisterSysStyleHook('SysTreeView32', TSysTreeViewStyleHook);
    RegisterSysStyleHook('ScrollBar', TSysScrollBarStyleHook);
  end;
end;

finalization

with TSysStyleManager do
begin
  UnRegisterSysStyleHook('#32770', TSysDialogStyleHook);
  UnRegisterSysStyleHook('ToolbarWindow32', TSysToolbarStyleHook);
  UnRegisterSysStyleHook('SysListView32', TSysListViewStyleHook);
  UnRegisterSysStyleHook('SysTabControl32', TSysTabControlStyleHook);
  UnRegisterSysStyleHook('SysTreeView32', TSysTreeViewStyleHook);
  UnRegisterSysStyleHook('ScrollBar', TSysScrollBarStyleHook);
end;

end.
