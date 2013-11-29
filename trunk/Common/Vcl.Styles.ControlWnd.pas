{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.ControlWnd                                                                       }
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
{ The Original Code is uControlWnd.pas.                                                            }
{                                                                                                  }
{ The Initial Developer of the Original Code is SMP3 [S.MAHDI]   e-mail SMP@LIVE.FR                }
{                                                                                                  }
{ Portions created by SMP3 are Copyright (C) 2013 SMP3.                                            }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2013 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.ControlWnd;
{
  +Added Enabled property .
  +Added Focused property .
  +Added Visible property .
}

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.Classes,
  Vcl.Themes,
  Vcl.Graphics,
  System.SysUtils;

const
  seNone = $33333333;

type
  TFrameState = (fActive, fInActive);
  TSysButtonsStates = set of (bsCloseNormal, bsCloseDisabled, bsCloseHot,
    bsClosePushed, bsMaxNormal, bsMaxDisabled, bsMaxHot, bsMaxPushed,
    bsMinNormal, bsMinDisabled, bsMinHot, bsMinPushed, bsHelpNormal,
    bsHelpDisabled, bsHelpHot, bsHelpPushed);

  TButtonState = (BSNormal = 2, BSHot = 3, BSPressed = 4, BSDisabled = 5, BSFocused = 6);

  TIconSize = record
    Width: Longint;
    Height: Longint;
  end;

  TControlWnd = class
  private
    FHandle: THandle;
    FProcInstance: Pointer;
    FOldProc: Pointer;
    FParentHandle: THandle;
    EditBrush: HBRUSH;
    StaticBrush: HBRUSH;
    FFont : TFont;
    function GetClientRect: TRect;
    function GetText: String;
    function GetParentClassName: String;
    function GetWidth: integer;
    function GetHeight: integer;
    function GetStyle: DWORD;
    function GetExStyle: DWORD;
    procedure SetStyle(Value: DWORD);
    procedure SetExStyle(Value: DWORD);
    function GetBorder: Boolean;
    function GetEnabled: Boolean;
    function GetFocused: Boolean;
    function GetVisible: Boolean;
    function GetFontColor: TColor;
    function IsMouseInControl: Boolean;
    function GetClientEdge: Boolean;
    function GetLeft: integer;
    function GetTop: integer;
    function GetClassName: String;
    function GetFont: TFont;

  protected
    function CallOrgWndProc(Message: TMessage): LRESULT;
    procedure WndProc(var Message: TMessage); virtual;
  public
    constructor Create(AHandle: THandle); virtual;
    Destructor Destroy; override;
    procedure Refresh;
    procedure Invalidate;
    procedure InvalidateNC;
    procedure SetRedraw(Value: Boolean);
    function GetParentHandle: HWND;
    function DrawTextCentered(DC: HDC; Details: TThemedElementDetails;
      const R: TRect; S: String): integer;
    procedure DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
     const S: string; var R: TRect; Flags: Cardinal);
     property Handle: THandle read FHandle write FHandle;
    property OldWndProc: Pointer read FOldProc;
    property ParentHandle: THandle read FParentHandle write FParentHandle;
    property ClassNameNative: String read GetClassName;
    property ParentClassName: String read GetParentClassName;
    property ClientRect: TRect read GetClientRect;
    property Text: String read GetText;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Left: integer read GetLeft;
    property Top: integer read GetTop;
    property Style: DWORD read GetStyle write SetStyle;
    property ExStyle: DWORD read GetExStyle write SetExStyle;
    property HasBorder: Boolean read GetBorder;
    property Enabled: Boolean read GetEnabled;
    property Focused: Boolean read GetFocused;
    property Visible: Boolean read GetVisible;
    property Font : TFont read GetFont;
    property FontColor: TColor read GetFontColor;
    property MouseInControl: Boolean read IsMouseInControl;
    property HasClientEdge: Boolean read GetClientEdge;
  end;


  function GetWindowClassName(Window: HWND): String;
  function GetWindowText(Window: HWND): String;
  function IsChildIn32770Dialog(Child: HWND): Boolean;
  function NormalizePoint(hWin: HWND; P: TPoint): TPoint;
  function GET_X_LPARAM(Value: DWORD): Longint;
  function GET_Y_LPARAM(Value: DWORD): Longint;
  procedure FillRectangle(DC: HDC; R: TRect; Color: TColor);
  function IsButtonChecked(Handle: THandle): Boolean;
  function IsButton(BtnWnd: HWND): Boolean;
  function IsButtonGroupeBox(BtnWnd: HWND): Boolean;
  function IsButtonRadioBox(BtnWnd: HWND): Boolean;
  function IsButtonCheckBox(BtnWnd: HWND): Boolean;
  function IsWindowMsgBox(Handle: HWND): Boolean;
  function IsWindowColorDialog(Handle: HWND): Boolean;
  function IsWindowFindDialog(Handle: HWND): Boolean;
  function IsWindowOpenOrSaveDialog(Window: HWND): Boolean;
  function RectVCenter(var R: TRect; Bounds: TRect): TRect;
  function IsSplitButton(BtnWnd: HWND): Boolean;

  procedure DrawBitmap(DC: HDC; hBmp: HBITMAP; const DstRect, SrcRect: TRect;
    AlphaBlend, Transparent: Boolean; const Opacity: Byte = 255;
    const TransparentColor: COLORREF = 0); overload;
  procedure DrawBitmap(DC: HDC; hBmp: HBITMAP; const DstPoint: TPoint;
    SrcRect: TRect; AlphaBlend, Transparent: Boolean; const Opacity: Byte = 255;
   const TransparentColor: COLORREF = 0); overload;

  function GetBitmapHeight(hBmp: HBITMAP): Integer;
  function GetBitmapWidth(hBmp: HBITMAP): Integer;
  function CreateBmp(Width, Height: Integer): HBITMAP;
  function GetBmpDc(hBmp: HBITMAP): HDC;
  procedure ReleaseBmpDc(var DC: HDC);
  function GetIconSize(h_Icon: HICON): TIconSize;
  function GetWindowIcon(hWin: HWND): HICON;


implementation

uses
  System.Types;


const
{$EXTERNALSYM BS_SPLITBUTTON}
  BS_SPLITBUTTON = $0000000C;
{$EXTERNALSYM BS_DEFSPLITBUTTON}
  BS_DEFSPLITBUTTON = $0000000D;
{$EXTERNALSYM BS_COMMANDLINK}
  BS_COMMANDLINK = $0000000E;
{$EXTERNALSYM BS_DEFCOMMANDLINK}
  BS_DEFCOMMANDLINK = $0000000F;

{$WARN ZERO_NIL_COMPAT OFF}



function GetWindowText(Window: HWND): String;
var
  Text: PChar;
begin
  GetMem(Text, 256);
  try
    Winapi.Windows.GetWindowText(Window, Text, 256);
    Result := String(Text);
  finally
    FreeMem(Text, 256);
  end;
end;

{$IFDEF PUREPASCAL}

function GET_X_LPARAM(Value: DWORD): Longint;
begin
  Result := Value and $FFFF;
end;

function GET_Y_LPARAM(Value: DWORD): Longint;
begin
  Result := Value SHR 16;
end;

{$ELSE}

function GET_X_LPARAM(Value: DWORD): Longint;
asm
  {$IFDEF CPUX64}
  MOV EAX,ECX
  {$ENDIF}
  AND EAX,$FFFF
end;

function GET_Y_LPARAM(Value: DWORD): Longint;
asm
  {$IFDEF CPUX64}
  MOV EAX,ECX
  {$ENDIF}
  SHR EAX,16
end;
{$ENDIF}

function NormalizePoint(hWin: HWND; P: TPoint): TPoint;
var
  WindowPos, ClientPos: TPoint;
  FLeft, FTop: Integer;
  R: TRect;
begin
  GetWindowRect(hWin, R);
  FLeft := R.Left;
  FTop := R.Top;

  begin
    WindowPos := Point(FLeft, FTop);
    ClientPos := Point(0, 0);
    ClientToScreen(hWin, ClientPos);
    Result := P;
    ScreenToClient(hWin, Result);
    inc(Result.X, ClientPos.X - WindowPos.X);
    inc(Result.Y, ClientPos.Y - WindowPos.Y);
  end;
end;

function GetWindowClassName(Window: HWND): String;
var
  sClassName: PChar;
begin
  GetMem(sClassName, 256);
  try
    GetClassName(Window, sClassName, 256);
    Result := String(sClassName);
  finally
   FreeMem(sClassName, 256);
  end;
end;

function IsChildIn32770Dialog(Child: HWND): Boolean;
var
  AParent: HWND;
  sClassName: String;
begin
  Result := False;
  AParent := Child;
  while AParent <> 0 do
    begin
      sClassName := GetWindowClassName(AParent);
      if sClassName = '#32770' then
        Exit(True);
      if AParent = 0 then
        Exit(False);
      AParent := GetParent(AParent);
    end;
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

procedure FillRectangle(DC: HDC; R: TRect; Color: TColor);
var
  Brush: HBRUSH;
begin
  Brush := CreateSolidBrush(Color);
  try
    WinApi.Windows.FillRect(DC, R, Brush);
  finally
    DeleteObject(Brush);
  end;
end;

function IsButtonChecked(Handle: THandle): Boolean;
begin
  Result := SendMessage(Handle, BM_GETCHECK, 0, 0) = BST_CHECKED;
end;

function IsButton(BtnWnd: HWND): Boolean;
var
  Style: DWORD;
begin
  Style := GetWindowLongPtr(BtnWnd, GWL_STYLE);
  Result := Style and BS_PUSHBUTTON = BS_PUSHBUTTON;
end;

function IsButtonGroupeBox(BtnWnd: HWND): Boolean;
var
  LStyle: DWORD;
begin
  { Check if Button is GroupeBox }
  LStyle := GetWindowLongPtr(BtnWnd, GWL_STYLE);
  Result := LStyle and BS_GROUPBOX = BS_GROUPBOX;
end;

function IsButtonRadioBox(BtnWnd: HWND): Boolean;
var
  LStyle: DWORD;
begin
  { Check if Button is RadioBox }
  Result := False;
  LStyle := GetWindowLongPtr(BtnWnd, GWL_STYLE);
  if LStyle and BS_SPLITBUTTON <> BS_SPLITBUTTON then
    Result := (LStyle and BS_RADIOBUTTON = BS_RADIOBUTTON);

  if not Result then
    Result := (LStyle = $50010009) or (LStyle = $50020009) or
      (LStyle = $50030009) or (LStyle = $50000009);
end;

function IsButtonCheckBox(BtnWnd: HWND): Boolean;
var
  LStyle: DWORD;
begin
  { Check if Button is CheckBox }
  LStyle := GetWindowLongPtr(BtnWnd, GWL_STYLE);
  Result := LStyle and BS_CHECKBOX = BS_CHECKBOX;
end;

function IsSplitButton(BtnWnd: HWND): Boolean;
var
  LStyle: DWORD;
begin
  { Check if Button is SplitButton }
  LStyle := GetWindowLongPtr(BtnWnd, GWL_STYLE);
  Result := LStyle and BS_SPLITBUTTON = BS_SPLITBUTTON;
end;

function IsWindowMsgBox(Handle: HWND): Boolean;
begin
  Result := ((FindWindowEx(Handle, 0, 'Edit', 0) = 0) and
    (GetDlgItem(Handle, $FFFF) <> 0)) and
    (GetWindowLongPtr(Handle, GWL_USERDATA) <> 0);
end;

function IsWindowColorDialog(Handle: HWND): Boolean;
begin
  Result := (GetDlgItem(Handle, $2C5) <> 0) and (GetDlgItem(Handle, $2D1) <> 0)
    and (GetDlgItem(Handle, $02C6) <> 0) and
    (GetWindowClassName(GetDlgItem(Handle, $FFFF)) = 'Static') and
    (GetWindowClassName(GetDlgItem(Handle, $02DB)) = 'Static');
end;

function IsWindowOpenOrSaveDialog(Window: HWND): Boolean;
var
  C: HWND;
begin
  Result := False;
  C := GetDlgItem(Window, $4A0);
  if C <> 0 then
    begin
      if GetWindowClassName(C) = 'ToolbarWindow32' then
        Result := True;
    end;
end;

function IsWindowFindDialog(Handle: HWND): Boolean;
begin
  Result := (GetWindowClassName(GetDlgItem(Handle, $420)) = 'Button') and
    (GetWindowClassName(GetDlgItem(Handle, $420)) = 'Button') and
    (IsButtonCheckBox(GetDlgItem(Handle, $410))) and
    (IsButtonCheckBox(GetDlgItem(Handle, $411)));
end;

function CreateBmp(Width, Height: Integer): HBITMAP;
var
  bmi: BITMAPINFO;
  pBits: Pointer;
begin
  if (Width = 0) or (Height = 0) then
    begin
      Result := 0;
      Exit;
    end;
  bmi.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
  bmi.bmiHeader.biWidth := Width;
  bmi.bmiHeader.biHeight := Height;
  bmi.bmiHeader.biPlanes := 1;
  bmi.bmiHeader.biBitCount := 32;
  bmi.bmiHeader.biCompression := BI_RGB;
  bmi.bmiHeader.biSizeImage := 0;
  bmi.bmiHeader.biXPelsPerMeter := 0;
  bmi.bmiHeader.biYPelsPerMeter := 0;
  bmi.bmiHeader.biClrUsed := 0;
  bmi.bmiHeader.biClrImportant := 0;
  Result := CreateDIBSection(0, bmi, DIB_RGB_COLORS, pBits, 0, 0);

end;

function GetBmpDc(hBmp: HBITMAP): HDC;
begin
  Result := 0;
  if hBmp <> 0 then
    begin
      Result := CreateCompatibleDC(0);
      SelectObject(Result, hBmp);
    end;
end;

procedure ReleaseBmpDc(var DC: HDC);
begin
  if DC <> 0 then
    DeleteDC(DC);
end;

function GetWindowIcon(hWin: HWND): HICON;
const
  WM_GETICON = $007F;
begin
  Result := 0;
  if hWin = INVALID_HANDLE_VALUE then
    Exit;
  Result := SendMessage(hWin, WM_GETICON, ICON_SMALL, 0);
  if Result <> 0 then
    Exit;
  Result := SendMessage(hWin, WM_GETICON, ICON_BIG, 0);
  if Result <> 0 then
    Exit;
  Result := SendMessage(hWin, WM_GETICON, ICON_SMALL2, 0);
  if Result <> 0 then
    Exit;
  Result := GetClassLongPtr(hWin, GCL_HICON);;
  if Result <> 0 then
    Exit;
  Result := GetClassLongPtr(hWin, GCL_HICONSM);
end;

function GetIconSize(h_Icon: HICON): TIconSize;
var
  IconInfo: TIconInfo;
  Bmp: BITMAP;
begin
  Result.Width := 0;
  Result.Height := 0;
  if h_Icon <> 0 then
    begin
      GetIconInfo(h_Icon, IconInfo);
      GetObject(IconInfo.hbmColor, sizeof(Bmp), @Bmp);
      Result.Width := Bmp.bmWidth;
      Result.Height := Bmp.bmHeight;
    end;
end;


function LoadBitmapFromMem(BmpMem: Pointer): HBITMAP;
var
  bfh: PBitmapFileHeader;
  bih: PBitmapInfoHeader;
  rgb: ^TRGBQuad;
  bi: BITMAPINFO;
  pPixels: PByte;
  ppvBits: Pointer;
  h_Bitmap: HBITMAP;

begin
  bfh := PBitmapFileHeader(BmpMem);
  bih := PBitmapInfoHeader(BmpMem);
  inc(PByte(bih), sizeof(tagBITMAPFILEHEADER));
  rgb := Pointer(bih);
  inc(PByte(rgb), sizeof(tagBITMAPINFOHEADER));
  bi.bmiColors[0] := rgb^;
  bi.bmiHeader := bih^;

  pPixels := PByte(BmpMem);
  inc(pPixels, bfh.bfOffBits);
  h_Bitmap := CreateDIBSection(0, bi, DIB_RGB_COLORS, ppvBits, 0, 0);
  if h_Bitmap = 0 then
    Exit(0);

  SetDIBits(0, h_Bitmap, 0, bih.biHeight, pPixels, bi, DIB_RGB_COLORS);
  Result := h_Bitmap;
end;


function GetAlphaChannelBitmap(DC: HDC; var hBmp: HBITMAP): HBITMAP;  overload;
var
  Bmp: BITMAP;
  BmpInfo: PBITMAPINFO;
  J, i: Integer;
  pBitData: Pointer;
  pData: PByte;
  BmpInfoSize, pBitData_Size: DWORD;
  DC_Need_Deleted: Boolean;
begin
  DC_Need_Deleted := False;
  if (hBmp = 0) then
    Exit(0);
  if DC = 0 then
    begin
      DC := CreateCompatibleDC(0);
      DC_Need_Deleted := True;
    end;

  GetObject(hBmp, sizeof(Bmp), @Bmp);
  if (Bmp.bmWidth = 0) or (Bmp.bmHeight = 0) then
    Exit(0);

  BmpInfoSize := sizeof(BITMAPINFOHEADER) + (256 * sizeof(RGBQUAD));

  getmem(BmpInfo, BmpInfoSize);
  try
    ZeroMemory(BmpInfo, BmpInfoSize);

    BmpInfo.bmiHeader.biSize := sizeof(BITMAPINFOHEADER);
    GetDIBits(DC, hBmp, 0, Bmp.bmHeight, 0, BmpInfo^, DIB_RGB_COLORS);

    pBitData_Size := Bmp.bmWidth * Bmp.bmHeight * sizeof(DWORD);
    getmem(pBitData, pBitData_Size);
    try
      ZeroMemory(pBitData, pBitData_Size);

      pData := PByte(pBitData);
      GetDIBits(DC, hBmp, 0, Bmp.bmHeight, pData, BmpInfo^, DIB_RGB_COLORS);
      if BmpInfo.bmiHeader.biBitCount <> 32 then
        begin
          FreeMem(pBitData);
          Exit(0);
        end;

      for J := 0 to Bmp.bmHeight - 1 do
        begin
          for i := 0 to Bmp.bmWidth - 1 do
            begin
              pData[0] := Byte(Round(pData[0] * pData[3] / 255));
              pData[1] := Byte(Round(pData[1] * pData[3] / 255));
              pData[2] := Byte(Round(pData[2] * pData[3] / 255));
              inc(pData, 4);
            end;
        end;

      SetDIBits(DC, hBmp, 0, Bmp.bmHeight, Pointer(pBitData), BmpInfo^,
        DIB_RGB_COLORS);

    finally
      FreeMem(pBitData, pBitData_Size);
    end;
  finally
    FreeMem(BmpInfo, BmpInfoSize);
  end;

  if DC_Need_Deleted then
    DeleteDC(DC);

  Result := hBmp;
end;

function GetAlphaChannelBitmap(var hBmp: HBITMAP): HBITMAP; overload;
begin
  Result := GetAlphaChannelBitmap(0, hBmp);
end;


function LoadACBitmapFromMem(BmpMem: Pointer): HBITMAP;
var
  hBmp: HBITMAP;
begin
  hBmp := LoadBitmapFromMem(BmpMem);
  Result := GetAlphaChannelBitmap(hBmp);
  DeleteObject(hBmp);
end;

function LoadAlphaChannelBitmap(AFile: PChar): HBITMAP;
var
  hBmp: HBITMAP;
begin
  hBmp := LoadImage(0, AFile, IMAGE_BITMAP, 0, 0, LR_LOADFROMFILE);
  Result := GetAlphaChannelBitmap(hBmp);
end;

{$IFDEF BuildInVCL}
procedure DrawBitmap(Canvas: TCanvas; Bmp: TBitmap;
  const DstRect, SrcRect: TRect; AlphaBlend, Transparent: Boolean;
  const Opacity: Byte = 255);
begin
  DrawBitmap(Canvas.Handle, Bmp.Handle, DstRect, SrcRect, AlphaBlend,
    Transparent, Opacity);
end;
{$ENDIF}

function GetBmpInfo(hBmp: HBITMAP): BITMAP;
begin
  ZeroMemory(@Result, sizeof(BITMAP));
  GetObject(hBmp, sizeof(Result), @Result);
end;

function GetBitmapHeight(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmHeight;
end;

function GetBitmapWidth(hBmp: HBITMAP): Integer;
begin
  Result := GetBmpInfo(hBmp).bmWidth;
end;

procedure DrawBitmap(DC: HDC; hBmp: HBITMAP; const DstRect, SrcRect: TRect;
  AlphaBlend, Transparent: Boolean; const Opacity: Byte = 255;
 const TransparentColor: COLORREF = 0);
var
  BF: TBlendFunction;
  SrcDC: HDC;
begin
  SrcDC := CreateCompatibleDC(DC);
  SelectObject(SrcDC, hBmp);
  SetStretchBltMode(DC, COLORONCOLOR);

  if AlphaBlend then
    begin
      BF.BlendOp := AC_SRC_OVER;
      BF.BlendFlags := 0;
      BF.SourceConstantAlpha := 255;
      BF.AlphaFormat := AC_SRC_ALPHA;
      Winapi.Windows.AlphaBlend(DC, DstRect.Left, DstRect.Top,
        DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, SrcDC,
        SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
        SrcRect.Bottom - SrcRect.Top, BF);
    end
  else if Transparent then
    begin
      Winapi.Windows.TransparentBlt(DC, DstRect.Left, DstRect.Top,
        DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, SrcDC,
        SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
        SrcRect.Bottom - SrcRect.Top, TransparentColor);
    end
  else
    begin
      Winapi.Windows.StretchBlt(DC, DstRect.Left, DstRect.Top,
        DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top, SrcDC,
        SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left,
        SrcRect.Bottom - SrcRect.Top, SRCCOPY);
    end;
  DeleteDC(SrcDC);
end;

function Rect(Left, Top, Right, Bottom: Integer): TRect;
begin
  Result.Left := Left;
  Result.Top := Top;
  Result.Bottom := Bottom;
  Result.Right := Right;
end;

procedure DrawBitmap(DC: HDC; hBmp: HBITMAP; const DstPoint: TPoint;
  SrcRect: TRect; AlphaBlend, Transparent: Boolean; const Opacity: Byte = 255;
 const TransparentColor: COLORREF = 0);
begin
  DrawBitmap(DC, hBmp, Rect(DstPoint.X, DstPoint.Y, DstPoint.X + SrcRect.Width,
    DstPoint.Y + SrcRect.Height), SrcRect, AlphaBlend, Transparent, Opacity,TransparentColor);
end;

{ TControlWnd }

function TControlWnd.CallOrgWndProc(Message: TMessage): LRESULT;
begin
  Result := CallWindowProc(FOldProc, Handle, Message.Msg, Message.wParam,
    Message.lParam);
end;

constructor TControlWnd.Create(AHandle: THandle);
begin
  { Save the original WndProc }
  FOldProc := Pointer(GetWindowLongPtr(AHandle, GWL_WNDPROC));
  FProcInstance := MakeObjectInstance(WndProc);
  FHandle := AHandle;
  FParentHandle := GetParent(FHandle);
  { Changing Window WndProc }
  SetWindowLongPtr(FHandle, GWL_WNDPROC, LONG_PTR(FProcInstance));
  EditBrush := 0;
  StaticBrush := 0;
  FFont := nil;
end;

destructor TControlWnd.Destroy;
begin
  FreeObjectInstance(FProcInstance);
  { We must set WndProc to the Old Proc !! }
  SetWindowLongPtr(Handle, GWL_WNDPROC, LONG_PTR(FOldProc));
  DeleteObject(StaticBrush);
  DeleteObject(EditBrush);
  FFont.Free;
  inherited;
end;

function TControlWnd.IsMouseInControl: Boolean;
var
  P: TPoint;
begin
  GetCursorPos(P);
  Result := WindowFromPoint(P) = Handle;
end;

function TControlWnd.GetText: String;
begin
  Result := GetWindowText(Handle);
end;

function TControlWnd.GetTop: integer;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := R.Top;
end;

function TControlWnd.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

function TControlWnd.GetWidth: integer;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := R.Width;
end;

function TControlWnd.GetBorder: Boolean;
begin
  // Result := ExStyle <> 0;
  Result := (GetWindowLong(Handle, GWL_STYLE) and WS_BORDER = WS_BORDER) or
    (GetWindowLong(Handle, GWL_EXSTYLE) and
    WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE);
end;

function TControlWnd.GetClientEdge: Boolean;
begin
  Result := GetWindowLong(Handle, GWL_EXSTYLE) and
    WS_EX_CLIENTEDGE = WS_EX_CLIENTEDGE;
end;

function TControlWnd.GetClientRect: TRect;
begin
  SetRectEmpty(Result);
  Winapi.Windows.GetClientRect(Handle, Result);
end;

function TControlWnd.GetHeight: integer;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := R.Height;
end;

function TControlWnd.GetLeft: integer;
var
  R: TRect;
begin
  GetWindowRect(Handle, R);
  Result := R.Left;
end;

function TControlWnd.GetClassName: String;
begin
  Result := GetWindowClassName(Handle);
end;

function TControlWnd.GetParentClassName: String;
begin
  Result := GetWindowClassName(ParentHandle);
end;

function TControlWnd.GetParentHandle: HWND;
begin
  Result := GetParent(Handle);
end;

function TControlWnd.GetStyle: DWORD;
begin
  Result := GetWindowLongPtr(Handle, GWL_STYLE);
end;

function TControlWnd.GetEnabled: Boolean;
begin
  Result := IsWindowEnabled(Handle);
end;

function TControlWnd.GetExStyle: DWORD;
begin
  Result := GetWindowLongPtr(Handle, GWL_EXSTYLE);
end;

function TControlWnd.GetFocused: Boolean;
begin
  Result := GetFocus = Handle;
end;

function TControlWnd.GetFont: TFont;
var
  LogFont: TLogFont;
  hFont  : HGDIOBJ;
begin
  if FFont<>nil then
   Exit(FFont);

  hFont := SendMessage(Handle, WM_GETFONT, 0, 0);
  Result := TFont.Create;
  FillChar(LogFont, SizeOf(LogFont), 0);
  GetObject(hFont, SizeOf(logfont), @LogFont);
  Result.Name   := StrPas(LogFont.lffaceName);
  Result.Height := LogFont.lfHeight;
  if LogFont.lfWeight >= FW_MEDIUM then
    Result.Style := Result.Style + [fsBold];
  if LogFont.lfItalic <> 0 then
    Result.Style := Result.Style + [fsItalic];
  if LogFont.lfUnderline <> 0 then
    Result.Style := Result.Style + [fsUnderline];
  if LogFont.lfStrikeout <> 0 then
    Result.Style := Result.Style + [fsStrikeout];
  case (LogFont.lfPitchAndFamily and 3) of
    VARIABLE_PITCH: Result.Pitch := fpVariable;
    FIXED_PITCH: Result.Pitch    := fpFixed;
  end;

  FFont:=Result;
end;


function TControlWnd.GetFontColor: TColor;
begin
  Result := StyleServices.GetSystemColor(clWindowText);
end;

procedure TControlWnd.DrawControlText(Canvas: TCanvas; Details: TThemedElementDetails;
  const S: string; var R: TRect; Flags: Cardinal);
var
  ThemeTextColor: TColor;
  TextFormat: TTextFormatFlags;
begin
  Canvas.Font := Self.Font;
  TextFormat := TTextFormatFlags(Flags);
  if StyleServices.GetElementColor(Details, ecTextColor, ThemeTextColor) then
  begin
    Canvas.Font.Color := ThemeTextColor;
    StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat, Canvas.Font.Color);
  end
  else
  begin
    Canvas.Refresh;
    StyleServices.DrawText(Canvas.Handle, Details, S, R, TextFormat);
  end;
end;


function TControlWnd.DrawTextCentered(DC: HDC; Details: TThemedElementDetails;
  const R: TRect; S: String): integer;
var
  DrawRect: TRect;
  DrawFlags: Cardinal;
  DrawParams: TDrawTextParams;
  SaveIndex: integer;
  sColor: TColor;
begin
  SaveIndex := SaveDC(DC);
  try
    SetBkMode(DC, TRANSPARENT);
    if not StyleServices.GetElementColor(Details, ecTextColor, sColor) then
      sColor := 0;
    SetTextColor(DC, sColor);
    DrawRect := R;
    DrawFlags := DT_END_ELLIPSIS or DT_NOPREFIX or DT_WORDBREAK or
      DT_EDITCONTROL or DT_CENTER;
    DrawText(DC, PChar(S), -1, DrawRect, DrawFlags or DT_CALCRECT);
    DrawRect.Right := R.Right;
    if DrawRect.Bottom < R.Bottom then
      OffsetRect(DrawRect, 0, (R.Bottom - DrawRect.Bottom) div 2)
    else
      DrawRect.Bottom := R.Bottom;
    ZeroMemory(@DrawParams, SizeOf(DrawParams));
    DrawParams.cbSize := SizeOf(DrawParams);
    DrawTextEx(DC, PChar(S), -1, DrawRect, DrawFlags, @DrawParams);
    Result := DrawParams.uiLengthDrawn;
  finally
    RestoreDC(DC, SaveIndex);
  end;
end;

procedure TControlWnd.Refresh;
begin
  SendMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TControlWnd.Invalidate;
begin
  InvalidateNC;
  InvalidateRect(Handle, nil, False);
end;

procedure TControlWnd.InvalidateNC;
begin
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TControlWnd.SetStyle(Value: DWORD);
begin
  SetWindowLongPtr(Handle, GWL_STYLE, Value);
end;

procedure TControlWnd.SetExStyle(Value: DWORD);
begin
  SetWindowLongPtr(Handle, GWL_EXSTYLE, Value);
end;

procedure TControlWnd.SetRedraw(Value: Boolean);
begin
  SendMessage(Handle, WM_SETREDRAW, wParam(Value), 0);
end;

procedure TControlWnd.WndProc(var Message: TMessage);
begin
  { WndProc . }
  { Keep this procedure Clean .=> in order to increase Drawings/Messages Speed }
end;


end.
