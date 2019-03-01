unit Vcl.Styles.Preview;

interface

Uses
  System.Classes, System.Generics.Collections, Winapi.Windows, Vcl.Styles,
  Vcl.Themes, Vcl.Forms, Vcl.Graphics, Vcl.Controls, Vcl.ExtCtrls;

type
  TVisualStylePreview = class(TCustomControl)
    protected
      FStyle           : TCustomStyleServices;
      FIcon            : HICON;
      FCaption         : TCaption;
      FRegion          : HRGN;
      FBitmap          : TBitmap;
      FUnavailableText : string;

      procedure SetStyle(const aStyle : TCustomStyleServices);

      function  GetBorderSize : TRect;
      function  RectVCenter(var aRect : TRect; aBounds : TRect): TRect;
      procedure DrawDefaultPanel(aHandle : THandle; aRect : TRect);
      procedure Paint; override;

    public
      constructor Create(AControl: TComponent); override;
      destructor  Destroy; override;
      procedure   AfterConstruction; override;

      property Icon            : HICON                read FIcon            write FIcon;
      property Style           : TCustomStyleServices read FStyle           write SetStyle;
      property Caption         : TCaption             read FCaption         write FCaption;
      property Bitmap          : TBitmap              read FBitmap          write FBitmap;
      property UnavailableText : string               read FUnavailableText write FUnavailableText;

    published
      property Align;
      property Anchors;
      property Visible;
  end;

implementation

uses
  System.SysUtils, System.Types, System.UITypes;

constructor TVisualStylePreview.Create(AControl: TComponent);
begin
  inherited Create(AControl);

  FRegion  := 0;
  FStyle   := nil;
  FCaption := '';
  FIcon    := 0;
  FBitmap  := nil;
end;

destructor TVisualStylePreview.Destroy;
begin
  try
    if (FRegion <> 0) then
      begin
        DeleteObject(FRegion);
        FRegion := 0;
      end;

    if (FBitmap <> nil) then FreeAndNil(FBitmap);
    if (FStyle  <> nil) then FreeAndNil(FStyle);
    if (FStyle  <> nil) then FreeAndNil(FStyle);
  finally
    inherited Destroy;
  end;
end;

procedure TVisualStylePreview.AfterConstruction;
begin
  inherited AfterConstruction;

  FBitmap             := TBitmap.Create;
  FBitmap.PixelFormat := pf32bit;
end;

procedure TVisualStylePreview.SetStyle(const aStyle : TCustomStyleServices);
begin
  if (FStyle <> nil) then FreeAndNil(FStyle);

  FStyle := aStyle;
  Refresh;
end;

function TVisualStylePreview.GetBorderSize: TRect;
var
  TempSize    : TSize;
  TempDetails : TThemedElementDetails;
begin
  Result := Rect(0, 0, 0, 0);

  TempDetails := Style.GetElementDetails(twCaptionActive);
  Style.GetElementSize(0, TempDetails, esActual, TempSize);
  Result.Top  := TempSize.cy;

  TempDetails := Style.GetElementDetails(twFrameLeftActive);
  Style.GetElementSize(0, TempDetails, esActual, TempSize);
  Result.Left := TempSize.cx;

  TempDetails  := Style.GetElementDetails(twFrameRightActive);
  Style.GetElementSize(0, TempDetails, esActual, TempSize);
  Result.Right := TempSize.cx;

  TempDetails   := Style.GetElementDetails(twFrameBottomActive);
  Style.GetElementSize(0, TempDetails, esActual, TempSize);
  Result.Bottom := TempSize.cy;
end;

function TVisualStylePreview.RectVCenter(var aRect : TRect; aBounds : TRect): TRect;
begin
  OffsetRect(aRect, - aRect.Left, - aRect.Top);
  OffsetRect(aRect, 0, (aBounds.Height - aRect.Height) div 2);
  OffsetRect(aRect, aBounds.Left, aBounds.Top);

  Result := aRect;
end;

procedure TVisualStylePreview.DrawDefaultPanel(aHandle : THandle; aRect : TRect);
var
  TempDetails : TThemedElementDetails;
  TempColor   : TColor;
  TempFlags   : DWORD;
begin
  TempDetails := StyleServices.GetElementDetails(tpPanelBackground);
  StyleServices.DrawElement(aHandle, TempDetails, aRect, nil);

  if (length(FUnavailableText) > 0) then
    begin
      if not(StyleServices.GetElementColor(TempDetails, ecTextColor, TempColor)) then
        TempColor := StyleServices.GetSystemColor(clBtnText);

      TempFlags := DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or DT_CENTER;

      StyleServices.DrawText(aHandle, TempDetails, FUnavailableText, aRect, TTextFormatFlags(TempFlags), TempColor);
    end;
end;

procedure TVisualStylePreview.Paint;
var
  LDetails        : TThemedElementDetails;
  CaptionDetails  : TThemedElementDetails;
  IconDetails     : TThemedElementDetails;
  IconRect        : TRect;
  BorderRect      : TRect;
  CaptionRect     : TRect;
  ButtonRect      : TRect;
  TextRect        : TRect;
  CaptionBitmap   : TBitmap;
  //LBitmap         : TBitmap;
  ThemeTextColor  : TColor;
  ARect           : TRect;
  LRect           : TRect;
  //BlendFunction   : TBlendFunction;
  LRegion         : HRgn;
  i               : Integer;
begin
  if (FStyle = nil) then
    begin
      DrawDefaultPanel(Canvas.Handle, ClientRect);
      exit;
    end;

  BorderRect := GetBorderSize;
  ARect:=ClientRect;
  CaptionBitmap := TBitmap.Create;
  try
    CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);
        {
    LBitmap:=TBitmap.Create;
    LBitmap.PixelFormat:=pf32bit;
    }
    FBitmap.Width :=ClientRect.Width;
    FBitmap.Height:=ClientRect.Height;

    //Draw background
    LDetails.Element := teWindow;
    LDetails.Part := 0;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ARect);

    //Draw caption border
    CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
    LDetails := Style.GetElementDetails(twCaptionActive);

    LRegion := FRegion;
    try
      Style.GetElementRegion(LDetails, ARect, FRegion);
      SetWindowRgn(Handle, FRegion, True);
    finally
      if LRegion <> 0 then
        DeleteObject(LRegion);
    end;

         {
    Style.GetElementRegion(LDetails, ARect, Region);
    SetWindowRgn(Handle, Region, True);
        }

    Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, CaptionRect);
    TextRect := CaptionRect;
    CaptionDetails := LDetails;

    //Draw icon
    IconDetails := Style.GetElementDetails(twSysButtonNormal);
    if not Style.GetElementContentRect(0, IconDetails, CaptionRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    IconRect := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(IconRect, ButtonRect);
    if ButtonRect.Width > 0 then

     if FIcon<>0 then
      DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, FIcon, 0, 0, 0, 0, DI_NORMAL);

    Inc(TextRect.Left, ButtonRect.Width + 5);

    //Draw buttons

    //Close button
    LDetails := Style.GetElementDetails(twCloseButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
     Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Maximize button
    LDetails := Style.GetElementDetails(twMaxButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Minimize button
    LDetails := Style.GetElementDetails(twMinButtonNormal);

    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    //Help button
    LDetails := Style.GetElementDetails(twHelpButtonNormal);
    if Style.GetElementContentRect(0, LDetails, CaptionRect, ButtonRect) then
      Style.DrawElement(CaptionBitmap.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;

    //Draw text
    Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, FCaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

    //Draw caption
    FBitmap.Canvas.Draw(0, 0, CaptionBitmap);

  finally
    CaptionBitmap.Free;
  end;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, CaptionRect);

  //Draw Main Menu
  LDetails:= Style.GetElementDetails(tmMenuBarBackgroundActive);
  LRect:=Rect(BorderRect.Left, BorderRect.Top+1, ARect.Width-BorderRect.Left,BorderRect.Top+1+20);
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, LRect);

  LDetails := Style.GetElementDetails(tmMenuBarItemNormal);
  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);

//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Color: TColor = clNone): Boolean; overload;
//    function DrawText(DC: HDC; Details: TThemedElementDetails;
//      const S: string; var R: TRect; Flags: TTextFormat; Options: TStyleTextOptions): Boolean; overload;

  CaptionRect := Rect(LRect.Left+10,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'File', CaptionRect, [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+40,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Edit', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+70,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'View', CaptionRect,  [tfLeft], ThemeTextColor);
  CaptionRect := Rect(LRect.Left+110,LRect.Top+3, LRect.Right ,LRect.Bottom);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Help', CaptionRect,  [tfLeft], ThemeTextColor);


  //Draw ToolButtons
  for i := 1 to 3 do
  begin
    LDetails := Style.GetElementDetails(ttbButtonNormal);
    ButtonRect.Left:=BorderRect.Left+5+((i-1)*76);
    ButtonRect.Top:=LRect.Top+30;
    ButtonRect.Width:=75;
    ButtonRect.Height:=25;
    Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

    Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
    Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'ToolButton'+IntToStr(i), ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
  end;

  //Draw Normal
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left:=BorderRect.Left+5;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Normal', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Hot
  LDetails := Style.GetElementDetails(tbPushButtonHot);
  ButtonRect.Left:=BorderRect.Left+85;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Hot', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Pressed
  LDetails := Style.GetElementDetails(tbPushButtonPressed);
  ButtonRect.Left:=BorderRect.Left+165;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Pressed', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Disabled
  LDetails := Style.GetElementDetails(tbPushButtonDisabled);
  ButtonRect.Left:=BorderRect.Left+245;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(FBitmap.Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(FBitmap.Canvas.Handle, LDetails, 'Disabled', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  Canvas.Draw(0,0,FBitmap);
end;

end.
