{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.FormStyleHooks                                                                   }
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
{ The Original Code is Vcl.Styles.FormStyleHooks.pas.                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}

unit Vcl.Styles.FormStyleHooks;

interface

uses
  Winapi.Windows,
  Vcl.Controls,
  Vcl.Graphics,
  Vcl.Forms;

type
  TFormStyleHookBackround=class(TFormStyleHook)
  strict private
    type
      TSettings = class
      strict private
        FColor: TColor;
        FImageLocation: string;
        FBitmap: TBitmap;
        FUseColor: Boolean;
        FUseImage: Boolean;
        FEnabled: Boolean;
        procedure SetColor(const Value: TColor);
        procedure SetImageLocation(const Value: string);
        procedure SetUseColor(const Value: Boolean);
        procedure SetUseImage(const Value: Boolean);
      public
        property UseImage : Boolean read FUseImage write SetUseImage;
        property UseColor : Boolean read FUseColor write SetUseColor;
        property Color : TColor read FColor write SetColor;
        property ImageLocation : string read FImageLocation write SetImageLocation;
        property Bitmap : TBitmap read FBitmap;
        property Enabled : Boolean read FEnabled write FEnabled;
        constructor Create;
        destructor  Destroy;override;
      end;
    class var FNCSettings: TSettings;
    class var FBackGroundSettings: TSettings;
    class var FMergeImages: boolean;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    class constructor Create;
    class destructor  Destroy;
  public
    class property MergeImages: boolean read FMergeImages write FMergeImages;
    class property NCSettings : TSettings read FNCSettings;
    class property BackGroundSettings : TSettings read FBackGroundSettings;
  end;


implementation

Uses
  System.SysUtils,
  System.Classes,
  Vcl.Imaging.Jpeg,
  Vcl.Imaging.pngimage,
  Vcl.Imaging.GIFImg,
  Vcl.Themes,
  Vcl.Styles;

type
  TCustomFormHack       = class(TCustomForm);
  TFormStyleHookHelper  = class helper for TFormStyleHook
  private
    function  GetFCloseButtonRect: TRect;
    procedure SetFCloseButtonRect(const Value: TRect);
    function GetFCaptionRect: TRect;
    function GetFHelpButtonRect: TRect;
    function GetFMaxButtonRect: TRect;
    function GetFMinButtonRect: TRect;
    function GetFSysMenuButtonRect: TRect;
    procedure SetFCaptionRect(const Value: TRect);
    procedure SetFHelpButtonRect(const Value: TRect);
    procedure SetFMaxButtonRect(const Value: TRect);
    procedure SetFMinButtonRect(const Value: TRect);
    procedure SetFSysMenuButtonRect(const Value: TRect);
    function GetFFormActive: Boolean;
    function GetFWidth: Integer;
    function GetFPressedButton: Integer;
    function GetFHotButton: Integer;
    function GetFHeight: integer;
  public
    property _FCloseButtonRect : TRect read GetFCloseButtonRect Write SetFCloseButtonRect;
    property _FMaxButtonRect : TRect read GetFMaxButtonRect Write SetFMaxButtonRect;
    property _FMinButtonRect : TRect read GetFMinButtonRect Write SetFMinButtonRect;
    property _FHelpButtonRect : TRect read GetFHelpButtonRect Write SetFHelpButtonRect;
    property _FSysMenuButtonRect : TRect read GetFSysMenuButtonRect Write SetFSysMenuButtonRect;
    property _FCaptionRect : TRect read GetFCaptionRect Write SetFCaptionRect;
    function _GetBorderSize: TRect;
    property _FFormActive: Boolean read GetFFormActive;
    property _FWidth: Integer read GetFWidth;
    property _FHeight : integer read GetFHeight;
    function _GetIconFast: TIcon;
    property _FPressedButton: Integer read GetFPressedButton;
    property _FHotButton: Integer read GetFHotButton;
    procedure MainMenuBarHookPaint(Canvas: TCanvas);
  end;

{ TFormStyleHookHelper }
function TFormStyleHookHelper.GetFCaptionRect: TRect;
begin
 result:=Self.FCaptionRect;
end;

function TFormStyleHookHelper.GetFCloseButtonRect: TRect;
begin
 result:=Self.FCloseButtonRect;
end;

function TFormStyleHookHelper.GetFFormActive: Boolean;
begin
 Result:=Self.FFormActive;
end;

function TFormStyleHookHelper.GetFHeight: integer;
begin
Result:=Self.FHeight;
end;

function TFormStyleHookHelper.GetFHelpButtonRect: TRect;
begin
 result:=Self.FHelpButtonRect;
end;

function TFormStyleHookHelper.GetFHotButton: Integer;
begin
Result:=Self.FHotButton;
end;

function TFormStyleHookHelper.GetFMaxButtonRect: TRect;
begin
 result:=Self.FMaxButtonRect;
end;

function TFormStyleHookHelper.GetFMinButtonRect: TRect;
begin
 result:=Self.FMinButtonRect;
end;

function TFormStyleHookHelper.GetFPressedButton: Integer;
begin
 Result:=Self.FPressedButton;
end;

function TFormStyleHookHelper.GetFSysMenuButtonRect: TRect;
begin
 result:=Self.FSysMenuButtonRect;
end;

function TFormStyleHookHelper.GetFWidth: Integer;
begin
 Result:=Self.FWidth;
end;

procedure TFormStyleHookHelper.MainMenuBarHookPaint(Canvas: TCanvas);
begin
  if Self.FMainMenuBarHook<>nil then
   Self.FMainMenuBarHook.Paint(Canvas);
end;

procedure TFormStyleHookHelper.SetFCaptionRect(const Value: TRect);
begin
 Self.FCaptionRect:=value;
end;

procedure TFormStyleHookHelper.SetFCloseButtonRect(const Value: TRect);
begin
 Self.FCloseButtonRect:=Value;
end;

procedure TFormStyleHookHelper.SetFHelpButtonRect(const Value: TRect);
begin
 Self.FHelpButtonRect:=Value;
end;

procedure TFormStyleHookHelper.SetFMaxButtonRect(const Value: TRect);
begin
 Self.FMaxButtonRect:=Value;
end;

procedure TFormStyleHookHelper.SetFMinButtonRect(const Value: TRect);
begin
 Self.FMinButtonRect:=Value;
end;

procedure TFormStyleHookHelper.SetFSysMenuButtonRect(const Value: TRect);
begin
 Self.FSysMenuButtonRect:=Value;
end;

function TFormStyleHookHelper._GetBorderSize: TRect;
begin
  Result:=Self.GetBorderSize;
end;

function TFormStyleHookHelper._GetIconFast: TIcon;
begin
  Result:=Self.GetIconFast;
end;

function RectVCenter(var R: TRect; Bounds: TRect): TRect;
begin
  OffsetRect(R, -R.Left, -R.Top);
  OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
  OffsetRect(R, Bounds.Left, Bounds.Top);
  Result := R;
end;

{ TFormStyleHookBackround.TSettings }

constructor TFormStyleHookBackround.TSettings.Create;
begin
  inherited;
  FEnabled:=False;
  FBitmap:=TBitmap.Create;
  ImageLocation:='';
  UseImage:=False;
end;

destructor TFormStyleHookBackround.TSettings.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TFormStyleHookBackround.TSettings.SetColor(const Value: TColor);
begin
  if Value<>FColor then
  FColor := Value;
end;

procedure TFormStyleHookBackround.TSettings.SetImageLocation(const Value: string);
var
  Picture: TPicture;
begin
  FImageLocation := Value;
  if FileExists(Value) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(Value);
      FBitmap.Width := Picture.Width;
      FBitmap.Height := Picture.Height;
      FBitmap.Canvas.Draw(0, 0, Picture.Graphic);
    finally
      Picture.Free;
    end;
  end;
end;


procedure TFormStyleHookBackround.TSettings.SetUseColor(const Value: Boolean);
begin
  FUseColor := Value;
  FUseImage := not Value;
end;

procedure TFormStyleHookBackround.TSettings.SetUseImage(const Value: Boolean);
begin
  FUseImage := Value;
  FUseColor := not Value;
end;

{ TFormStyleHookBackround }

class constructor TFormStyleHookBackround.Create;
begin
   FMergeImages:=False;
   FNCSettings:=TFormStyleHookBackround.TSettings.Create;
   FBackGroundSettings:=TFormStyleHookBackround.TSettings.Create;
end;

class destructor TFormStyleHookBackround.Destroy;
begin
  FreeAndNil(FNCSettings);
  FreeAndNil(FBackGroundSettings);
end;


procedure TFormStyleHookBackround.PaintBackground(Canvas: TCanvas);
var
  LRect   : TRect;
  RBitmap : TRect;
  L,H     : Integer;
begin
  //if the option is not enabled use the default inherited PaintBackground method
  if not BackGroundSettings.Enabled then
   inherited
  else
  begin
    //get he bounds of the control (form)
    LRect := Rect(0, 0, Control.ClientWidth, Control.ClientHeight);
    //use a custom color for the background?
    if  BackGroundSettings.UseColor then
    begin
     Canvas.Brush.Color:=BackGroundSettings.Color;
     Canvas.FillRect(LRect);
    end
    else
    //use a bitmap
    begin
      //check the size of the bitmap against the control bounds to detrine how the bitmap is drawn
      if (BackGroundSettings.Bitmap.Width<LRect.Width) or (BackGroundSettings.Bitmap.Height<LRect.Height) then
      begin
       Canvas.Brush.Bitmap := BackGroundSettings.BitMap;
       Canvas.FillRect(LRect);
      end
      else
      begin
       //check if the the background bitmap must be merged with non client area bitmap
       if not FMergeImages then
        Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,LRect)
       else
       begin
        RBitmap:=LRect;
        H:=_GetBorderSize.Top;
        L:=_GetBorderSize.Left;
        RBitmap.SetLocation(L, H);
        Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,RBitmap);
       end;
      end;
    end;
  end;
end;

procedure TFormStyleHookBackround.PaintNC(Canvas: TCanvas);
var
  LDetail: TThemedWindow;
  LDetails,
  CaptionDetails,
  IconDetails   : TThemedElementDetails;
  R, R1, DrawRect, ButtonRect, TextRect: TRect;
  CaptionBuffer: TBitmap;
  FButtonState: TThemedWindow;
  TextFormat: TTextFormat;
  LText: string;
  SrcBackRect     : TRect;
begin
  //if the setting is not enabled use the original PaintNC method
  if not NCSettings.Enabled then
  begin
   inherited ;
   exit;
  end;

  //check the border style of the form
  if Form.BorderStyle = bsNone then
  begin
    MainMenuBarHookPaint(Canvas);    Exit;
  end;


  {init some parameters}
  _FCloseButtonRect := Rect(0, 0, 0, 0);
  _FMaxButtonRect := Rect(0, 0, 0, 0);
  _FMinButtonRect := Rect(0, 0, 0, 0);
  _FHelpButtonRect := Rect(0, 0, 0, 0);
  _FSysMenuButtonRect := Rect(0, 0, 0, 0);
  _FCaptionRect := Rect(0, 0, 0, 0);

  if not StyleServices.Available then
    Exit;
  R := _GetBorderSize;

  {draw caption}

  if (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if _FFormActive then
      LDetail := twCaptionActive
    else
      LDetail := twCaptionInActive
  end
  else
  begin
   if _FFormActive then
      LDetail := twSmallCaptionActive
    else
      LDetail := twSmallCaptionInActive
  end;
  CaptionBuffer := TBitmap.Create;
  CaptionBuffer.SetSize(_FWidth, R.Top);

  {draw caption border}
  DrawRect := Rect(0, 0, CaptionBuffer.Width, CaptionBuffer.Height);
  LDetails := StyleServices.GetElementDetails(LDetail);  //used for draw text in the caption

  //check if a must use a custom color or a bitmap
  if FNCSettings.UseColor then
  begin
    //use the select color to fill the background of the canvas
    CaptionBuffer.Canvas.Brush.Color:=FNCSettings.Color;
    CaptionBuffer.Canvas.FillRect(DrawRect);
  end
  else
  begin
    //use the bitmap to fill the canvas
    SrcBackRect.Left:=0;
    SrcBackRect.Top:=0;
    SrcBackRect.Width:=DrawRect.Width;
    SrcBackRect.Height:=DrawRect.Height;
    //SrcBackRect.SetLocation(FNCSettings.Bitmap.Width-DrawRect.Width, 0);
    //SrcBackRect.SetLocation(_GetBorderSize.Width, 0);
    CaptionBuffer.Canvas.CopyRect(DrawRect, FNCSettings.Bitmap.Canvas,SrcBackRect);
  end;

  TextRect := DrawRect;
  CaptionDetails := LDetails;

  {draw icon}
  if (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    IconDetails := StyleServices.GetElementDetails(twSysButtonNormal);
    if not StyleServices.GetElementContentRect(0, IconDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    R1 := Rect(0, 0, GetSystemMetrics(SM_CXSMICON), GetSystemMetrics(SM_CYSMICON));
    RectVCenter(R1, ButtonRect);
    if ButtonRect.Width > 0 then
      DrawIconEx(CaptionBuffer.Canvas.Handle, R1.Left, R1.Top, _GetIconFast.Handle, 0, 0, 0, 0, DI_NORMAL);
    Inc(TextRect.Left, ButtonRect.Width + 5);
    _FSysMenuButtonRect := ButtonRect;
  end
  else
    Inc(TextRect.Left, R.Left);

  {draw buttons}
  if (biSystemMenu in TCustomFormHack(Form).BorderIcons) then
  begin
    if (Form.BorderStyle <> bsToolWindow) and
       (Form.BorderStyle <> bsSizeToolWin) then
    begin
      if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
        FButtonState := twCloseButtonPushed
      else if _FHotButton = HTCLOSE then
        FButtonState := twCloseButtonHot
      else
        if _FFormActive then
          FButtonState := twCloseButtonNormal
        else
          FButtonState := twCloseButtonDisabled;
     end
    else
    begin
      if (_FPressedButton = HTCLOSE) and (_FHotButton = HTCLOSE) then
        FButtonState := twSmallCloseButtonPushed
      else if _FHotButton = HTCLOSE then
        FButtonState := twSmallCloseButtonHot
      else
        if _FFormActive then
          FButtonState := twSmallCloseButtonNormal
        else
          FButtonState := twSmallCloseButtonDisabled;
    end;

    LDetails := StyleServices.GetElementDetails(FButtonState);
    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);

    StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FCloseButtonRect := ButtonRect;
  end;

  if (biMaximize in TCustomFormHack(Form).BorderIcons) and
     (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if Form.WindowState = wsMaximized then
    begin
      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twRestoreButtonPushed
      else if _FHotButton = HTMAXBUTTON then
        FButtonState := twRestoreButtonHot
      else
      if _FFormActive then
        FButtonState := twRestoreButtonNormal
      else
        FButtonState := twRestoreButtonDisabled;
    end
    else
    begin
      if (_FPressedButton = HTMAXBUTTON) and (_FHotButton = HTMAXBUTTON) then
        FButtonState := twMaxButtonPushed
      else if _FHotButton = HTMAXBUTTON then
        FButtonState := twMaxButtonHot
      else
      if _FFormActive then
        FButtonState := twMaxButtonNormal
      else
        FButtonState := twMaxButtonDisabled;
    end;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FMaxButtonRect := ButtonRect;
  end;

  if (biMinimize in TCustomFormHack(Form).BorderIcons) and
     (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     (Form.BorderStyle <> bsDialog) and
     (Form.BorderStyle <> bsToolWindow) and
     (Form.BorderStyle <> bsSizeToolWin) then
  begin
    if (_FPressedButton = HTMINBUTTON) and (_FHotButton = HTMINBUTTON) then
      FButtonState := twMinButtonPushed
    else if _FHotButton = HTMINBUTTON then
      FButtonState := twMinButtonHot
    else
      if _FFormActive then
        FButtonState := twMinButtonNormal
      else
        FButtonState := twMinButtonDisabled;

    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);
    if ButtonRect.Left > 0 then TextRect.Right := ButtonRect.Left;
    _FMinButtonRect := ButtonRect;
  end;

  if (biHelp in TCustomFormHack(Form).BorderIcons) and (biSystemMenu in TCustomFormHack(Form).BorderIcons) and
     ((not (biMaximize in TCustomFormHack(Form).BorderIcons) and
     not (biMinimize in TCustomFormHack(Form).BorderIcons)) or (Form.BorderStyle = bsDialog))
  then
  begin
    if (_FPressedButton = HTHELP) and (_FHotButton = HTHELP) then
      FButtonState := twHelpButtonPushed
    else if _FHotButton = HTHELP then
      FButtonState := twHelpButtonHot
    else
    if _FFormActive then
      FButtonState := twHelpButtonNormal
    else
      FButtonState := twHelpButtonDisabled;
    LDetails := StyleServices.GetElementDetails(FButtonState);

    if not StyleServices.GetElementContentRect(0, LDetails, DrawRect, ButtonRect) then
      ButtonRect := Rect(0, 0, 0, 0);
    if ButtonRect.Width > 0 then
      StyleServices.DrawElement(CaptionBuffer.Canvas.Handle, LDetails, ButtonRect);

    if ButtonRect.Left > 0 then
      TextRect.Right := ButtonRect.Left;
    _FHelpButtonRect := ButtonRect;
  end;

  {draw text}
  TextFormat := [tfLeft, tfSingleLine, tfVerticalCenter];
  if Control.UseRightToLeftReading then
    Include(TextFormat, tfRtlReading);

  LText := Text;
  StyleServices.DrawText(CaptionBuffer.Canvas.Handle, CaptionDetails, LText, TextRect, TextFormat);
  _FCaptionRect := TextRect;

  {draw caption buffer}

  Canvas.Draw(0, 0, CaptionBuffer);
  CaptionBuffer.Free;

  {draw menubar}
  MainMenuBarHookPaint(Canvas);

  {draw left border}
  DrawRect := Rect(0, R.Top, R.Left, _FHeight - R.Bottom);
  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (DrawRect.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.BitMap);
    end;

  {draw right border}
  DrawRect := Rect(_FWidth - R.Right, R.Top, _FWidth, _FHeight - R.Bottom);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (Control.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
        Canvas.StretchDraw(DrawRect, FNCSettings.BitMap);
    end;

  {draw Bottom border}
  DrawRect := Rect(0, _FHeight - R.Bottom, _FWidth, _FHeight);

  if DrawRect.Bottom - DrawRect.Top > 0 then
    //use a color?
    if FNCSettings.UseColor then
    begin
      Canvas.Brush.Color:=FNCSettings.Color;
      Canvas.FillRect(DrawRect);
    end
    else
    begin
      if (DrawRect.Height<=FNCSettings.BitMap.Height) and (Control.Width<=FNCSettings.BitMap.Width)  then
        Canvas.CopyRect(DrawRect,FNCSettings.Bitmap.Canvas,DrawRect)
      else
      begin
        SrcBackRect.Left:=0;
        SrcBackRect.Top:=0;
        SrcBackRect.Width:=DrawRect.Width;
        SrcBackRect.Height:=DrawRect.Height;
        SrcBackRect.SetLocation(FNCSettings.BitMap.Width-DrawRect.Width, 0);
        Canvas.CopyRect(DrawRect, FNCSettings.BitMap.Canvas,SrcBackRect);
      end;
    end;
end;

end.
