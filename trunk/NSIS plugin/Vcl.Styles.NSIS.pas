// **************************************************************************************************
//
// unit Vcl.Styles.NSIS
// http://code.google.com/p/vcl-styles-utils/
//
// The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License");
// you may not use this file except in compliance with the License. You may obtain a copy of the
// License at http://www.mozilla.org/MPL/
//
// Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF
// ANY KIND, either express or implied. See the License for the specific language governing rights
// and limitations under the License.
//
// The Original Code is  NSISVCLStyles.dpr
//
// The Initial Developer of the Original Code is Rodrigo Ruz V.
//
// Portions created by Rodrigo Ruz V. are Copyright (C) 2013-2014 Rodrigo Ruz V.
//
// All Rights Reserved.
//
// **************************************************************************************************

unit Vcl.Styles.NSIS;

interface

uses
  System.Types,
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  WinApi.Windows,
  WinApi.Messages,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.StdCtrls,
  Vcl.Graphics,
  Vcl.Styles.Utils.SysStyleHook,
  Vcl.Styles.Utils.Forms,
  Vcl.Styles.Utils.StdCtrls;

type
  TTransparentStaticNSIS = class(TSysStaticStyleHook)
  private
  protected
    procedure Paint(Canvas: TCanvas); override;
    procedure WndProc(var Message: TMessage); override;
    procedure PaintBackground(Canvas: TCanvas); override;
  public
    constructor Create(AHandle: THandle); override;
    Destructor Destroy; override;
  end;

  /// <summary> Dialog Style hook to add image and/or color support for the background and non client area
  /// </summary>
  TSysDialogStyleHookBackground = class(TSysDialogStyleHook)
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
      FUseAlpha: Boolean;
      FAlphaValue: Byte;
      procedure SetColor(const Value: TColor);
      procedure SetImageLocation(const Value: string);
      procedure SetUseColor(const Value: Boolean);
      procedure SetUseImage(const Value: Boolean);
    public
      property UseImage: Boolean read FUseImage write SetUseImage;
      property UseColor: Boolean read FUseColor write SetUseColor;
      property Color: TColor read FColor write SetColor;
      property ImageLocation: string read FImageLocation write SetImageLocation;
      property Bitmap: TBitmap read FBitmap;
      property Enabled: Boolean read FEnabled write FEnabled;
      property UseAlpha: Boolean read FUseAlpha write FUseAlpha;
      property AlphaValue: Byte read FAlphaValue write FAlphaValue;
      constructor Create;
      destructor Destroy; override;
    end;
    class var FNCSettings: TSettings;
    class var FBackGroundSettings: TSettings;
    class var FMergeImages: Boolean;
    class Var FSharedBitMap: TBitmap;
    class var FSharedImageLocation: string;
    class procedure SetSharedImageLocation(const Value: string); static;
  protected
    procedure PaintNC(Canvas: TCanvas); override;
    procedure PaintBackground(Canvas: TCanvas); override;
    class constructor Create;
    class destructor Destroy;
  public
    constructor Create(AHandle: THandle); override;
    class property SharedImageLocation: string read FSharedImageLocation
      write SetSharedImageLocation;
    class property SharedBitMap: TBitmap read FSharedBitMap write FSharedBitMap;
    class property MergeImages: Boolean read FMergeImages write FMergeImages;
    class property NCSettings: TSettings read FNCSettings;
    class property BackGroundSettings: TSettings read FBackGroundSettings;
  end;

  TSysDialogStyleHookNC = class(TSysDialogStyleHook)
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AHandle: THandle); override;
  end;

var
  NSIS_IgnoredControls: TList<HWND>;

implementation

uses
//  IOUTILS,
  Vcl.Styles.Utils.SysControls;

//procedure Addlog(const Msg: string);
//begin
//  TFile.AppendAllText('C:\Test\log.txt',
//    Format('%s %s %s', [FormatDateTime('hh:nn:ss.zzz', Now), Msg, sLineBreak]));
//end;

{ TTransparentStaticNSIS }

constructor TTransparentStaticNSIS.Create(AHandle: THandle);
begin
  inherited;
  OverrideEraseBkgnd := True;
  OverrideFont := False;
  // if (SysControl.ExStyle and WS_EX_TRANSPARENT <> WS_EX_TRANSPARENT) then
  // SysControl.ExStyle  := SysControl.ExStyle or WS_EX_TRANSPARENT;

end;

destructor TTransparentStaticNSIS.Destroy;
begin

  inherited;
end;

procedure TTransparentStaticNSIS.Paint(Canvas: TCanvas);
const
  Alignments: array [TAlignment] of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  States: array [Boolean] of TThemedTextLabel = (ttlTextLabelDisabled,
    ttlTextLabelNormal);
var
  LDetails: TThemedElementDetails;
  LRect: TRect;
begin
  LRect := SysControl.ClientRect;

  LDetails := StyleServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  StyleServices.DrawParentBackground(Handle, Canvas.Handle, LDetails, False);
  Canvas.Brush.Style := bsClear;

  LDetails := StyleServices.GetElementDetails(States[SysControl.Enabled]);
  DrawText(Canvas.Handle, LDetails, SysControl.Text, LRect, TextFormat);
end;

procedure TTransparentStaticNSIS.PaintBackground(Canvas: TCanvas);
//var
//  Details: TThemedElementDetails;
begin
  // if StyleServices.Available then
  // begin
  // Details.Element := teButton;
  // if StyleServices.HasTransparentParts(Details) then
  // StyleServices.DrawParentBackground(Handle, Canvas.Handle, Details, False);
  // end;
end;

procedure TTransparentStaticNSIS.WndProc(var Message: TMessage);
begin
  inherited;
  case Message.Msg of
    CM_CTLCOLORSTATIC:
      begin
        // SetTextColor(Message.wParam, ColorToRGB(FontColor));
        SetBkMode(Message.wParam, TRANSPARENT);
        // StyleServices.DrawParentBackground(Handle, Message.wParam, nil, False);
        Message.Result := GetStockObject(NULL_BRUSH);
        Exit;
      end;
  else
    inherited;
  end;
end;

{ TSysDialogStyleHookBackground }

class constructor TSysDialogStyleHookBackground.Create;
begin
  FMergeImages := False;
  FSharedBitMap := TBitmap.Create;
  FNCSettings := TSysDialogStyleHookBackground.TSettings.Create;
  FBackGroundSettings := TSysDialogStyleHookBackground.TSettings.Create;
end;

constructor TSysDialogStyleHookBackground.Create(AHandle: THandle);
begin
  inherited;
end;

class destructor TSysDialogStyleHookBackground.Destroy;
begin
  FreeAndNil(FSharedBitMap);
  FreeAndNil(FNCSettings);
  FreeAndNil(FBackGroundSettings);
end;

procedure TSysDialogStyleHookBackground.PaintBackground(Canvas: TCanvas);
var
  LRect: TRect;
  RBitmap: TRect;
  L, H: Integer;
begin
  // if the option is not enabled use the default inherited PaintBackground method
  if not BackGroundSettings.Enabled then
    inherited
  else
  begin
    // Addlog('pass');
    // get he bounds of the control (form)
    LRect := Rect(0, 0, SysControl.ClientWidth, SysControl.ClientHeight);
    // use a custom color for the background?
    if BackGroundSettings.UseColor then
    begin
      Canvas.Brush.Color := BackGroundSettings.Color;
      Canvas.FillRect(LRect);
    end
    else
    // use a bitmap
    begin
      // check the size of the bitmap against the control bounds to detrine how the bitmap is drawn
      if not FMergeImages and ((BackGroundSettings.Bitmap.Width < LRect.Width)
        or (BackGroundSettings.Bitmap.Height < LRect.Height)) then
      begin
        Canvas.Brush.Bitmap := BackGroundSettings.Bitmap;
        Canvas.FillRect(LRect);
      end
      else
      begin
        // check if the the background bitmap must be merged with non client area bitmap
        if not FMergeImages then
          Canvas.CopyRect(LRect, BackGroundSettings.Bitmap.Canvas, LRect)
        else
        begin
          RBitmap := LRect;
          H := GetBorderSize.Top;
          L := GetBorderSize.Left;
          RBitmap.SetLocation(L, H);
          // Canvas.CopyRect(LRect,BackGroundSettings.Bitmap.Canvas,RBitmap);
          Canvas.CopyRect(LRect, FSharedBitMap.Canvas, RBitmap);
        end;
      end;
    end;
  end;
end;

procedure TSysDialogStyleHookBackground.PaintNC(Canvas: TCanvas);
begin
  inherited
end;

class procedure TSysDialogStyleHookBackground.SetSharedImageLocation
  (const Value: string);
var
  Picture: TPicture;
begin
  FSharedImageLocation := Value;
  if FileExists(Value) then
  begin
    Picture := TPicture.Create;
    try
      Picture.LoadFromFile(Value);
      FSharedBitMap.Width := Picture.Width;
      FSharedBitMap.Height := Picture.Height;
      FSharedBitMap.Canvas.Draw(0, 0, Picture.Graphic);
    finally
      Picture.Free;
    end;
  end;
end;

{ TSysDialogStyleHookBackground.TSettings }

constructor TSysDialogStyleHookBackground.TSettings.Create;
begin
  inherited;
  FUseAlpha := False;
  FAlphaValue := 200;
  FEnabled := False;
  FBitmap := TBitmap.Create;
  ImageLocation := '';
  UseImage := False;
end;

destructor TSysDialogStyleHookBackground.TSettings.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TSysDialogStyleHookBackground.TSettings.SetColor(const Value: TColor);
begin
  if Value <> FColor then
    FColor := Value;
end;

procedure TSysDialogStyleHookBackground.TSettings.SetImageLocation
  (const Value: string);
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

procedure TSysDialogStyleHookBackground.TSettings.SetUseColor
  (const Value: Boolean);
begin
  FUseColor := Value;
  FUseImage := not Value;
end;

procedure TSysDialogStyleHookBackground.TSettings.SetUseImage
  (const Value: Boolean);
begin
  FUseImage := Value;
  FUseColor := not Value;
end;

{ TSysDialogStyleHookNC }

constructor TSysDialogStyleHookNC.Create(AHandle: THandle);
begin
  inherited;
  OverridePaintNC := False;
end;

procedure TSysDialogStyleHookNC.WndProc(var Message: TMessage);
begin
  inherited;
end;

function BeforeNSISHookingControl(Info: PControlInfo): Boolean;
var
  LInfo: TControlInfo;
begin
  //Addlog('BeforeNSISHookingControl '+IntToHex(LInfo.Handle, 8));
  LInfo := Info^;
  Result := NSIS_IgnoredControls.IndexOf(LInfo.Handle) < 0;
//  if not Result then
//    Addlog(IntToHex(LInfo.Handle, 8));
end;

procedure HookNotificationNSIS(Action: TSysHookAction; Info: PControlInfo);
var
  LInfo: TControlInfo;
begin
  LInfo := Info^;
  if Action = cRemoved then
    if NSIS_IgnoredControls.IndexOf(LInfo.Handle) >= 0 then
      NSIS_IgnoredControls.Remove(LInfo.Handle);
end;

initialization

NSIS_IgnoredControls := TList<HWND>.Create;
TSysStyleManager.OnBeforeHookingControl := @BeforeNSISHookingControl;
TSysStyleManager.OnHookNotification := @HookNotificationNSIS;

finalization

NSIS_IgnoredControls.Free;

end.
