{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Ext                                                                              }
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
{ The Original Code is Vcl.Styles.Ext.pas.                                                         }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Ext;

interface

{$DEFINE USE_VCL_STYLESAPI}

Uses
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Forms,
  Generics.Collections,
  Winapi.Windows,
  Vcl.Graphics,
  System.Classes;

type
  TStyleHookList = TList<TStyleHookClass>;


  TStyleServicesHandle = type Pointer;
  TSourceInfo = record
    Data: TStyleServicesHandle;
    StyleClass: TCustomStyleServicesClass;
  end;

  {$REGION 'Documentation'}
  ///	<summary>Heper for the TStyleManager class
  ///	</summary>
  {$ENDREGION}
  TStyleManagerHelper = Class Helper for TStyleManager
  strict private
   class function GetStyleSourceInfo(const StyleName: string): TSourceInfo; static;
   class function GetStyles: TList<TCustomStyleServices>;
   class function _GetStyles: TList<TCustomStyleServices>; static;
  public
   class function GetRegisteredStyles: TDictionary<string, TSourceInfo>;
   {$REGION 'Documentation'}
   ///	<summary>Get the TSourceInfo for a Style
   ///	</summary>
   {$ENDREGION}
   class property StyleSourceInfo[const StyleName: string]: TSourceInfo read GetStyleSourceInfo;
   {$REGION 'Documentation'}
   ///	<summary>Send the CM_CUSTOMSTYLECHANGED message to all the forms
   ///	</summary>
   {$ENDREGION}
   class procedure RefreshCurrentTheme;
   {$REGION 'Documentation'}
   ///	<summary>Return the loaded styles (TCustomStyleServices) in the system
   ///	</summary>
   {$ENDREGION}
   class property Styles: TList<TCustomStyleServices> read _GetStyles;
   {$REGION 'Documentation'}
   ///	<summary>Force to reload a modified vcl style
   ///	</summary>
   {$ENDREGION}
   class procedure ReloadStyle(const Name: string);overload;
   end;


procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string);


{$IFDEF USE_VCL_STYLESAPI}
type
  TCustomStyleExt = class(TCustomStyle)
  strict private
    FStream    : TStream;
  public
    function  GetStyleInfo : TStyleInfo;
  private
    function GetBitmapList: TObjectList<TBitmap>;
    procedure SetStyleInfo(const Value: TStyleInfo);
  public
    {$REGION 'Documentation'}
    ///	<summary>Create a  TCustomStyleExt using a vcl style stored in a file
    ///	</summary>
    {$ENDREGION}
    constructor Create(const FileName :string);overload;
    {$REGION 'Documentation'}
    ///	<summary>Create a  TCustomStyleExt using a vcl style stored in a stream
    ///	</summary>
    {$ENDREGION}
    constructor Create(const Stream:TStream);overload;
    constructor Create(const Style:TCustomStyle);overload;
    destructor Destroy;override;
    {$REGION 'Documentation'}
    ///	<summary>Replace a internal bitmap of the Style
    ///	</summary>
    {$ENDREGION}
    procedure ReplaceBitmap(DestIndex : Integer;Src: TBitmap);
    {$REGION 'Documentation'}
    ///	<summary>Set a returns the TStyleInfo fo the current style
    ///	</summary>
    {$ENDREGION}
    property StyleInfo : TStyleInfo read GetStyleInfo write SetStyleInfo;
    {$REGION 'Documentation'}
    ///	<summary>Return the list of the bitmaps of the style
    ///	</summary>
    {$ENDREGION}
    property BitmapList: TObjectList<TBitmap> read GetBitmapList;
    property LocalStream : TStream read FStream;
    {$REGION 'Documentation'}
    ///	<summary>Copy the modified style to an Stream
    ///	</summary>
    {$ENDREGION}
    procedure CopyToStream(Stream : TStream);
  end;

{$ENDIF}

implementation


uses
{$IFDEF USE_VCL_STYLESAPI}
 System.ZLib,
 System.UITypes,
 Vcl.StdCtrls,
 Vcl.ImgList,
 Vcl.Consts,
 Vcl.GraphUtil,
 Vcl.Imaging.pngimage,
 Winapi.Messages,
{$ENDIF}
 Vcl.Controls,
 System.Sysutils;

{$IFDEF USE_VCL_STYLESAPI}
{$I 'C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\source\vcl\StyleUtils.inc'}
{$I 'C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\source\vcl\StyleAPI.inc'}
{$ENDIF}


type
  TCustomStyleHelper = Class Helper for TCustomStyle
  private
    function GetSource: TObject;
  public
    property Source: TObject read GetSource;
  End;

  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  END;

class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  Result:= Self.FRegisteredStyleHooks;
end;

function TCustomStyleHelper.GetSource: TObject;
begin
  Result:=Self.FSource;
end;

{ TStyleManagerHelper }



class function TStyleManagerHelper.GetRegisteredStyles: TDictionary<string, TSourceInfo>;
var
  t            : TPair<string, TStyleManager.TSourceInfo>;
  SourceInfo   : TSourceInfo;
begin
 Result:=TDictionary<string, TSourceInfo>.Create;
  for t in Self.FRegisteredStyles do
  begin
   SourceInfo.Data:=t.Value.Data;
   SourceInfo.StyleClass:=t.Value.StyleClass;
   Result.Add(t.Key,SourceInfo);
  end;
end;


class function TStyleManagerHelper.GetStyles: TList<TCustomStyleServices>;
begin
  Result:=Self.FStyles;
end;

class function TStyleManagerHelper.GetStyleSourceInfo(const StyleName: string): TSourceInfo;
Var
 LRegisteredStyles : TDictionary<string, TSourceInfo>;
begin
  LRegisteredStyles:=TStyleManager.GetRegisteredStyles;
  try
    if LRegisteredStyles.ContainsKey(StyleName) then
      Result:=LRegisteredStyles[StyleName];
  finally
     LRegisteredStyles.Free;
  end;
end;

class procedure TStyleManagerHelper.RefreshCurrentTheme;
var
  I: Integer;
begin
  for I := 0 to Screen.FormCount - 1 do
    if Screen.Forms[I].HandleAllocated then
      if IsWindowVisible(Screen.Forms[I].Handle) then
        PostMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0)
      else
        SendMessage(Screen.Forms[I].Handle, CM_CUSTOMSTYLECHANGED, 0, 0);
end;


class procedure TStyleManagerHelper.ReloadStyle(const Name: string);
var
  LStyle: TCustomStyleServices;
begin

 if SameText(Name, ActiveStyle.Name, loUserLocale) then
   SetStyle(SystemStyle);

 for LStyle in Styles do
  if SameText(Name, LStyle.Name, loUserLocale) then
  begin
    LStyle.Free;
    Styles.Remove(LStyle);
  end;

 SetStyle(Name);
end;

class function TStyleManagerHelper._GetStyles: TList<TCustomStyleServices>;
begin
  Result:=TStyleManager.GetStyles;
end;

function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
begin
 Result:=nil;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
      Result:=TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
end;

function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
var
  List    : TStyleHookList;
begin
 Result:=False;
    if TCustomStyleEngine.GetRegisteredStyleHooks.ContainsKey(ControlClass) then
    begin
      List := TCustomStyleEngine.GetRegisteredStyleHooks[ControlClass];
      Result:=List.IndexOf(StyleHookClass) <> -1;
    end;
end;

Procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
begin
   if not IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.RegisterStyleHook(ControlClass, TStyleHook);
end;

Procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
begin
   if IsStyleHookRegistered(ControlClass, TStyleHook) then
    TStyleManager.Engine.UnRegisterStyleHook(ControlClass, TStyleHook);
end;


{$IFDEF USE_VCL_STYLESAPI}
{ TVCLStyleExt }

constructor TCustomStyleExt.Create(const FileName: string);
var
  LStream: TFileStream;
begin
  LStream := TFileStream.Create(FileName, fmOpenRead);
  try
    Create(LStream);
  finally
    LStream.Free;
  end;
end;

procedure TCustomStyleExt.CopyToStream(Stream: TStream);
var
 I :  Integer;
begin
  Stream.Size:=0;
  Stream.Position:=0;


   TseStyle(Source).FCleanCopy.Name        :=  TseStyle(Source).StyleSource.Name;
   TseStyle(Source).FCleanCopy.Author      :=  TseStyle(Source).StyleSource.Author;
   TseStyle(Source).FCleanCopy.AuthorEMail :=  TseStyle(Source).StyleSource.AuthorEMail;
   TseStyle(Source).FCleanCopy.AuthorURL   :=  TseStyle(Source).StyleSource.AuthorURL;
   TseStyle(Source).FCleanCopy.Version     :=  TseStyle(Source).StyleSource.Version;


  //Replace the updated bitmaps
  for i := 0 to TseStyle(Source).FCleanCopy.Bitmaps.Count-1  do
   TseStyle(Source).FCleanCopy.Bitmaps[i].Assign(TseStyle(Source).StyleSource.Bitmaps[i]);

  TseStyle(Source).SaveToStream(Stream);
  {
  TseStyle(Source).StyleSource.Fonts.Assign(TseStyle(Source).Fonts);
  TseStyle(Source).StyleSource.Colors.Assign(TseStyle(Source).Colors);
  TseStyle(Source).StyleSource.SysColors.Assign(TseStyle(Source).SysColors);
  TseStyle(Source).StyleSource.SaveToStream(Stream);
  }
end;

constructor TCustomStyleExt.Create(const Style: TCustomStyle);
begin
  //Style.Source
  //inherited Create(TStream(Style.));

end;

constructor TCustomStyleExt.Create(const Stream: TStream);
begin
  inherited Create;
  FStream:=TMemoryStream.Create;

  Stream.Seek(0, soBeginning); //index 0 to load
  FStream.CopyFrom(Stream, Stream.Size);
  Stream.Seek(0, soBeginning); //restore index 0 after


  FStream.Seek(0, soBeginning);//index 0 to load
  TseStyle(Source).LoadFromStream(FStream);
end;



destructor TCustomStyleExt.Destroy;
begin
  if Assigned(FStream) then
    FStream.Free;
  inherited Destroy;
end;

function TCustomStyleExt.GetBitmapList: TObjectList<TBitmap>;
var
  I: Integer;
begin
  Result:=TObjectList<TBitmap>.Create;
  for I:=0 to TseStyle(Source).StyleSource.Bitmaps.Count-1 do
  begin
    Result.Add(TBitmap.Create);
    Result[I].PixelFormat:=pf32bit;
    Result[I].Width := TseStyle(Source).StyleSource.Bitmaps[I].Width;
    Result[I].Height:= TseStyle(Source).StyleSource.Bitmaps[I].Height;
    TseStyle(Source).StyleSource.Bitmaps[I].Draw(Result[I].Canvas,0,0);
  end;

//  TseStyle(Source).StyleSource.Colors
end;

procedure TCustomStyleExt.ReplaceBitmap(DestIndex: Integer; Src: TBitmap);
var
  BF          : TBlendFunction;
  Canvas      : TCanvas;
  BitMap      : TseBitmap;
  DstRect, SrcRect: TRect;
begin
  BitMap:=TseStyle(Source).StyleSource.Bitmaps[DestIndex];
  SrcRect:=Rect(0 ,0, Src.Width, Src.Height);
  DstRect:=Rect(0 ,0, Src.Width, Src.Height);

  Canvas:= BitMap.Canvas;
  SetStretchBltMode(Canvas.Handle, COLORONCOLOR);
  if BitMap.AlphaBlend then
  begin
    BF.BlendOp := AC_SRC_OVER;
    BF.BlendFlags := 0;
    BF.SourceConstantAlpha := 255;
    BF.AlphaFormat := AC_SRC_ALPHA;
    Winapi.Windows.AlphaBlend(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, BF);
  end
  else
  if BitMap.Transparent then
  begin
    Winapi.Windows.TransparentBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, seTransparent);
  end
  else
  begin
    Winapi.Windows.StretchBlt(Canvas.Handle, DstRect.Left, DstRect.Top, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top,
      Src.Canvas.Handle, SrcRect.Left, SrcRect.Top, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top, SRCCOPY);
  end;
end;

procedure TCustomStyleExt.SetStyleInfo(const Value: TStyleInfo);
begin
 TseStyle(Source).StyleSource.Name:=Value.Name;
 TseStyle(Source).StyleSource.Author:=Value.Author;
 TseStyle(Source).StyleSource.AuthorEMail:=Value.AuthorEMail;
 TseStyle(Source).StyleSource.AuthorURL:=Value.AuthorURL;
 TseStyle(Source).StyleSource.Version:=Value.Version;
end;

function TCustomStyleExt.GetStyleInfo: TStyleInfo;
begin
 Result.Name        :=  TseStyle(Source).StyleSource.Name;
 Result.Author      :=  TseStyle(Source).StyleSource.Author;
 Result.AuthorEMail :=  TseStyle(Source).StyleSource.AuthorEMail;
 Result.AuthorURL   :=  TseStyle(Source).StyleSource.AuthorURL;
 Result.Version     :=  TseStyle(Source).StyleSource.Version;
end;


{$ENDIF}

procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string);
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
  ThemeTextColor  : TColor;

    function GetBorderSize: TRect;
    var
      Size: TSize;
      Details: TThemedElementDetails;
      Detail: TThemedWindow;
    begin
      Result  := Rect(0, 0, 0, 0);
      Detail  := twCaptionActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Top := Size.cy;
      Detail := twFrameLeftActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Left := Size.cx;
      Detail := twFrameRightActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Right := Size.cx;
      Detail := twFrameBottomActive;
      Details := Style.GetElementDetails(Detail);
      Style.GetElementSize(0, Details, esActual, Size);
      Result.Bottom := Size.cy;
    end;

    function RectVCenter(var R: TRect; Bounds: TRect): TRect;
    begin
      OffsetRect(R, -R.Left, -R.Top);
      OffsetRect(R, 0, (Bounds.Height - R.Height) div 2);
      OffsetRect(R, Bounds.Left, Bounds.Top);
      Result := R;
    end;

begin
  BorderRect := GetBorderSize;

  CaptionBitmap := TBitmap.Create;
  CaptionBitmap.SetSize(ARect.Width, BorderRect.Top);

  //Draw background
  LDetails.Element := teWindow;
  LDetails.Part := 0;
  Style.DrawElement(Canvas.Handle, LDetails, ARect);

  //Draw caption border
  CaptionRect := Rect(0, 0, CaptionBitmap.Width, CaptionBitmap.Height);
  LDetails := Style.GetElementDetails(twCaptionActive);
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
   if Assigned(Application.MainForm) then
    DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, Application.MainForm.Icon.Handle, 0, 0, 0, 0, DI_NORMAL);

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
  Style.DrawText(CaptionBitmap.Canvas.Handle, CaptionDetails, ACaption, TextRect, [tfLeft, tfSingleLine, tfVerticalCenter]);

  //Draw caption
  Canvas.Draw(0, 0, CaptionBitmap);
  CaptionBitmap.Free;

  //Draw left border
  CaptionRect := Rect(0, BorderRect.Top, BorderRect.Left, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameLeftActive);
  if CaptionRect.Bottom - CaptionRect.Top > 0 then
    Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw right border
  CaptionRect := Rect(ARect.Width - BorderRect.Right, BorderRect.Top, ARect.Width, ARect.Height - BorderRect.Bottom);
  LDetails := Style.GetElementDetails(twFrameRightActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);

  //Draw Bottom border
  CaptionRect := Rect(0, ARect.Height - BorderRect.Bottom, ARect.Width, ARect.Height);
  LDetails := Style.GetElementDetails(twFrameBottomActive);
  Style.DrawElement(Canvas.Handle, LDetails, CaptionRect);


  //Draw Ok button
  LDetails := Style.GetElementDetails(tbPushButtonNormal);
  ButtonRect.Left:=30;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'OK', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);

  //Draw Cancel button
  ButtonRect.Left:=110;
  ButtonRect.Top:=ARect.Height-45;
  ButtonRect.Width:=75;
  ButtonRect.Height:=25;
  Style.DrawElement(Canvas.Handle, LDetails, ButtonRect);

  Style.GetElementColor(LDetails, ecTextColor, ThemeTextColor);
  Style.DrawText(Canvas.Handle, LDetails, 'Cancel', ButtonRect, TTextFormatFlags(DT_VCENTER or DT_CENTER), ThemeTextColor);
end;

initialization
{$IFDEF USE_VCL_STYLESAPI}
 InitStyleAPI;
{$ENDIF}

finalization
{$IFDEF USE_VCL_STYLESAPI}
 FinalizeStyleAPI;
{$ENDIF}


end.
