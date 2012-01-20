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
   class function RegisteredStyles: TDictionary<string, TSourceInfo>;
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
   class procedure ReloadStyle(const Name: string);
   {$REGION 'Documentation'}
   ///	<summary>remove a vcl style
   ///	</summary>
   {$ENDREGION}
   class procedure RemoveStyle(const Name: string);
   end;


procedure ApplyEmptyVCLStyleHook(ControlClass :TClass);
procedure RemoveEmptyVCLStyleHook(ControlClass :TClass);
function  IsStyleHookRegistered(ControlClass: TClass; StyleHookClass: TStyleHookClass) : Boolean;
function  GetRegisteredStylesHooks(ControlClass: TClass) : TStyleHookList;
procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string;hIcon:HICON=0);


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
    constructor Create(const FileName :string);reintroduce; overload;
    {$REGION 'Documentation'}
    ///	<summary>Create a  TCustomStyleExt using a vcl style stored in a stream
    ///	</summary>
    {$ENDREGION}
    constructor Create(const Stream:TStream);reintroduce; overload;
    constructor Create(const Style:TCustomStyle);reintroduce; overload;
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


  TCustomStyleHelper = Class Helper for TCustomStyle
  private
    function GetSource: TObject;
  public
    property Source: TObject read GetSource;
    procedure SetStyleColor(Color: TStyleColor; NewColor: TColor);
    procedure SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
    procedure SetSystemColor(Color: TColor; NewColor: TColor);
  End;

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
  TStyleHookDictionary = TDictionary<TClass, TStyleHookList>;
  TCustomStyleEngineHelper = Class Helper for TCustomStyleEngine
  public
    class function GetRegisteredStyleHooks : TStyleHookDictionary;
  End;



class function TCustomStyleEngineHelper.GetRegisteredStyleHooks: TStyleHookDictionary;
begin
  Result:= Self.FRegisteredStyleHooks;
end;

function TCustomStyleHelper.GetSource: TObject;
begin
  Result:=Self.FSource;
end;

{ TStyleManagerHelper }



class function TStyleManagerHelper.RegisteredStyles: TDictionary<string, TSourceInfo>;
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
  LRegisteredStyles:=TStyleManager.RegisteredStyles;
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

class procedure TStyleManagerHelper.RemoveStyle(const Name: string);
var
  LStyle: TCustomStyleServices;
  t     : TPair<string, TStyleManager.TSourceInfo>;
begin
 if SameText(Name, ActiveStyle.Name, loUserLocale) then
   SetStyle(SystemStyle);

 for LStyle in Styles do
  if SameText(Name, LStyle.Name, loUserLocale) then
  begin
    LStyle.Free;
    Styles.Remove(LStyle);
  end;

  for t in Self.FRegisteredStyles do
    if SameText(Name, t.Key, loUserLocale) then
     Self.FRegisteredStyles.Remove(t.Key);

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



{ TCustomStyleHelper }

procedure TCustomStyleHelper.SetStyleColor(Color: TStyleColor; NewColor: TColor);
begin
//  TseStyle(Self.FSource).SysColors[Color]:=NewColor;
  case Color of
    scBorder: TSeStyle(Self.FSource).Colors[ktcBorder]:=NewColor;
    scButtonDisabled: TSeStyle(Self.FSource).Colors[ktcButtonDisabled]:=NewColor;
    scButtonFocused: TSeStyle(Self.FSource).Colors[ktcButtonFocused]:=NewColor;
    scButtonHot: TSeStyle(Self.FSource).Colors[ktcButtonHot]:=NewColor;
    scButtonNormal: TSeStyle(Self.FSource).Colors[ktcButton]:=NewColor;
    scButtonPressed: TSeStyle(Self.FSource).Colors[ktcButtonPressed]:=NewColor;
    scCategoryButtons: TSeStyle(Self.FSource).Colors[ktcCategoryButtons]:=NewColor;
    scCategoryButtonsGradientBase: TSeStyle(Self.FSource).Colors[ktcCategoryButtonsGradientBase]:=NewColor;
    scCategoryButtonsGradientEnd: TSeStyle(Self.FSource).Colors[ktcCategoryButtonsGradientEnd]:=NewColor;
    scCategoryPanelGroup: TSeStyle(Self.FSource).Colors[ktcCategoryPanelGroup]:=NewColor;
    scComboBox: TSeStyle(Self.FSource).Colors[ktcComboBox]:=NewColor;
    scComboBoxDisabled: TSeStyle(Self.FSource).Colors[ktcComboBoxDisabled]:=NewColor;
    scEdit: TSeStyle(Self.FSource).Colors[ktcEdit]:=NewColor;
    scEditDisabled: TSeStyle(Self.FSource).Colors[ktcEditDisabled]:=NewColor;
    scGrid: TSeStyle(Self.FSource).Colors[ktcGrid]:=NewColor;
    scGenericBackground: TSeStyle(Self.FSource).Colors[ktcGenericBackground]:=NewColor;
    scGenericGradientEnd: TSeStyle(Self.FSource).Colors[ktcGenericGradientEnd]:=NewColor;
    scGenericGradientBase: TSeStyle(Self.FSource).Colors[ktcGenericGradientBase]:=NewColor;
    scHintGradientBase: TSeStyle(Self.FSource).Colors[ktcHintGradientBase]:=NewColor;
    scHintGradientEnd: TSeStyle(Self.FSource).Colors[ktcHintGradientEnd]:=NewColor;
    scListBox: TSeStyle(Self.FSource).Colors[ktcListBox]:=NewColor;
    scListBoxDisabled: TSeStyle(Self.FSource).Colors[ktcListBoxDisabled]:=NewColor;
    scListView: TSeStyle(Self.FSource).Colors[ktcListView]:=NewColor;
    scPanel: TSeStyle(Self.FSource).Colors[ktcPanel]:=NewColor;
    scPanelDisabled: TSeStyle(Self.FSource).Colors[ktcPanelDisabled]:=NewColor;
    scSplitter: TSeStyle(Self.FSource).Colors[ktcSplitter]:=NewColor;
    scToolBarGradientBase: TSeStyle(Self.FSource).Colors[ktcToolBarGradientBase]:=NewColor;
    scToolBarGradientEnd: TSeStyle(Self.FSource).Colors[ktcToolBarGradientEnd]:=NewColor;
    scTreeView: TSeStyle(Self.FSource).Colors[ktcTreeView]:=NewColor;
    scWindow: TSeStyle(Self.FSource).Colors[ktcWindow]:=NewColor;
  end;
end;

procedure TCustomStyleHelper.SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
begin
  case Font of
    sfButtonTextDisabled: TSeStyle(Self.FSource).Fonts[ktfButtonTextDisabled].Color:=NewColor;
    sfButtonTextFocused: TSeStyle(Self.FSource).Fonts[ktfButtonTextFocused].Color:=NewColor;
    sfButtonTextHot: TSeStyle(Self.FSource).Fonts[ktfButtonTextHot].Color:=NewColor;
    sfButtonTextNormal: TSeStyle(Self.FSource).Fonts[ktfButtonTextNormal].Color:=NewColor;
    sfButtonTextPressed: TSeStyle(Self.FSource).Fonts[ktfButtonTextPressed].Color:=NewColor;
    sfCaptionTextInactive: TSeStyle(Self.FSource).Fonts[ktfCaptionTextInactive].Color:=NewColor;
    sfCaptionTextNormal: TSeStyle(Self.FSource).Fonts[ktfCaptionTextNormal].Color:=NewColor;
    sfCategoryPanelGroupHeaderHot: TSeStyle(Self.FSource).Fonts[ktfCategoryPanelGroupHeaderHot].Color:=NewColor;
    sfCategoryPanelGroupHeaderNormal: TSeStyle(Self.FSource).Fonts[ktfCategoryPanelGroupHeaderNormal].Color:=NewColor;
    sfCatgeoryButtonsCategoryNormal: TSeStyle(Self.FSource).Fonts[ktfCatgeoryButtonsCategoryNormal].Color:=NewColor;
    sfCatgeoryButtonsCategorySelected: TSeStyle(Self.FSource).Fonts[ktfCatgeoryButtonsCategorySelected].Color:=NewColor;
    sfCatgeoryButtonsHot: TSeStyle(Self.FSource).Fonts[ktfCatgeoryButtonsHot].Color:=NewColor;
    sfCatgeoryButtonsNormal: TSeStyle(Self.FSource).Fonts[ktfCatgeoryButtonsNormal].Color:=NewColor;
    sfCatgeoryButtonsSelected: TSeStyle(Self.FSource).Fonts[ktfCatgeoryButtonsSelected].Color:=NewColor;
    sfCheckBoxTextDisabled: TSeStyle(Self.FSource).Fonts[ktfCheckBoxTextDisabled].Color:=NewColor;
    sfCheckBoxTextFocused: TSeStyle(Self.FSource).Fonts[ktfCheckBoxTextFocused].Color:=NewColor;
    sfCheckBoxTextHot: TSeStyle(Self.FSource).Fonts[ktfCheckBoxTextHot].Color:=NewColor;
    sfCheckBoxTextNormal: TSeStyle(Self.FSource).Fonts[ktfCheckBoxTextNormal].Color:=NewColor;
    sfCheckBoxTextPressed: TSeStyle(Self.FSource).Fonts[ktfCheckBoxTextPressed].Color:=NewColor;
    sfComboBoxItemDisabled: TSeStyle(Self.FSource).Fonts[ktfComboBoxItemDisabled].Color:=NewColor;
    sfComboBoxItemFocused: TSeStyle(Self.FSource).Fonts[ktfComboBoxItemFocused].Color:=NewColor;
    sfComboBoxItemHot: TSeStyle(Self.FSource).Fonts[ktfComboBoxItemHot].Color:=NewColor;
    sfComboBoxItemNormal: TSeStyle(Self.FSource).Fonts[ktfComboBoxItemNormal].Color:=NewColor;
    sfComboBoxItemSelected: TSeStyle(Self.FSource).Fonts[ktfComboBoxItemSelected].Color:=NewColor;
    sfEditBoxTextDisabled: TSeStyle(Self.FSource).Fonts[ktfEditBoxTextDisabled].Color:=NewColor;
    sfEditBoxTextFocused: TSeStyle(Self.FSource).Fonts[ktfEditBoxTextFocused].Color:=NewColor;
    sfEditBoxTextHot: TSeStyle(Self.FSource).Fonts[ktfEditBoxTextHot].Color:=NewColor;
    sfEditBoxTextNormal: TSeStyle(Self.FSource).Fonts[ktfEditBoxTextNormal].Color:=NewColor;
    sfEditBoxTextSelected: TSeStyle(Self.FSource).Fonts[ktfEditBoxTextSelected].Color:=NewColor;
    sfGridItemFixedHot: TSeStyle(Self.FSource).Fonts[ktfGridItemFixedHot].Color:=NewColor;
    sfGridItemFixedNormal: TSeStyle(Self.FSource).Fonts[ktfGridItemFixedNormal].Color:=NewColor;
    sfGridItemFixedPressed: TSeStyle(Self.FSource).Fonts[ktfGridItemFixedPressed].Color:=NewColor;
    sfGridItemNormal: TSeStyle(Self.FSource).Fonts[ktfGridItemNormal].Color:=NewColor;
    sfGridItemSelected: TSeStyle(Self.FSource).Fonts[ktfGridItemSelected].Color:=NewColor;
    sfGroupBoxTextDisabled: TSeStyle(Self.FSource).Fonts[ktfGroupBoxTextDisabled].Color:=NewColor;
    sfGroupBoxTextNormal: TSeStyle(Self.FSource).Fonts[ktfGroupBoxTextNormal].Color:=NewColor;
    sfHeaderSectionTextDisabled: TSeStyle(Self.FSource).Fonts[ktfHeaderSectionTextDisabled].Color:=NewColor;
    sfHeaderSectionTextHot: TSeStyle(Self.FSource).Fonts[ktfHeaderSectionTextHot].Color:=NewColor;
    sfHeaderSectionTextNormal: TSeStyle(Self.FSource).Fonts[ktfHeaderSectionTextNormal].Color:=NewColor;
    sfHeaderSectionTextPressed: TSeStyle(Self.FSource).Fonts[ktfHeaderSectionTextPressed].Color:=NewColor;
    sfListItemTextDisabled: TSeStyle(Self.FSource).Fonts[ktfListItemTextDisabled].Color:=NewColor;
    sfListItemTextFocused: TSeStyle(Self.FSource).Fonts[ktfListItemTextFocused].Color:=NewColor;
    sfListItemTextHot: TSeStyle(Self.FSource).Fonts[ktfListItemTextHot].Color:=NewColor;
    sfListItemTextNormal: TSeStyle(Self.FSource).Fonts[ktfListItemTextNormal].Color:=NewColor;
    sfListItemTextSelected: TSeStyle(Self.FSource).Fonts[ktfListItemTextSelected].Color:=NewColor;
    sfMenuItemTextDisabled: TSeStyle(Self.FSource).Fonts[ktfMenuItemTextDisabled].Color:=NewColor;
    sfMenuItemTextHot: TSeStyle(Self.FSource).Fonts[ktfMenuItemTextHot].Color:=NewColor;
    sfMenuItemTextNormal: TSeStyle(Self.FSource).Fonts[ktfMenuItemTextNormal].Color:=NewColor;
    sfMenuItemTextSelected: TSeStyle(Self.FSource).Fonts[ktfMenuItemTextSelected].Color:=NewColor;
    sfPanelTextDisabled: TSeStyle(Self.FSource).Fonts[ktfPanelTextDisabled].Color:=NewColor;
    sfPanelTextNormal: TSeStyle(Self.FSource).Fonts[ktfPanelTextNormal].Color:=NewColor;
    sfPopupMenuItemTextDisabled: TSeStyle(Self.FSource).Fonts[ktfPopupMenuItemTextDisabled].Color:=NewColor;
    sfPopupMenuItemTextHot: TSeStyle(Self.FSource).Fonts[ktfPopupMenuItemTextHot].Color:=NewColor;
    sfPopupMenuItemTextNormal: TSeStyle(Self.FSource).Fonts[ktfPopupMenuItemTextNormal].Color:=NewColor;
    sfPopupMenuItemTextSelected: TSeStyle(Self.FSource).Fonts[ktfPopupMenuItemTextSelected].Color:=NewColor;
    sfRadioButtonTextDisabled: TSeStyle(Self.FSource).Fonts[ktfRadioButtonTextDisabled].Color:=NewColor;
    sfRadioButtonTextFocused: TSeStyle(Self.FSource).Fonts[ktfRadioButtonTextFocused].Color:=NewColor;
    sfRadioButtonTextHot: TSeStyle(Self.FSource).Fonts[ktfRadioButtonTextHot].Color:=NewColor;
    sfRadioButtonTextNormal: TSeStyle(Self.FSource).Fonts[ktfRadioButtonTextNormal].Color:=NewColor;
    sfRadioButtonTextPressed: TSeStyle(Self.FSource).Fonts[ktfRadioButtonTextPressed].Color:=NewColor;
    sfSmCaptionTextInactive: TSeStyle(Self.FSource).Fonts[ktfSmCaptionTextInactive].Color:=NewColor;
    sfSmCaptionTextNormal: TSeStyle(Self.FSource).Fonts[ktfSmCaptionTextNormal].Color:=NewColor;
    sfStatusPanelTextDisabled: TSeStyle(Self.FSource).Fonts[ktfStatusPanelTextDisabled].Color:=NewColor;
    sfStatusPanelTextNormal: TSeStyle(Self.FSource).Fonts[ktfStatusPanelTextNormal].Color:=NewColor;
    sfTabTextActiveDisabled: TSeStyle(Self.FSource).Fonts[ktfTabTextActiveDisabled].Color:=NewColor;
    sfTabTextActiveHot: TSeStyle(Self.FSource).Fonts[ktfTabTextActiveHot].Color:=NewColor;
    sfTabTextActiveNormal: TSeStyle(Self.FSource).Fonts[ktfTabTextActiveNormal].Color:=NewColor;
    sfTabTextInactiveDisabled: TSeStyle(Self.FSource).Fonts[ktfTabTextInactiveDisabled].Color:=NewColor;
    sfTabTextInactiveHot: TSeStyle(Self.FSource).Fonts[ktfTabTextInactiveHot].Color:=NewColor;
    sfTabTextInactiveNormal: TSeStyle(Self.FSource).Fonts[ktfTabTextInactiveNormal].Color:=NewColor;
    sfTextLabelDisabled: TSeStyle(Self.FSource).Fonts[ktfStaticTextDisabled].Color:=NewColor;
    sfTextLabelFocused: TSeStyle(Self.FSource).Fonts[ktfStaticTextFocused].Color:=NewColor;
    sfTextLabelHot: TSeStyle(Self.FSource).Fonts[ktfStaticTextHot].Color:=NewColor;
    sfTextLabelNormal: TSeStyle(Self.FSource).Fonts[ktfStaticTextNormal].Color:=NewColor;
    sfToolItemTextDisabled: TSeStyle(Self.FSource).Fonts[ktfToolItemTextDisabled].Color:=NewColor;
    sfToolItemTextHot: TSeStyle(Self.FSource).Fonts[ktfToolItemTextHot].Color:=NewColor;
    sfToolItemTextNormal: TSeStyle(Self.FSource).Fonts[ktfToolItemTextNormal].Color:=NewColor;
    sfToolItemTextSelected: TSeStyle(Self.FSource).Fonts[ktfToolItemTextSelected].Color:=NewColor;
    sfTreeItemTextDisabled: TSeStyle(Self.FSource).Fonts[ktfTreeItemTextDisabled].Color:=NewColor;
    sfTreeItemTextFocused: TSeStyle(Self.FSource).Fonts[ktfTreeItemTextFocused].Color:=NewColor;
    sfTreeItemTextHot: TSeStyle(Self.FSource).Fonts[ktfTreeItemTextHot].Color:=NewColor;
    sfTreeItemTextNormal: TSeStyle(Self.FSource).Fonts[ktfTreeItemTextNormal].Color:=NewColor;
    sfTreeItemTextSelected: TSeStyle(Self.FSource).Fonts[ktfTreeItemTextSelected].Color:=NewColor;
    sfWindowTextDisabled: TSeStyle(Self.FSource).Fonts[ktfWindowTextDisabled].Color:=NewColor;
    sfWindowTextNormal: TSeStyle(Self.FSource).Fonts[ktfWindowTextNormal].Color:=NewColor;
  end;
end;

procedure TCustomStyleHelper.SetSystemColor(Color, NewColor: TColor);
begin
  TseStyle(Self.FSource).SysColors[Color]:=NewColor;
end;

{$ENDIF}

procedure DrawSampleWindow(Style:TCustomStyle;Canvas:TCanvas;ARect:TRect;const ACaption : string;hIcon:HICON=0);
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
  {
   if Assigned(Application.MainForm) then
    DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, Application.MainForm.Icon.Handle, 0, 0, 0, 0, DI_NORMAL);
  }
   if hIcon<>0 then
    DrawIconEx(CaptionBitmap.Canvas.Handle, IconRect.Left, IconRect.Top, hIcon, 0, 0, 0, 0, DI_NORMAL);

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
