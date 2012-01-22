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

const
  VclStyles_MaxSysColor = 23;
  VclStyles_SysColors: array[0..VclStyles_MaxSysColor - 1] of TIdentMapEntry = (
    (Value: Vcl.Graphics.clActiveBorder; Name: 'clActiveBorder'),
    (Value: Vcl.Graphics.clActiveCaption; Name: 'clActiveCaption'),
    (Value: Vcl.Graphics.clBtnFace; Name: 'clBtnFace'),
    (Value: Vcl.Graphics.clBtnHighlight; Name: 'clBtnHighlight'),
    (Value: Vcl.Graphics.clBtnShadow; Name: 'clBtnShadow'),
    (Value: Vcl.Graphics.clBtnText; Name: 'clBtnText'),
    (Value: Vcl.Graphics.clCaptionText; Name: 'clCaptionText'),
    (Value: Vcl.Graphics.clGrayText; Name: 'clGrayText'),
    (Value: Vcl.Graphics.clHighlight; Name: 'clHighlight'),
    (Value: Vcl.Graphics.clHighlightText; Name: 'clHighlightText'),
    (Value: Vcl.Graphics.clInactiveBorder; Name: 'clInactiveBorder'),
    (Value: Vcl.Graphics.clInactiveCaption; Name: 'clInactiveCaption'),
    (Value: Vcl.Graphics.clInactiveCaptionText; Name: 'clInactiveCaptionText'),
    (Value: Vcl.Graphics.clInfoBk; Name: 'clInfoBk'),
    (Value: Vcl.Graphics.clInfoText; Name: 'clInfoText'),
    (Value: Vcl.Graphics.clMenu; Name: 'clMenu'),
    (Value: Vcl.Graphics.clMenuText; Name: 'clMenuText'),
    (Value: Vcl.Graphics.clScrollBar; Name: 'clScrollBar'),
    (Value: Vcl.Graphics.cl3DDkShadow; Name: 'cl3DDkShadow'),
    (Value: Vcl.Graphics.cl3DLight; Name: 'cl3DLight'),
    (Value: Vcl.Graphics.clWindow; Name: 'clWindow'),
    (Value: Vcl.Graphics.clWindowFrame; Name: 'clWindowFrame'),
    (Value: Vcl.Graphics.clWindowText; Name: 'clWindowText'));

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
    function GetSource: TObject;
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

    property Source: TObject read GetSource;
    procedure SetStyleColor(Color: TStyleColor; NewColor: TColor);
    procedure SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
    procedure SetSystemColor(Color: TColor; NewColor: TColor);
  end;

        {
  TCustomStyleHelper = Class Helper for TCustomStyle
  private
    function GetSource: TObject;
  public
    property Source: TObject read GetSource;
    procedure SetStyleColor(Color: TStyleColor; NewColor: TColor);
    procedure SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
    procedure SetSystemColor(Color: TColor; NewColor: TColor);
  End;
      }

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
 Rtti,
 Vcl.Dialogs,
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
     if (t.Value.Data<>nil) then
     begin
       TStream(t.Value.Data).Position:=0;
       break;
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

  //Replace the modified bitmaps
  for i := 0 to TseStyle(Source).FCleanCopy.Bitmaps.Count-1  do
   TseStyle(Source).FCleanCopy.Bitmaps[i].Assign(TseStyle(Source).StyleSource.Bitmaps[i]);

  //TseStyle(Source).StyleSource.SysColors.Assign(TseStyle(Source).SysColors);

  //Replace the modified colors
  TseStyle(Source).FCleanCopy.SysColors.Assign(TseStyle(Source).SysColors);
  TseStyle(Source).FCleanCopy.Colors.Assign(TseStyle(Source).Colors);
  TseStyle(Source).FCleanCopy.Fonts.Assign(TseStyle(Source).Fonts);

  //ShowMessage(ColorToString(TseStyle(Source).SysColors[clWindow]));
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

procedure TCustomStyleExt.SetStyleColor(Color: TStyleColor; NewColor: TColor);
begin
  case Color of
    scBorder: if TSeStyle(Source).Colors[ktcBorder]<>NewColor then TSeStyle(Source).Colors[ktcBorder]:=NewColor;
    scButtonDisabled:  if TSeStyle(Source).Colors[ktcButtonDisabled]<>NewColor then TSeStyle(Source).Colors[ktcButtonDisabled]:=NewColor;
    scButtonFocused:  if TSeStyle(Source).Colors[ktcButtonFocused]<>NewColor then TSeStyle(Source).Colors[ktcButtonFocused]:=NewColor;
    scButtonHot:  if TSeStyle(Source).Colors[ktcButtonHot]<>NewColor then TSeStyle(Source).Colors[ktcButtonHot]:=NewColor;
    scButtonNormal:  if TSeStyle(Source).Colors[ktcButton]<>NewColor then TSeStyle(Source).Colors[ktcButton]:=NewColor;
    scButtonPressed:  if TSeStyle(Source).Colors[ktcButtonPressed]<>NewColor then TSeStyle(Source).Colors[ktcButtonPressed]:=NewColor;
    scCategoryButtons:  if TSeStyle(Source).Colors[ktcCategoryButtons]<>NewColor then TSeStyle(Source).Colors[ktcCategoryButtons]:=NewColor;
    scCategoryButtonsGradientBase:  if TSeStyle(Source).Colors[ktcCategoryButtonsGradientBase]<>NewColor then TSeStyle(Source).Colors[ktcCategoryButtonsGradientBase]:=NewColor;
    scCategoryButtonsGradientEnd:  if TSeStyle(Source).Colors[ktcCategoryButtonsGradientEnd]<>NewColor then TSeStyle(Source).Colors[ktcCategoryButtonsGradientEnd]:=NewColor;
    scCategoryPanelGroup:  if TSeStyle(Source).Colors[ktcCategoryPanelGroup]<>NewColor then TSeStyle(Source).Colors[ktcCategoryPanelGroup]:=NewColor;
    scComboBox:  if TSeStyle(Source).Colors[ktcComboBox]<>NewColor then TSeStyle(Source).Colors[ktcComboBox]:=NewColor;
    scComboBoxDisabled:  if TSeStyle(Source).Colors[ktcComboBoxDisabled]<>NewColor then TSeStyle(Source).Colors[ktcComboBoxDisabled]:=NewColor;
    scEdit:  if TSeStyle(Source).Colors[ktcEdit]<>NewColor then TSeStyle(Source).Colors[ktcEdit]:=NewColor;
    scEditDisabled:  if TSeStyle(Source).Colors[ktcEditDisabled]<>NewColor then TSeStyle(Source).Colors[ktcEditDisabled]:=NewColor;
    scGrid:  if TSeStyle(Source).Colors[ktcGrid]<>NewColor then TSeStyle(Source).Colors[ktcGrid]:=NewColor;
    scGenericBackground:  if TSeStyle(Source).Colors[ktcGenericBackground]<>NewColor then TSeStyle(Source).Colors[ktcGenericBackground]:=NewColor;
    scGenericGradientEnd:  if TSeStyle(Source).Colors[ktcGenericGradientEnd]<>NewColor then TSeStyle(Source).Colors[ktcGenericGradientEnd]:=NewColor;
    scGenericGradientBase:  if TSeStyle(Source).Colors[ktcGenericGradientBase]<>NewColor then TSeStyle(Source).Colors[ktcGenericGradientBase]:=NewColor;
    scHintGradientBase:  if TSeStyle(Source).Colors[ktcHintGradientBase]<>NewColor then TSeStyle(Source).Colors[ktcHintGradientBase]:=NewColor;
    scHintGradientEnd:  if TSeStyle(Source).Colors[ktcHintGradientEnd]<>NewColor then TSeStyle(Source).Colors[ktcHintGradientEnd]:=NewColor;
    scListBox:  if TSeStyle(Source).Colors[ktcListBox]<>NewColor then TSeStyle(Source).Colors[ktcListBox]:=NewColor;
    scListBoxDisabled:  if TSeStyle(Source).Colors[ktcListBoxDisabled]<>NewColor then TSeStyle(Source).Colors[ktcListBoxDisabled]:=NewColor;
    scListView:  if TSeStyle(Source).Colors[ktcListView]<>NewColor then TSeStyle(Source).Colors[ktcListView]:=NewColor;
    scPanel:  if TSeStyle(Source).Colors[ktcPanel]<>NewColor then TSeStyle(Source).Colors[ktcPanel]:=NewColor;
    scPanelDisabled:  if TSeStyle(Source).Colors[ktcPanelDisabled]<>NewColor then TSeStyle(Source).Colors[ktcPanelDisabled]:=NewColor;
    scSplitter:  if TSeStyle(Source).Colors[ktcSplitter]<>NewColor then TSeStyle(Source).Colors[ktcSplitter]:=NewColor;
    scToolBarGradientBase:  if TSeStyle(Source).Colors[ktcToolBarGradientBase]<>NewColor then TSeStyle(Source).Colors[ktcToolBarGradientBase]:=NewColor;
    scToolBarGradientEnd:  if TSeStyle(Source).Colors[ktcToolBarGradientEnd]<>NewColor then TSeStyle(Source).Colors[ktcToolBarGradientEnd]:=NewColor;
    scTreeView:  if TSeStyle(Source).Colors[ktcTreeView]<>NewColor then TSeStyle(Source).Colors[ktcTreeView]:=NewColor;
    scWindow: if TSeStyle(Source).Colors[ktcWindow]<>NewColor then TSeStyle(Source).Colors[ktcWindow]:=NewColor;
  end;
end;

procedure TCustomStyleExt.SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
begin
  case Font of
    sfButtonTextDisabled: TSeStyle(Source).Fonts[ktfButtonTextDisabled].Color:=NewColor;
    sfButtonTextFocused: TSeStyle(Source).Fonts[ktfButtonTextFocused].Color:=NewColor;
    sfButtonTextHot: TSeStyle(Source).Fonts[ktfButtonTextHot].Color:=NewColor;
    sfButtonTextNormal: TSeStyle(Source).Fonts[ktfButtonTextNormal].Color:=NewColor;
    sfButtonTextPressed: TSeStyle(Source).Fonts[ktfButtonTextPressed].Color:=NewColor;
    sfCaptionTextInactive: TSeStyle(Source).Fonts[ktfCaptionTextInactive].Color:=NewColor;
    sfCaptionTextNormal: TSeStyle(Source).Fonts[ktfCaptionTextNormal].Color:=NewColor;
    sfCategoryPanelGroupHeaderHot: TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderHot].Color:=NewColor;
    sfCategoryPanelGroupHeaderNormal: TSeStyle(Source).Fonts[ktfCategoryPanelGroupHeaderNormal].Color:=NewColor;
    sfCatgeoryButtonsCategoryNormal: TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategoryNormal].Color:=NewColor;
    sfCatgeoryButtonsCategorySelected: TSeStyle(Source).Fonts[ktfCatgeoryButtonsCategorySelected].Color:=NewColor;
    sfCatgeoryButtonsHot: TSeStyle(Source).Fonts[ktfCatgeoryButtonsHot].Color:=NewColor;
    sfCatgeoryButtonsNormal: TSeStyle(Source).Fonts[ktfCatgeoryButtonsNormal].Color:=NewColor;
    sfCatgeoryButtonsSelected: TSeStyle(Source).Fonts[ktfCatgeoryButtonsSelected].Color:=NewColor;
    sfCheckBoxTextDisabled: TSeStyle(Source).Fonts[ktfCheckBoxTextDisabled].Color:=NewColor;
    sfCheckBoxTextFocused: TSeStyle(Source).Fonts[ktfCheckBoxTextFocused].Color:=NewColor;
    sfCheckBoxTextHot: TSeStyle(Source).Fonts[ktfCheckBoxTextHot].Color:=NewColor;
    sfCheckBoxTextNormal: TSeStyle(Source).Fonts[ktfCheckBoxTextNormal].Color:=NewColor;
    sfCheckBoxTextPressed: TSeStyle(Source).Fonts[ktfCheckBoxTextPressed].Color:=NewColor;
    sfComboBoxItemDisabled: TSeStyle(Source).Fonts[ktfComboBoxItemDisabled].Color:=NewColor;
    sfComboBoxItemFocused: TSeStyle(Source).Fonts[ktfComboBoxItemFocused].Color:=NewColor;
    sfComboBoxItemHot: TSeStyle(Source).Fonts[ktfComboBoxItemHot].Color:=NewColor;
    sfComboBoxItemNormal: TSeStyle(Source).Fonts[ktfComboBoxItemNormal].Color:=NewColor;
    sfComboBoxItemSelected: TSeStyle(Source).Fonts[ktfComboBoxItemSelected].Color:=NewColor;
    sfEditBoxTextDisabled: TSeStyle(Source).Fonts[ktfEditBoxTextDisabled].Color:=NewColor;
    sfEditBoxTextFocused: TSeStyle(Source).Fonts[ktfEditBoxTextFocused].Color:=NewColor;
    sfEditBoxTextHot: TSeStyle(Source).Fonts[ktfEditBoxTextHot].Color:=NewColor;
    sfEditBoxTextNormal: TSeStyle(Source).Fonts[ktfEditBoxTextNormal].Color:=NewColor;
    sfEditBoxTextSelected: TSeStyle(Source).Fonts[ktfEditBoxTextSelected].Color:=NewColor;
    sfGridItemFixedHot: TSeStyle(Source).Fonts[ktfGridItemFixedHot].Color:=NewColor;
    sfGridItemFixedNormal: TSeStyle(Source).Fonts[ktfGridItemFixedNormal].Color:=NewColor;
    sfGridItemFixedPressed: TSeStyle(Source).Fonts[ktfGridItemFixedPressed].Color:=NewColor;
    sfGridItemNormal: TSeStyle(Source).Fonts[ktfGridItemNormal].Color:=NewColor;
    sfGridItemSelected: TSeStyle(Source).Fonts[ktfGridItemSelected].Color:=NewColor;
    sfGroupBoxTextDisabled: TSeStyle(Source).Fonts[ktfGroupBoxTextDisabled].Color:=NewColor;
    sfGroupBoxTextNormal: TSeStyle(Source).Fonts[ktfGroupBoxTextNormal].Color:=NewColor;
    sfHeaderSectionTextDisabled: TSeStyle(Source).Fonts[ktfHeaderSectionTextDisabled].Color:=NewColor;
    sfHeaderSectionTextHot: TSeStyle(Source).Fonts[ktfHeaderSectionTextHot].Color:=NewColor;
    sfHeaderSectionTextNormal: TSeStyle(Source).Fonts[ktfHeaderSectionTextNormal].Color:=NewColor;
    sfHeaderSectionTextPressed: TSeStyle(Source).Fonts[ktfHeaderSectionTextPressed].Color:=NewColor;
    sfListItemTextDisabled: TSeStyle(Source).Fonts[ktfListItemTextDisabled].Color:=NewColor;
    sfListItemTextFocused: TSeStyle(Source).Fonts[ktfListItemTextFocused].Color:=NewColor;
    sfListItemTextHot: TSeStyle(Source).Fonts[ktfListItemTextHot].Color:=NewColor;
    sfListItemTextNormal: TSeStyle(Source).Fonts[ktfListItemTextNormal].Color:=NewColor;
    sfListItemTextSelected: TSeStyle(Source).Fonts[ktfListItemTextSelected].Color:=NewColor;
    sfMenuItemTextDisabled: TSeStyle(Source).Fonts[ktfMenuItemTextDisabled].Color:=NewColor;
    sfMenuItemTextHot: TSeStyle(Source).Fonts[ktfMenuItemTextHot].Color:=NewColor;
    sfMenuItemTextNormal: TSeStyle(Source).Fonts[ktfMenuItemTextNormal].Color:=NewColor;
    sfMenuItemTextSelected: TSeStyle(Source).Fonts[ktfMenuItemTextSelected].Color:=NewColor;
    sfPanelTextDisabled: TSeStyle(Source).Fonts[ktfPanelTextDisabled].Color:=NewColor;
    sfPanelTextNormal: TSeStyle(Source).Fonts[ktfPanelTextNormal].Color:=NewColor;
    sfPopupMenuItemTextDisabled: TSeStyle(Source).Fonts[ktfPopupMenuItemTextDisabled].Color:=NewColor;
    sfPopupMenuItemTextHot: TSeStyle(Source).Fonts[ktfPopupMenuItemTextHot].Color:=NewColor;
    sfPopupMenuItemTextNormal: TSeStyle(Source).Fonts[ktfPopupMenuItemTextNormal].Color:=NewColor;
    sfPopupMenuItemTextSelected: TSeStyle(Source).Fonts[ktfPopupMenuItemTextSelected].Color:=NewColor;
    sfRadioButtonTextDisabled: TSeStyle(Source).Fonts[ktfRadioButtonTextDisabled].Color:=NewColor;
    sfRadioButtonTextFocused: TSeStyle(Source).Fonts[ktfRadioButtonTextFocused].Color:=NewColor;
    sfRadioButtonTextHot: TSeStyle(Source).Fonts[ktfRadioButtonTextHot].Color:=NewColor;
    sfRadioButtonTextNormal: TSeStyle(Source).Fonts[ktfRadioButtonTextNormal].Color:=NewColor;
    sfRadioButtonTextPressed: TSeStyle(Source).Fonts[ktfRadioButtonTextPressed].Color:=NewColor;
    sfSmCaptionTextInactive: TSeStyle(Source).Fonts[ktfSmCaptionTextInactive].Color:=NewColor;
    sfSmCaptionTextNormal: TSeStyle(Source).Fonts[ktfSmCaptionTextNormal].Color:=NewColor;
    sfStatusPanelTextDisabled: TSeStyle(Source).Fonts[ktfStatusPanelTextDisabled].Color:=NewColor;
    sfStatusPanelTextNormal: TSeStyle(Source).Fonts[ktfStatusPanelTextNormal].Color:=NewColor;
    sfTabTextActiveDisabled: TSeStyle(Source).Fonts[ktfTabTextActiveDisabled].Color:=NewColor;
    sfTabTextActiveHot: TSeStyle(Source).Fonts[ktfTabTextActiveHot].Color:=NewColor;
    sfTabTextActiveNormal: TSeStyle(Source).Fonts[ktfTabTextActiveNormal].Color:=NewColor;
    sfTabTextInactiveDisabled: TSeStyle(Source).Fonts[ktfTabTextInactiveDisabled].Color:=NewColor;
    sfTabTextInactiveHot: TSeStyle(Source).Fonts[ktfTabTextInactiveHot].Color:=NewColor;
    sfTabTextInactiveNormal: TSeStyle(Source).Fonts[ktfTabTextInactiveNormal].Color:=NewColor;
    sfTextLabelDisabled: TSeStyle(Source).Fonts[ktfStaticTextDisabled].Color:=NewColor;
    sfTextLabelFocused: TSeStyle(Source).Fonts[ktfStaticTextFocused].Color:=NewColor;
    sfTextLabelHot: TSeStyle(Source).Fonts[ktfStaticTextHot].Color:=NewColor;
    sfTextLabelNormal: TSeStyle(Source).Fonts[ktfStaticTextNormal].Color:=NewColor;
    sfToolItemTextDisabled: TSeStyle(Source).Fonts[ktfToolItemTextDisabled].Color:=NewColor;
    sfToolItemTextHot: TSeStyle(Source).Fonts[ktfToolItemTextHot].Color:=NewColor;
    sfToolItemTextNormal: TSeStyle(Source).Fonts[ktfToolItemTextNormal].Color:=NewColor;
    sfToolItemTextSelected: TSeStyle(Source).Fonts[ktfToolItemTextSelected].Color:=NewColor;
    sfTreeItemTextDisabled: TSeStyle(Source).Fonts[ktfTreeItemTextDisabled].Color:=NewColor;
    sfTreeItemTextFocused: TSeStyle(Source).Fonts[ktfTreeItemTextFocused].Color:=NewColor;
    sfTreeItemTextHot: TSeStyle(Source).Fonts[ktfTreeItemTextHot].Color:=NewColor;
    sfTreeItemTextNormal: TSeStyle(Source).Fonts[ktfTreeItemTextNormal].Color:=NewColor;
    sfTreeItemTextSelected: TSeStyle(Source).Fonts[ktfTreeItemTextSelected].Color:=NewColor;
    sfWindowTextDisabled: TSeStyle(Source).Fonts[ktfWindowTextDisabled].Color:=NewColor;
    sfWindowTextNormal: TSeStyle(Source).Fonts[ktfWindowTextNormal].Color:=NewColor;
  end;
end;

procedure TCustomStyleExt.SetSystemColor(Color, NewColor: TColor);
begin
  if TseStyle(Source).SysColors[Color]<>NewColor then
    TseStyle(Source).SysColors[Color]:=NewColor;
end;


function TCustomStyleExt.GetSource: TObject;
begin
  Result:=TRttiContext.Create.GetType(Self.ClassType).GetField('FSource').GetValue(Self).AsObject;
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
//function TCustomStyleHelper.GetSource: TObject;
//begin
//  {$IFDEF USE_RTTI}
//  Result:=TRttiContext.Create.GetType(Self.ClassType).GetField('FSource').GetValue(Self).AsObject;
//  {$ELSE}
//  Result:=Self.FSource;
//  {$ENDIF}
//end;
//
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

  //BlendFunction   : TBlendFunction;

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
  {
  BlendFunction.BlendOp := AC_SRC_OVER;
  BlendFunction.BlendFlags := 0;
  BlendFunction.SourceConstantAlpha := 100;
  BlendFunction.AlphaFormat :=  AC_SRC_OVER;
  WinAPi.Windows.AlphaBlend(
    Canvas.Handle,
    0,
    0,
    CaptionBitmap.Width,
    CaptionBitmap.Height,
    CaptionBitmap.Canvas.Handle,
    0,
    0,
    CaptionBitmap.Width,
    CaptionBitmap.Height,
    BlendFunction
  );
    }


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
