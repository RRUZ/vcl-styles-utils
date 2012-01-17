{**************************************************************************************************}
{                                                                                                  }
{ Unit uHueSat                                                                                     }
{ unit uHueSat  for the Delphi IDE Theme Editor                                                    }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Original Code is uHueSat.pas.                                                                }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2011 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}



unit uHueSat;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Mask, ComCtrls, Generics.Defaults, uHSLUtils,
  Generics.Collections, Vcl.ImgList, Vcl.ActnList,
  Vcl.PlatformDefaultStyleActnCtrls, Vcl.ActnMan;

type
  TFrmHueSat = class(TForm)
    BtnApply:    TButton;
    BtnSave: TButton;
    ImageList1: TImageList;
    ImageVCLStyle: TImage;
    Label4: TLabel;
    ComboBoxVclStyles: TComboBox;
    Button1: TButton;
    ActionManager1: TActionManager;
    ActionApplyStyle: TAction;
    CheckBoxSepia: TCheckBox;
    SaveDialog1: TSaveDialog;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TrackBarHue: TTrackBar;
    EditLight: TEdit;
    UpDownLight: TUpDown;
    EditSat: TEdit;
    UpDownSat: TUpDown;
    EditHue: TEdit;
    UpDownHue: TUpDown;
    ButtonHue: TButton;
    TrackBarSaturation: TTrackBar;
    ButtonSaturation: TButton;
    TrackBarLightness: TTrackBar;
    ButtonLightness: TButton;
    Label1: TLabel;
    Bevel4: TBevel;
    Label2: TLabel;
    Bevel2: TBevel;
    Label3: TLabel;
    Bevel3: TBevel;
    TabSheet2: TTabSheet;
    TrackBarRed: TTrackBar;
    Label5: TLabel;
    Bevel5: TBevel;
    EditRed: TEdit;
    UpDownRed: TUpDown;
    Button2: TButton;
    TrackBarGreen: TTrackBar;
    Label6: TLabel;
    EditGreen: TEdit;
    UpDownGreen: TUpDown;
    Button3: TButton;
    Bevel6: TBevel;
    TrackBarBlue: TTrackBar;
    Label7: TLabel;
    EditBlue: TEdit;
    UpDownBlue: TUpDown;
    Button4: TButton;
    Bevel7: TBevel;
    TabSheet3: TTabSheet;
    ButtonApplyBlend: TButton;
    RadioButtonHSL: TRadioButton;
    RadioButtonRGB: TRadioButton;
    RadioButtonBlend: TRadioButton;
    ColorBoxblend: TColorBox;
    Button6: TButton;
    ComboBoxBlend: TComboBox;
    Label8: TLabel;
    Label9: TLabel;
    ColorDialog1: TColorDialog;
    Label10: TLabel;
    procedure ButtonHueClick(Sender: TObject);
    procedure ButtonSaturationClick(Sender: TObject);
    procedure ButtonLightnessClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TrackBarHueChange(Sender: TObject);
    procedure TrackBarLightnessChange(Sender: TObject);
    procedure TrackBarSaturationChange(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure EditHueExit(Sender: TObject);
    procedure UpDownHueChanging(Sender: TObject; var AllowChange: Boolean);
    procedure EditSatExit(Sender: TObject);
    procedure UpDownSatChanging(Sender: TObject; var AllowChange: Boolean);
    procedure EditLightExit(Sender: TObject);
    procedure UpDownLightChanging(Sender: TObject; var AllowChange: Boolean);
    procedure ComboBoxVclStylesChange(Sender: TObject);
    procedure ActionApplyStyleUpdate(Sender: TObject);
    procedure ActionApplyStyleExecute(Sender: TObject);
    procedure CheckBoxSepiaClick(Sender: TObject);
    procedure BtnSaveClick(Sender: TObject);
    procedure TrackBarRedChange(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure ButtonApplyBlendClick(Sender: TObject);
    procedure RadioButtonHSLClick(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure ComboBoxBlendChange(Sender: TObject);
  private
    OriginalBitMap : TBitmap;
    SepiaBitMap    : TBitmap;
    ModifiedBitMap : TBitmap;
    FStyleName     : string;
    procedure DrawSeletedVCLStyle;
    function GetStyleName: string;
    property StyleName: string Read GetStyleName Write FStyleName;
    procedure LoadStyle;
    procedure SetPageActive(Index:integer);
    function GetFilters  : TObjectList<TBitmap32Filter>;
    procedure AcceptFiles(var msg : TMessage); message WM_DROPFILES;
    procedure FillListStyles;
    procedure BuildPreview;
  end;

var
  FrmHueSat : TFrmHueSat;

implementation

{$R *.dfm}

uses
  Rtti,
  ShellAPI,
  IOUtils,
  StrUtils,
  Vcl.Styles.Ext,
  Vcl.Styles.Utils,
  Vcl.Themes,
  Vcl.Styles,
  uVCLStylesInfo;


procedure TFrmHueSat.AcceptFiles(var msg: TMessage);
const
  cb = 255;
var
  FileIndex,
  nCount     : integer;
  FileName   : array [0..cb] of Char;
  StyleInfo  : TStyleInfo;
  StyleName  : string;
begin
  StyleName :='';
  nCount := DragQueryFile( msg.WParam, $FFFFFFFF, FileName, cb);
  for FileIndex := 0 to nCount-1 do
  begin
    DragQueryFile( msg.WParam, FileIndex, FileName, cb );
    if TStyleManager.IsValidStyle(FileName, StyleInfo) then
    if not MatchText(StyleInfo.Name, TStyleManager.StyleNames) then
    begin
      TStyleManager.LoadFromFile(FileName);
      StyleName:=StyleInfo.Name;
    end;
  end;

  FillListStyles;
  if StyleName<>'' then
  begin
   ComboBoxVclStyles.ItemIndex:=ComboBoxVclStyles.Items.IndexOf(StyleName);
   LoadStyle;
  end;


  DragFinish( msg.WParam );
end;

procedure TFrmHueSat.ActionApplyStyleExecute(Sender: TObject);
begin
 TStyleManager.SetStyle(StyleName);
end;

procedure TFrmHueSat.ActionApplyStyleUpdate(Sender: TObject);
begin
 TCustomAction(Sender).Enabled:=(StyleName<>'') and not SameText(StyleName, TStyleManager.ActiveStyle.Name, loUserLocale);
end;


function TFrmHueSat.GetFilters: TObjectList<TBitmap32Filter>;
var
  LFilter : TValue;
  ctx     : TRttiContext;
  RttiInstanceType : TRttiInstanceType;
begin
  Result:=TObjectList<TBitmap32Filter>.Create;

    if RadioButtonHSL.Checked then
    begin
      If CheckBoxSepia.Checked then
        Result.Add(TBitmap32SepiaFilter.Create(20));

      If UpDownHue.Position<>0 then
      begin
        if Trunc(UpDownHue.Position) >= 0 then
           Result.Add(TBitmap32HueFilter.Create(Trunc(UpDownHue.Position)))
        else
            Result.Add(TBitmap32HueFilter.Create(360-Abs(Trunc(UpDownHue.Position))));
      end;

      If UpDownSat.Position<>0 then
        Result.Add(TBitmap32SaturationFilter.Create((255 - ((Trunc(UpDownSat.Position) * 255) div MaxSat))));

      If UpDownLight.Position<>0 then
        Result.Add(TBitmap32LightnessFilter.Create(Trunc(UpDownLight.Position)));
    end;


    if RadioButtonRGB.Checked then
    begin
      If UpDownRed.Position>0 then
        Result.Add(TBitmap32RedFilter.Create(Trunc(UpDownRed.Position)));

      If UpDownGreen.Position>0 then
        Result.Add(TBitmap32GreenFilter.Create(Trunc(UpDownGreen.Position)));

      If UpDownBlue.Position>0 then
        Result.Add(TBitmap32BlueFilter.Create(Trunc(UpDownBlue.Position)));
    end;

    if RadioButtonBlend.Checked then
    begin
      ctx := TRttiContext.Create;
      RttiInstanceType := (ctx.GetType(ComboBoxBlend.Items.Objects[ComboBoxBlend.ItemIndex]) as TRttiInstanceType);
      LFilter := RttiInstanceType.GetMethod('Create').Invoke(RttiInstanceType.MetaclassType,[ColorBoxblend.Selected]);
      Result.Add(TBitmap32Filter(LFilter.AsObject));
      ctx.Free;
    end;
end;


procedure TFrmHueSat.BtnApplyClick(Sender: TObject);
Var
  LFilters : TObjectList<TBitmap32Filter>;
  VclUtils : TVclStylesUtils;
begin
  if StyleName='' then exit;
  try
    LFilters:=GetFilters;

    VclUtils:=TVclStylesUtils.Create(StyleName);
    try
      VclUtils.SetFilters(LFilters);
      VclUtils.ApplyChanges;
    finally
      LFilters.Free;
      VclUtils.Free;
    end;

    TStyleManager.ReloadStyle(StyleName);
    LoadStyle;
  except
    on E: Exception do
      ShowMessage(Format('Error saving vcl style - Message : %s : Trace %s', [E.Message, E.StackTrace]));
  end;
end;

procedure TFrmHueSat.BtnSaveClick(Sender: TObject);
Var
  LFilters : TObjectList<TBitmap32Filter>;
  VclUtils : TVclStylesUtils;
  Frm      : TFrmVCLStyleInfoDialog;
begin
 if StyleName='' then exit;

 if SaveDialog1.Execute then
 begin
    try
      LFilters:=GetFilters;
      VclUtils:=TVclStylesUtils.Create(StyleName);
      try
        Frm:=TFrmVCLStyleInfoDialog.Create(Self);
        try
         Frm.StyleInfo:=VclUtils.StyleExt.StyleInfo;
         if Frm.Execute then
          VclUtils.StyleExt.StyleInfo:=Frm.StyleInfo;
        finally
         Frm.Free;
        end;

        VclUtils.SetFilters(LFilters);
        VclUtils.SaveToFile(SaveDialog1.FileName);
        ImageVCLStyle.Picture.Bitmap.SaveToFile(ChangeFileExt(SaveDialog1.FileName,'.bmp'));
        MessageDlg('Vcl Style Saved', mtInformation, [mbOK], 0);
      finally
        LFilters.Free;
        VclUtils.Free;
      end;

    except
      on E: Exception do
        ShowMessage(Format('Error saving vcl style - Message : %s : Trace %s',
          [E.Message, E.StackTrace]));
    end;
 end;
end;


procedure TFrmHueSat.Button2Click(Sender: TObject);
begin
  UpDownRed.Position   := 0;
  TrackBarRed.Position := 0;
end;

procedure TFrmHueSat.Button3Click(Sender: TObject);
begin
  UpDownGreen.Position   := 0;
  TrackBarGreen.Position := 0;
end;

procedure TFrmHueSat.Button4Click(Sender: TObject);
begin
  UpDownBlue.Position   := 0;
  TrackBarBlue.Position := 0;
end;

procedure TFrmHueSat.ButtonApplyBlendClick(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.Button6Click(Sender: TObject);
begin
 if ColorDialog1.Execute then
  ColorBoxblend.Selected:=ColorDialog1.Color;
end;

procedure TFrmHueSat.ButtonHueClick(Sender: TObject);
begin
  UpDownHue.Position   := DefHue;
  TrackBarHue.Position := DefHue;
end;

procedure TFrmHueSat.ButtonLightnessClick(Sender: TObject);
begin
  UpDownLight.Position       := DefLig;
  TrackBarLightness.Position := DefLig;
end;

procedure TFrmHueSat.ButtonSaturationClick(Sender: TObject);
begin
  UpDownSat.Position := DefSat;
  TrackBarSaturation.Position := DefSat;
end;

procedure TFrmHueSat.CheckBoxSepiaClick(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.ComboBoxBlendChange(Sender: TObject);
begin
  BuildPreview;
end;

procedure TFrmHueSat.ComboBoxVclStylesChange(Sender: TObject);
begin
  LoadStyle;
  BuildPreview;
end;

procedure TFrmHueSat.DrawSeletedVCLStyle;
var
  LBitmap   : TBitmap;
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
begin
   ImageVCLStyle.Picture:=nil;
   if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
   begin
    LBitmap:=TBitmap.Create;
    try
       LBitmap.PixelFormat:=pf32bit;
       LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
       LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
       TStyleManager.StyleNames;//call DiscoverStyleResources
       SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
       LStyle:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
       try
         DrawSampleWindow(LStyle, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleName);
         ImageVCLStyle.Picture.Assign(LBitmap);
       finally
         LStyle.Free;
       end;
    finally
      LBitmap.Free;
    end;
   end;
end;

procedure TFrmHueSat.EditHueExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditHue.Text, Value) then
  begin
    if Value< UpDownHue.Min then
     EditHue.Text:=IntToStr(UpDownHue.Min)
    else
    if Value> UpDownHue.Max then
     EditHue.Text:=IntToStr(UpDownHue.Max);

     UpDownHueChanging(nil, Allow);
  end;
end;

procedure TFrmHueSat.EditLightExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditLight.Text, Value) then
  begin
    if Value< UpDownLight.Min then
     EditLight.Text:=IntToStr(UpDownLight.Min)
    else
    if Value> UpDownLight.Max then
     EditLight.Text:=IntToStr(UpDownLight.Max);

    UpDownLightChanging(nil, Allow);
  end;
end;


procedure TFrmHueSat.EditSatExit(Sender: TObject);
Var
  Value : Integer;
  Allow : Boolean;
begin
  if TryStrToInt(EditSat.Text, Value) then
  begin
    if Value< UpDownSat.Min then
     EditSat.Text:=IntToStr(UpDownSat.Min)
    else
    if Value> UpDownSat.Max then
     EditSat.Text:=IntToStr(UpDownSat.Max);

    UpDownSatChanging(nil, Allow);
  end;
end;


procedure TFrmHueSat.FillListStyles;
var
  s : string;
begin
 ComboBoxVclStyles.Items.Clear;
 for s in TStyleManager.StyleNames do
  if not SameText(s,'Windows') then
   ComboBoxVclStyles.Items.Add(s);
end;

procedure TFrmHueSat.FormCreate(Sender: TObject);
begin
 DragAcceptFiles( Handle, True );
 ReportMemoryLeaksOnShutdown:=True;

  FillListStyles;
  if ComboBoxVclStyles.Items.Count>0 then
  ComboBoxVclStyles.ItemIndex:=0;

  OriginalBitMap := TBitmap.Create;
  ModifiedBitMap := TBitmap.Create;
  SepiaBitMap    := TBitmap.Create;
  LoadStyle;


  With ComboBoxBlend.Items do
  begin
   AddObject('Burn', TypeInfo(TBitmap32BlendBurn));
   AddObject('Multiply', TypeInfo(TBitmap32BlendMultiply));
   AddObject('Additive', TypeInfo(TBitmap32BlendAdditive));
   AddObject('Dodge', TypeInfo(TBitmap32BlendDodge));
   AddObject('Overlay', TypeInfo(TBitmap32BlendOverlay));
   AddObject('Difference', TypeInfo(TBitmap32BlendDifference));
   AddObject('Lighten', TypeInfo(TBitmap32BlendLighten));
   AddObject('Darken', TypeInfo(TBitmap32BlendDarken));
   AddObject('Screen', TypeInfo(TBitmap32BlendScreen));
  end;

  ComboBoxBlend.ItemIndex:=0;
  SetPageActive(0);
end;

procedure TFrmHueSat.FormDestroy(Sender: TObject);
begin
  OriginalBitMap.Free;
  ModifiedBitMap.Free;
  SepiaBitMap.Free;
end;

function TFrmHueSat.GetStyleName: string;
begin
  Result := ComboBoxVclStyles.Text;
end;

procedure TFrmHueSat.BuildPreview;
var
  LFilters : TObjectList<TBitmap32Filter>;
  //VclUtils : TVclStylesUtils;
  LBitmap  : TBitmap;
  Filter   : TBitmap32Filter;
begin
  if StyleName='' then exit;
  LFilters:=GetFilters;
  //VclUtils:=TVclStylesUtils.Create(StyleName, True);
  LBitmap:=TBitmap.Create;
  try
    LBitmap.Assign(OriginalBitMap);
    for Filter in LFilters do
     Filter.Apply(LBitmap);

    ImageVCLStyle.Picture.Assign(LBitmap);

    {
    VclUtils.SetFilters(LFilters);
    VclUtils.ApplyChanges;
      LBitmap:=TBitmap.Create;
      try
         ImageVCLStyle.Picture:=nil;
         LBitmap.PixelFormat:=pf32bit;
         LBitmap.Width :=ImageVCLStyle.ClientRect.Width;
         LBitmap.Height:=ImageVCLStyle.ClientRect.Height;
         DrawSampleWindow(VclUtils.StyleExt, LBitmap.Canvas, ImageVCLStyle.ClientRect, StyleName, Icon.Handle);
         ImageVCLStyle.Picture.Assign(LBitmap);
      finally
        LBitmap.Free;
      end;
     }
  finally
    LFilters.Free;
    LBitmap.Free;
    //VclUtils.Free;
  end;
end;


procedure TFrmHueSat.SetPageActive(Index: integer);
var
 i : Integer;
begin
  PageControl1.ActivePageIndex:=Index;
  for i := 0 to PageControl1.PageCount-1 do
   PageControl1.Pages[i].TabVisible:=i=Index;
end;


procedure TFrmHueSat.LoadStyle;
begin
  CheckBoxSepia.Checked:=False;
  DrawSeletedVCLStyle;

  OriginalBitMap.Assign(ImageVCLStyle.Picture.Bitmap);
  ModifiedBitMap.Assign(OriginalBitMap);

  UpDownHue.Position   := DefHue;
  TrackBarHue.Position := DefHue;

  UpDownSat.Position := DefSat;
  TrackBarSaturation.Position := DefSat;

  UpDownLight.Position       := DefLig;
  TrackBarLightness.Position := DefLig;

  UpDownRed.Position       := 0;
  TrackBarRed.Position     := 0;

  UpDownGreen.Position       := 0;
  TrackBarGreen.Position     := 0;

  UpDownBlue.Position       := 0;
  TrackBarBlue.Position     := 0;
end;

procedure TFrmHueSat.RadioButtonHSLClick(Sender: TObject);
begin
  SetPageActive(TRadioButton(Sender).Tag);
  LoadStyle;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarRedChange(Sender: TObject);
begin
  UpDownRed.Position   := TrackBarRed.Position;
  UpDownGreen.Position := TrackBarGreen.Position;
  UpDownBlue.Position  := TrackBarBlue.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarHueChange(Sender: TObject);
begin
  UpDownHue.Position := TrackBarHue.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarLightnessChange(Sender: TObject);
begin
  UpDownLight.Position := TrackBarLightness.Position;
  BuildPreview;
end;

procedure TFrmHueSat.TrackBarSaturationChange(Sender: TObject);
begin
  UpDownSat.Position := TrackBarSaturation.Position;
  BuildPreview;
end;

procedure TFrmHueSat.UpDownHueChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarHue.Position := UpDownHue.Position;
  AllowChange:=True;
end;

procedure TFrmHueSat.UpDownLightChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarLightness.Position := UpDownLight.Position;
  AllowChange:=True;
end;

procedure TFrmHueSat.UpDownSatChanging(Sender: TObject;
  var AllowChange: Boolean);
begin
  TrackBarSaturation.Position := UpDownSat.Position;
  AllowChange:=True;
end;

procedure RegisterVCLStyle(const StyleFileName: string);
begin
   if TStyleManager.IsValidStyle(StyleFileName) then
     TStyleManager.LoadFromFile(StyleFileName);
end;

procedure RegisterVCLStyles;
var
  Style     : string;
  StylesDir : string;
begin
  StylesDir:=ExpandFileName(ExtractFilePath(ParamStr(0))  + '\..\Styles');
  if DirectoryExists(StylesDir) then
    for Style in TDirectory.GetFiles(IncludeTrailingPathDelimiter(StylesDir), '*.vsf') do
      RegisterVCLStyle(Style);
end;


initialization
 RegisterVCLStyles;

end.
