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
  private
    OriginalBitMap : TBitmap;
    SepiaBitMap    : TBitmap;
    ModifiedBitMap : TBitmap;
    FStyleName     : string;
    procedure SetRGB(DR, DG, DB: integer);
    procedure Saturation(Value: integer);
    procedure Lightness(Value: integer);
    procedure Hue(Value: integer);
    procedure DrawSeletedVCLStyle;
    function GetStyleName: string;
    property StyleName: string Read GetStyleName Write FStyleName;
    procedure LoadStyle;
  end;

var
  FrmHueSat : TFrmHueSat;

implementation

{$R *.dfm}

uses
  Vcl.Styles.Ext,
  Vcl.Styles.Utils,
  Vcl.Themes,
  Vcl.Styles;


procedure TFrmHueSat.ActionApplyStyleExecute(Sender: TObject);
begin
 TStyleManager.SetStyle(StyleName);
end;

procedure TFrmHueSat.ActionApplyStyleUpdate(Sender: TObject);
begin
 TCustomAction(Sender).Enabled:=not SameText(StyleName, TStyleManager.ActiveStyle.Name, loUserLocale);
end;

procedure TFrmHueSat.BtnApplyClick(Sender: TObject);
Var
  LFilters : TObjectList<TBitmap32Filter>;
  VclUtils : TVclStylesUtils;
begin
  try
    LFilters:=TObjectList<TBitmap32Filter>.Create;

    If CheckBoxSepia.Checked then
      LFilters.Add(TBitmap32SepiaFilter.Create(20));

    If UpDownHue.Position<>0 then
      LFilters.Add(TBitmap32HueFilter.Create(Trunc(UpDownHue.Position)));

    If UpDownSat.Position<>0 then
      LFilters.Add(TBitmap32SaturationFilter.Create(Trunc(UpDownSat.Position)));

    If UpDownLight.Position<>0 then
      LFilters.Add(TBitmap32LightnessFilter.Create(Trunc(UpDownLight.Position)));

    If UpDownRed.Position>0 then
      LFilters.Add(TBitmap32RedFilter.Create(Trunc(UpDownRed.Position)));

    If UpDownGreen.Position>0 then
      LFilters.Add(TBitmap32GreenFilter.Create(Trunc(UpDownGreen.Position)));

    If UpDownBlue.Position>0 then
      LFilters.Add(TBitmap32BlueFilter.Create(Trunc(UpDownBlue.Position)));

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
begin
 if SaveDialog1.Execute then
 begin
    try
      LFilters:=TObjectList<TBitmap32Filter>.Create;

      If CheckBoxSepia.Checked then
        LFilters.Add(TBitmap32SepiaFilter.Create(20));

      If UpDownHue.Position<>0 then
        LFilters.Add(TBitmap32HueFilter.Create(Trunc(UpDownHue.Position)));

      If UpDownSat.Position<>0 then
        LFilters.Add(TBitmap32SaturationFilter.Create(Trunc(UpDownSat.Position)));

      If UpDownLight.Position<>0 then
        LFilters.Add(TBitmap32LightnessFilter.Create(Trunc(UpDownLight.Position)));

      VclUtils:=TVclStylesUtils.Create(StyleName);
      try
        VclUtils.SetFilters(LFilters);
        VclUtils.SaveToFile(SaveDialog1.FileName);
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
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Assign(OriginalBitMap);

    if CheckBoxSepia.Checked then
     _Sepia32(Bitmap,20);

    ImageVCLStyle.Picture.Assign(Bitmap);
    SepiaBitMap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;

  if UpDownHue.Position<>0 then
    Hue(Trunc(UpDownHue.Position));

  if UpDownSat.Position <> 0 then
    Saturation(Trunc(UpDownSat.Position));

  if UpDownLight.Position <> 0 then
    Lightness(Trunc(UpDownLight.Position));

end;

procedure TFrmHueSat.ComboBoxVclStylesChange(Sender: TObject);
begin
  LoadStyle;
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


procedure TFrmHueSat.FormCreate(Sender: TObject);
var
  s : string;
begin
 ReportMemoryLeaksOnShutdown:=True;

 for s in TStyleManager.StyleNames do
  if CompareText(s,'Windows')<>0 then
   ComboBoxVclStyles.Items.Add(s);

  ComboBoxVclStyles.ItemIndex:=0;

  OriginalBitMap := TBitmap.Create;
  ModifiedBitMap := TBitmap.Create;
  SepiaBitMap    := TBitmap.Create;
  LoadStyle;
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

procedure TFrmHueSat.Hue(Value: integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    if CheckBoxSepia.Checked then
     Bitmap.Assign(SepiaBitMap)
    else
     Bitmap.Assign(OriginalBitMap);

    if Value >= 0 then
      _Hue32(Bitmap, Value)
    else
    if Value < 0 then
      _Hue32(Bitmap, 360 - Abs(Value));

    ImageVCLStyle.Picture.Assign(Bitmap);
    ModifiedBitMap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
end;

procedure TFrmHueSat.Lightness(Value: integer);
var
  Bitmap: TBitmap;
  BackUp: TBitmap;
begin
  Bitmap := TBitmap.Create;
  BackUp := TBitmap.Create;
  try
    Bitmap.Assign(ModifiedBitMap);

    if Value >= 0 then
      _Lightness32(Bitmap, Value)
    else
      _Darkness32(Bitmap, Abs(Value));

    BackUp.Assign(OriginalBitMap);
    ImageVCLStyle.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
    BackUp.Free;
  end;
end;

procedure TFrmHueSat.Saturation(Value: integer);
var
  Bitmap: TBitmap;
  BackUp: TBitmap;
begin
  Bitmap := TBitmap.Create;
  BackUp := TBitmap.Create;
  try
    Bitmap.Assign(ModifiedBitMap);
    _Saturation32(Bitmap, (255 - ((Value * 255) div MaxSat)));
    BackUp.Assign(OriginalBitMap);
    ImageVCLStyle.Picture.Assign(Bitmap);
  finally
    Bitmap.Free;
    BackUp.Free;
  end;
end;


procedure TFrmHueSat.SetRGB(DR, DG, DB: integer);
var
  Bitmap: TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    if CheckBoxSepia.Checked then
     Bitmap.Assign(SepiaBitMap)
    else
     Bitmap.Assign(OriginalBitMap);

    _SetRGB32(Bitmap, DR, DG, DB);



    ImageVCLStyle.Picture.Assign(Bitmap);
    ModifiedBitMap.Assign(Bitmap);
  finally
    Bitmap.Free;
  end;
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

procedure TFrmHueSat.TrackBarRedChange(Sender: TObject);
begin
  UpDownRed.Position   := TrackBarRed.Position;
  UpDownGreen.Position := TrackBarGreen.Position;
  UpDownBlue.Position  := TrackBarBlue.Position;
  SetRGB(Trunc(UpDownRed.Position),Trunc(UpDownGreen.Position),Trunc(UpDownBlue.Position));
end;

procedure TFrmHueSat.TrackBarHueChange(Sender: TObject);
begin
  UpDownHue.Position := TrackBarHue.Position;
  Hue(Trunc(UpDownHue.Position));

  if UpDownSat.Position <> 0 then
    Saturation(Trunc(UpDownSat.Position));

  if UpDownLight.Position <> 0 then
    Lightness(Trunc(UpDownLight.Position));
end;

procedure TFrmHueSat.TrackBarLightnessChange(Sender: TObject);
begin
  UpDownLight.Position := TrackBarLightness.Position;
  Lightness(Trunc(UpDownLight.Position));
end;

procedure TFrmHueSat.TrackBarSaturationChange(Sender: TObject);
begin
  UpDownSat.Position := TrackBarSaturation.Position;
  Saturation(Trunc(UpDownSat.Position));
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

end.
