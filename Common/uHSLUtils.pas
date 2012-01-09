{**************************************************************************************************}
{                                                                                                  }
{ Unit uHSLUtils                                                                                   }
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
{ The Original Code is uHSLUtils.pas.                                                              }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}


unit uHSLUtils;

interface

uses
  Windows,
  Graphics,
  Classes,
  SysUtils;

type
  PRGB24     = ^TRGB24;

  TRGB24 = record
    B, G, R: byte;
  end;
  PRGBArray24 = ^TRGBArray24;
  TRGBArray24 = array[0..0] of TRGB24;

  PRGB32     = ^TRGB32;
  TRGB32 = record
    B, G, R, A: byte;
  end;
  PRGBArray32 = ^TRGBArray32;
  TRGBArray32 = array[0..0] of TRGB32;

const
  MaxHue = 180;
  MinHue = -180;
  DefHue = 0;

  MaxSat = 100;
  MinSat = 0;
  DefSat = 0;

  MaxLig = 255;
  MinLig = -255;
  DefLig = 0;


function  _HSLtoRGB(HueValue, SaturationValue, LightValue: Double): TColor;
procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: Double);

procedure _Hue24(var ABitMap: TBitmap; Value: integer);
procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
procedure _Hue32(const ABitMap: TBitmap; Value: integer);
procedure _Lightness32(const ABitMap: TBitmap; Value: integer);
procedure _Darkness32(const ABitMap: TBitmap; Value: integer);
procedure _Saturation32(const ABitMap: TBitmap; Value: integer);
procedure _Sepia32(const ABitMap: TBitmap;Value : Byte=32);


Type
  TBitmap32Filter=class
  private
   FValue : Integer;
  public
   constructor Create(AValue:Integer);
   procedure Apply(ABitMap: TBitmap);virtual;abstract;
   property Value : Integer read FValue Write FValue;
  end;

  TBitmap32HueFilter=class(TBitmap32Filter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32SaturationFilter=class(TBitmap32Filter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32LightnessFilter=class(TBitmap32Filter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32SepiaFilter=class(TBitmap32Filter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

implementation

function RoundIntToByte(i: integer): byte;
begin
  if i > 255 then Result := 255
  else
  if i < 0   then Result := 0
  else
    Result := i;
end;


procedure GetRGB(Col: TColor; var R, G, B: byte);
var
  Color: $0..$FFFFFFFF;
begin
  Color := ColorToRGB(Col);
  R     := ($000000FF and Color);
  G     := ($0000FF00 and Color) shr 8;
  B     := ($00FF0000 and Color) shr 16;
end;


procedure _Sepia32(const ABitMap: TBitmap;Value : Byte);
var
  r, g, b, a: byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
  H, S, L: double;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray32(Line)[x].R;
      g    := PRGBArray32(Line)[x].G;
      b    := PRGBArray32(Line)[x].B;
      a    := PRGBArray32(Line)[x].A;

      ARGB:=(r+g+b) div 3;
      //then convert it to sepia
      r:=ARGB+(Value*2);
      g:=ARGB+Value;
      b:=ARGB;

      if r <= ((Value*2)-1) then
        r:=255;
      if g <= (Value-1) then
        g:=255;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;
{
var
    gcolor:integer;//greyscale color
    r,g,b:byte;
    h,w:integer;
    RowOriginal :  PRGBArray32;
begin
  //modify
  for h := 0 to bmp.height-1 do
  begin
    RowOriginal  := pRGBArray(bmp.Scanline[h]);
    for w := 0 to bmp.width-1 do
    begin
      //get greyscale
      r:=RowOriginal[w].rgbtRed;
      g:=RowOriginal[w].rgbtGreen;
      b:=RowOriginal[w].rgbtBlue;
      gcolor:=(r+g+b) div 3;
      //then convert it to sepia
      r:=gcolor+(depth*2);
      g:=gcolor+depth;
      b:=gcolor;
      if r <= ((depth*2)-1) then
        r:=255;
      if g <= (depth-1) then
        g:=255;
      //output
      RowOriginal[w].rgbtRed:=r;
      RowOriginal[w].rgbtGreen:=g;
      RowOriginal[w].rgbtBlue:=b;
    end;
  end;
end;
  }

procedure _Hue24(var ABitMap: TBitmap; Value: integer);
var
  r, g, b: byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
  H, S, L: double;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray24(Line)[x].R;
      g    := PRGBArray24(Line)[x].G;
      b    := PRGBArray24(Line)[x].B;
      ARGB := RGB(r, g, b);
      _RGBtoHSL(ARGB, H, S, L);
      H    := H + Value / 360;
      ARGB := _HSLtoRGB(H, S, L);
      GetRGB(ARGB, R, G, B);
      PRGBArray24(Line)[x].R := r;
      PRGBArray24(Line)[x].G := g;
      PRGBArray24(Line)[x].B := b;
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Hue32(const ABitMap: TBitmap; Value: integer);
var
  r, g, b, a: byte;
  x, y:    integer;
  ARGB:    TColor;
  Line, Delta: integer;
  H, S, L: double;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray32(Line)[x].R;
      g    := PRGBArray32(Line)[x].G;
      b    := PRGBArray32(Line)[x].B;
      a    := PRGBArray32(Line)[x].A;

      ARGB := RGB(r, g, b);
      _RGBtoHSL(ARGB, H, S, L);
      H    := H + Value / 360;
      ARGB := _HSLtoRGB(H, S, L);
      GetRGB(ARGB, R, G, B);

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;


procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
var
  Gray, r, g, b, x, y: integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray24(Line)[x].R;
      g    := PRGBArray24(Line)[x].G;
      b    := PRGBArray24(Line)[x].B;
      Gray := (r + g + b) div 3;
      PRGBArray24(Line)[x].R := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
      PRGBArray24(Line)[x].G := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
      PRGBArray24(Line)[x].B := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Saturation32(const ABitMap: TBitmap; Value: integer);
var
  Gray, r, g, b, a, x, y: integer;
  Line, Delta: integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r    := PRGBArray32(Line)[x].R;
      g    := PRGBArray32(Line)[x].G;
      b    := PRGBArray32(Line)[x].B;
      a    := PRGBArray32(Line)[x].a;
      Gray := (r + g + b) div 3;
      PRGBArray32(Line)[x].R := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
      PRGBArray32(Line)[x].G := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
      PRGBArray32(Line)[x].B := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;



procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
var
  r, g, b, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray24(Line)[x].R;
      g := PRGBArray24(Line)[x].G;
      b := PRGBArray24(Line)[x].B;
      PRGBArray24(Line)[x].R := RoundIntToByte(r + ((255 - r) * Value) div 255);
      PRGBArray24(Line)[x].G := RoundIntToByte(g + ((255 - g) * Value) div 255);
      PRGBArray24(Line)[x].B := RoundIntToByte(b + ((255 - b) * Value) div 255);
    end;
    Inc(Line, Delta);
  end;
end;


procedure _Lightness32(const ABitMap: TBitmap; Value: integer);
var
  r, g, b, a, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray32(Line)[x].R;
      g := PRGBArray32(Line)[x].G;
      b := PRGBArray32(Line)[x].B;
      a := PRGBArray32(Line)[x].A;
      PRGBArray32(Line)[x].R := RoundIntToByte(r + ((255 - r) * Value) div 255);
      PRGBArray32(Line)[x].G := RoundIntToByte(g + ((255 - g) * Value) div 255);
      PRGBArray32(Line)[x].B := RoundIntToByte(b + ((255 - b) * Value) div 255);
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;




procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
var
  r, g, b, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray24(Line)[x].R;
      g := PRGBArray24(Line)[x].G;
      b := PRGBArray24(Line)[x].B;
      PRGBArray24(Line)[x].R := RoundIntToByte(r - ((r) * Value) div 255);
      PRGBArray24(Line)[x].G := RoundIntToByte(g - ((g) * Value) div 255);
      PRGBArray24(Line)[x].B := RoundIntToByte(b - ((b) * Value) div 255);
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Darkness32(const ABitMap: TBitmap; Value: integer);
var
  r, g, b, a, x, y: integer;
  Line, Delta:   integer;
begin
  Line  := integer(ABitMap.ScanLine[0]);
  Delta := integer(ABitMap.ScanLine[1]) - Line;
  for y := 0 to ABitMap.Height - 1 do
  begin
    for x := 0 to ABitMap.Width - 1 do
    begin
      r := PRGBArray32(Line)[x].R;
      g := PRGBArray32(Line)[x].G;
      b := PRGBArray32(Line)[x].B;
      a := PRGBArray32(Line)[x].A;
      PRGBArray32(Line)[x].R := RoundIntToByte(r - ((r) * Value) div 255);
      PRGBArray32(Line)[x].G := RoundIntToByte(g - ((g) * Value) div 255);
      PRGBArray32(Line)[x].B := RoundIntToByte(b - ((b) * Value) div 255);
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;



function _HSLtoRGB(HueValue, SaturationValue, LightValue: double): TColor;
var
  M1, M2: double;

  function HueToColourValue(Hue: double): byte;
  var
    V: double;
  begin
    if Hue < 0 then
      Hue := Hue + 1
    else
    if Hue > 1 then
      Hue := Hue - 1;

    if 6 * Hue < 1 then
      V := M1 + (M2 - M1) * Hue * 6
    else
    if 2 * Hue < 1 then
      V := M2
    else
    if 3 * Hue < 2 then
      V := M1 + (M2 - M1) * (2 / 3 - Hue) * 6
    else
      V := M1;
    Result := round(255 * V);
  end;

var
  R, G, B: byte;
begin
  if SaturationValue = 0 then
  begin
    R := round(255 * LightValue);
    G := R;
    B := R;
  end
  else
  begin
    if LightValue <= 0.5 then
      M2 := LightValue * (1 + SaturationValue)
    else
      M2 := LightValue + SaturationValue - LightValue * SaturationValue;
    M1 := 2 * LightValue - M2;
    R := HueToColourValue(HueValue + 1 / 3);
    G := HueToColourValue(HueValue);
    B := HueToColourValue(HueValue - 1 / 3);
  end;

  Result := RGB(R, G, B);
end;

procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: double);

  function Max(a, b: double): double;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

  function Min(a, b: double): double;
  begin
    if a < b then
      Result := a
    else
      Result := b;
  end;

var
  R, G, B, D, Cmax, Cmin: double;
begin
  R    := GetRValue(RGB) / 255;
  G    := GetGValue(RGB) / 255;
  B    := GetBValue(RGB) / 255;
  Cmax := Max(R, Max(G, B));
  Cmin := Min(R, Min(G, B));

  LightValue := (Cmax + Cmin) / 2;

  if Cmax = Cmin then
  begin
    HueValue := 0;
    SaturationValue := 0;
  end
  else
  begin
    D := Cmax - Cmin;

    if LightValue < 0.5 then
      SaturationValue := D / (Cmax + Cmin)
    else
      SaturationValue := D / (2 - Cmax - Cmin);

    if R = Cmax then
      HueValue := (G - B) / D
    else
    if G = Cmax then
      HueValue := 2 + (B - R) / D
    else
      HueValue := 4 + (R - G) / D;

    HueValue := HueValue / 6;
    if HueValue < 0 then
      HueValue := HueValue + 1;
  end;
end;

{ TBitmap32Filter }


{ TBitmap32HueFilter }
procedure TBitmap32HueFilter.Apply(ABitMap: TBitmap);
begin
  _Hue32(ABitMap, Value);
end;

{ TBitmap32SaturationFilter }

procedure TBitmap32SaturationFilter.Apply(ABitMap: TBitmap);
begin
  _Saturation32(ABitMap, Value);
end;

{ TBitmap32LightnessFilter }

procedure TBitmap32LightnessFilter.Apply(ABitMap: TBitmap);
begin
  if Value >= 0 then
    _Lightness32(ABitMap, Value)
  else
    _Darkness32(ABitMap, Abs(Value));
end;

{ TBitmap32SepiaFilter }

procedure TBitmap32SepiaFilter.Apply(ABitMap: TBitmap);
begin
 _Sepia32(ABitMap, Value);
end;

{ TBitmap32Filter }

constructor TBitmap32Filter.Create(AValue: Integer);
begin
 inherited Create;
 FValue:=AValue;
end;

end.
