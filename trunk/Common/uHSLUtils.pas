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
procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Byte);

procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);
procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);

Type
  TBitmapFilter=class
  private
   FValue : Integer;
  public
   constructor Create(AValue:Integer);
   procedure Apply(ABitMap: TBitmap);virtual;abstract;
   property Value : Integer read FValue Write FValue;
  end;

  TBitmap32HueFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32SaturationFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32LightnessFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32SepiaFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32RedFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32GreenFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlueFilter=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendBurn=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendMultiply=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;


  TBitmap32BlendAdditive=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendDodge=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendOverlay=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendDifference=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendLighten=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendDarken=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;

  TBitmap32BlendScreen=class(TBitmapFilter)
  public
   procedure Apply(ABitMap: TBitmap);override;
  end;


implementation

Uses
  Math;

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

      r:=ARGB+(Value*2);
      g:=ARGB+(Value*1);
      b:=ARGB+(Value*1);

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

{
if b = 0 then
  result := 0
else begin
  c := 255 - (((255-a) SHL 8) DIV b);
  if c < 0 then result := 0 else result := c;
end;
}
procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  c: Integer;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      if br=0 then
       r:=0
      else
      begin
       c:=RoundIntToByte(255-(((255-r) SHL 8) DIV br));
       r:=c;
      end;

      if bg=0 then
       g:=0
      else
      begin
       c:=RoundIntToByte(255-(((255-g) SHL 8) DIV bg));
       g:=c;
      end;

      if bb=0 then
       b:=0
      else
      begin
       c:=RoundIntToByte(255-(((255-b) SHL 8) DIV bb));
       b:=c;
      end;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{result := (a*b) SHR 8;}
procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      r:=(r*br) shr 8;
      g:=(g*bg) shr 8;
      b:=(b*bb) shr 8;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;


{
c := a+b;
if c > 255 then result := 255 else result := c;
}
procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  c: Integer;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      c:=RoundIntToByte(r+br);
      r:=c;
      c:=RoundIntToByte(g+bg);
      g:=c;
      c:=RoundIntToByte(b+bb);
      b:=c;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
if b = 255 then
  result := 255
else begin
  c := (a SHL 8) DIV (255-b);
  if c > 255 then result := 255 else result := c;
end;
}

procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  c: Integer;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      if br=255 then
       r:=255
      else
      begin
        c := RoundIntToByte((r SHL 8) DIV (255-br));
        r := c;
      end;

      if bg=255 then
       g:=255
      else
      begin
        c := RoundIntToByte((g SHL 8) DIV (255-bg));
        g := c;
      end;

      if bb=255 then
       b:=255
      else
      begin
        c := RoundIntToByte((b SHL 8) DIV (255-bb));
        b := c;
      end;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
if a < 128 then
  result := (a*b) SHR 7
else
  result := 255 - ((255-a) * (255-b) SHR 7);
}
procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  c: Integer;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      if r<128 then
       r:=RoundIntToByte((r*br) shr 7)
      else
      begin
        c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 7));
        r := c;
      end;

      if g<128 then
       g:=RoundIntToByte((g*bg) shr 7)
      else
      begin
        c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 7));
        g := c;
      end;

      if b<128 then
       b:=RoundIntToByte((r*bb) shr 7)
      else
      begin
        c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 7));
        b := c;
      end;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
result := abs(a-b);
}
procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      r:=abs(r-br);
      g:=abs(g-bg);
      b:=abs(b-bb);

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
if a > b then
  result := a
else
  result := b;
}
procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      r:=IfThen(r>br, r, br);
      g:=IfThen(g>bg, g, bg);
      b:=IfThen(b>bb, b, bb);

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
if a < b then
  result := a
else
  result := b;
}
procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      r:=IfThen(r<br, r, br);
      g:=IfThen(g<bg, g, bg);
      b:=IfThen(b<bb, b, bb);

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;

{
result := 255 - ((255-a) * (255-b) SHR 8);
}
procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);
var
  r, g, b, a   : byte;
  br, bg, bb   : byte;
  c: Integer;
  x, y:    integer;
  ARGB:    TColor;
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
      a    := PRGBArray32(Line)[x].A;

      ARGB := Value;
      GetRGB(ARGB, br,bg, bb);

      c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 8));
      r := c;

      c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 8));
      g := c;

      c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 8));
      b := c;

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;


procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Byte);
var
  r, g, b, a: byte;
  x, y:    integer;
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
      a    := PRGBArray32(Line)[x].A;
      PRGBArray32(Line)[x].R := RoundIntToByte(r+DR);
      PRGBArray32(Line)[x].G := RoundIntToByte(g+DG);
      PRGBArray32(Line)[x].B := RoundIntToByte(b+DB);
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
  if ABitMap.PixelFormat=pf32bit then
  _Hue32(ABitMap, Value);
end;

{ TBitmap32SaturationFilter }

procedure TBitmap32SaturationFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  _Saturation32(ABitMap, Value);
end;

{ TBitmap32LightnessFilter }

procedure TBitmap32LightnessFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  begin
    if Value >= 0 then
      _Lightness32(ABitMap, Value)
    else
      _Darkness32(ABitMap, Abs(Value));
  end;
end;

{ TBitmap32SepiaFilter }

procedure TBitmap32SepiaFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  _Sepia32(ABitMap, Value);
end;

{ TBitmap32Filter }

constructor TBitmapFilter.Create(AValue: Integer);
begin
 inherited Create;
 FValue:=AValue;
end;

{ TBitmap32BlueFilter }

procedure TBitmap32BlueFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
 _SetRGB32(ABitMap,0,0,Value);
end;

{ TBitmap32RedFilter }

procedure TBitmap32RedFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
 _SetRGB32(ABitMap,Value,0,0);
end;

{ TBitmap32GreenFilter }

procedure TBitmap32GreenFilter.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
 _SetRGB32(ABitMap,0,Value,0);
end;

{ TBitmap32BlendBurn }

procedure TBitmap32BlendBurn.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendBurn32(ABitMap, Value);
end;

{ TBitmap32BlendMultiply }

procedure TBitmap32BlendMultiply.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  _BlendMultiply32(ABitMap, Value);
end;

{ TBitmap32BlendAdditive }

procedure TBitmap32BlendAdditive.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  _BlendAdditive32(ABitMap, Value);
end;

{ TBitmap32BlendDodge }

procedure TBitmap32BlendDodge.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendDodge32(ABitMap, Value);
end;

{ TBitmap32BlendOverlay }

procedure TBitmap32BlendOverlay.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendOverlay32(ABitMap, Value);
end;

{ TBitmap32BlendLighten }

procedure TBitmap32BlendLighten.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendLighten32(ABitMap, Value);
end;

{ TBitmap32BlendDarken }

procedure TBitmap32BlendDarken.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendDarken32(ABitMap, Value);
end;

{ TBitmap32BlendScreen }

procedure TBitmap32BlendScreen.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendScreen32(ABitMap, Value);
end;

{ TBitmap32BlendDifference }

procedure TBitmap32BlendDifference.Apply(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _BlendDifference32(ABitMap, Value);
end;

end.
