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

  MaxSat = 255;
  MinSat = 0;
  DefSat = 0;

  MaxLig = 255;
  MinLig = -255;
  DefLig = 0;


function  _HSLtoRGB(HueValue, SaturationValue, LightValue: Double): TColor;
procedure _RGBtoHSL(RGB: TColor; var HueValue, SaturationValue, LightValue: Double);


procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Hue24(var ABitMap: TBitmap; Value: integer);
procedure _Hue32(const ABitMap: TBitmap; Value: integer);

procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Sepia24(const ABitMap: TBitmap;Value : Byte=32);
procedure _Sepia32(const ABitMap: TBitmap;Value : Byte=32);

procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);


procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
procedure _Lightness32(const ABitMap: TBitmap; Value: integer);


procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
procedure _Darkness32(const ABitMap: TBitmap; Value: integer);

procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
procedure _Saturation32(const ABitMap: TBitmap; Value: integer);


procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);

procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);


procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);

procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);


Type
  TColorFilter=class
  private
   FValue : Integer;
  public
   constructor Create(AValue:Integer);
   property Value : Integer read FValue Write FValue;
   function ProcessColor(AColor: TColor):TColor;virtual;abstract;
  end;

  TBitmapFilter=class(TColorFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);virtual;abstract;
  end;

  TBitmap32HueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SaturationFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32LightnessFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32SepiaFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32RedFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32GreenFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlueFilter=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendBurn=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendMultiply=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;


  TBitmap32BlendAdditive=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDodge=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendOverlay=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDifference=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendLighten=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendDarken=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
  end;

  TBitmap32BlendScreen=class(TBitmapFilter)
  public
   procedure ProcessBitmap(ABitMap: TBitmap);override;
   function  ProcessColor(AColor: TColor):TColor;override;
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

  TFilterCallback  = procedure (const AColor: TColor;Value: Integer; out NewColor:TColor);



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


procedure _ProcessBitmap32(const ABitMap: TBitmap;Value: Integer;_Process:TFilterCallback);
var
  r, g, b, a   : byte;
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

      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);

      PRGBArray32(Line)[x].R := r;
      PRGBArray32(Line)[x].G := g;
      PRGBArray32(Line)[x].B := b;
      PRGBArray32(Line)[x].A := a;
    end;
    Inc(Line, Delta);
  end;
end;


procedure _ProcessBitmap24(const ABitMap: TBitmap;Value: Integer;_Process:TFilterCallback);
var
  r, g, b    : byte;
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
      r    := PRGBArray24(Line)[x].R;
      g    := PRGBArray24(Line)[x].G;
      b    := PRGBArray24(Line)[x].B;

      _Process(RGB(r,g,b), Value, ARGB);
      GetRGB(ARGB, r, g, b);

      PRGBArray24(Line)[x].R := r;
      PRGBArray24(Line)[x].G := g;
      PRGBArray24(Line)[x].B := b;
    end;
    Inc(Line, Delta);
  end;
end;

procedure _Sepia(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB:=(r+g+b) div 3;

  r:=ARGB+(Value*2);
  g:=ARGB+(Value*1);
  b:=ARGB+(Value*1);

  if r <= ((Value*2)-1) then
    r:=255;
  if g <= (Value-1) then
    g:=255;

  NewColor:= RGB(r, g, b);
end;


procedure _Sepia24(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap24(ABitMap, Value, _Sepia);
end;


procedure _Sepia32(const ABitMap: TBitmap;Value : Byte);
begin
  _ProcessBitmap32(ABitMap, Value, _Sepia);
end;

procedure _Hue(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  H, S, L      : double;
begin
  _RGBtoHSL(AColor, H, S, L);
  H    := H + Value / 360;
  ARGB := _HSLtoRGB(H, S, L);
  NewColor:= ARGB;
end;


procedure _Hue24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Hue);
end;

procedure _Hue32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Hue);
end;


{
if b = 0 then
  result := 0
else begin
  c := 255 - (((255-a) SHL 8) DIV b);
  if c < 0 then result := 0 else result := c;
end;
}

procedure _BlendBurn(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  ARGB         : TColor;
  r, g, b      : byte;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
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

  NewColor:=RGB(r, g, b);
end;

procedure _BlendBurn24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendBurn);
end;


procedure _BlendBurn32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendBurn);
end;


{result := (a*b) SHR 8;}

procedure _BlendMultiply(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  ARGB := Value;
  GetRGB(AColor, r,g, b);
  GetRGB(ARGB  , br,bg, bb);
  r:=(r*br) shr 8;
  g:=(g*bg) shr 8;
  b:=(b*bb) shr 8;
  NewColor:= RGB(r,g,b);
end;


procedure _BlendMultiply24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendMultiply);
end;

procedure _BlendMultiply32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendMultiply);
end;


{
c := a+b;
if c > 255 then result := 255 else result := c;
}
procedure _BlendAdditive(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  ARGB := Value;
  GetRGB(AColor, r,g, b);
  GetRGB(ARGB, br,bg, bb);

  c:=RoundIntToByte(r+br);
  r:=c;
  c:=RoundIntToByte(g+bg);
  g:=c;
  c:=RoundIntToByte(b+bb);
  b:=c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendAdditive24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendAdditive);
end;


procedure _BlendAdditive32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendAdditive);
end;

{
if b = 255 then
  result := 255
else begin
  c := (a SHL 8) DIV (255-b);
  if c > 255 then result := 255 else result := c;
end;
}
procedure _BlendDodge(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

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

  NewColor:= RGB(r,g,b);
end;

procedure _BlendDodge24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDodge);
end;


procedure _BlendDodge32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDodge);
end;

{
if a < 128 then
  result := (a*b) SHR 7
else
  result := 255 - ((255-a) * (255-b) SHR 7);
}
procedure _BlendOverlay(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);
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

  NewColor:= RGB(r,g,b);
end;

procedure _BlendOverlay24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendOverlay);
end;


procedure _BlendOverlay32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendOverlay);
end;

{
result := abs(a-b);
}

procedure _BlendDifference(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=abs(r-br);
  g:=abs(g-bg);
  b:=abs(b-bb);
  NewColor:= RGB(r,g,b);
end;


procedure _BlendDifference24(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap24(ABitMap, Value, _BlendDifference);
end;

procedure _BlendDifference32(const ABitMap: TBitmap;Value: Integer);
begin
  _ProcessBitmap32(ABitMap, Value, _BlendDifference);
end;

{
if a > b then
  result := a
else
  result := b;
}
procedure _BlendLighten(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  r:=IfThen(r>br, r, br);
  g:=IfThen(g>bg, g, bg);
  b:=IfThen(b>bb, b, bb);

  NewColor:= RGB(r,g,b);
end;

procedure _BlendLighten24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendLighten);
end;

procedure _BlendLighten32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendLighten);
end;

{
if a < b then
  result := a
else
  result := b;
}
procedure _BlendDarken(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
begin
  GetRGB(AColor, r,g, b);
  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);
  r:=IfThen(r<br, r, br);
  g:=IfThen(g<bg, g, bg);
  b:=IfThen(b<bb, b, bb);
  NewColor:= RGB(r,g,b);
end;

procedure _BlendDarken24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendDarken);
end;

procedure _BlendDarken32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendDarken);
end;

{
result := 255 - ((255-a) * (255-b) SHR 8);
}
procedure _BlendScreen(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  ARGB         : TColor;
  br, bg, bb   : byte;
  c            : Integer;
begin
  GetRGB(AColor, r,g, b);

  ARGB := Value;
  GetRGB(ARGB, br,bg, bb);

  c := RoundIntToByte(255 - ((255-r) * (255-br) SHR 8));
  r := c;

  c := RoundIntToByte(255 - ((255-g) * (255-bg) SHR 8));
  g := c;

  c := RoundIntToByte(255 - ((255-b) * (255-bb) SHR 8));
  b := c;

  NewColor:= RGB(r,g,b);
end;

procedure _BlendScreen24(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap24(ABitMap, Value, _BlendScreen);
end;


procedure _BlendScreen32(const ABitMap: TBitmap;Value: Integer);
begin
 _ProcessBitmap32(ABitMap, Value, _BlendScreen);
end;


procedure _SetRComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r:=RoundIntToByte(r+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetGComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  g:=RoundIntToByte(g+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetBComponent(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  b:=RoundIntToByte(b+Value);
  NewColor:= RGB(r,g,b);
end;

procedure _SetRGB24(const ABitMap: TBitmap; DR,DG,DB: Integer);
var
  r, g, b : byte;
  x, y:    integer;
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
      PRGBArray24(Line)[x].R := RoundIntToByte(r+DR);
      PRGBArray24(Line)[x].G := RoundIntToByte(g+DG);
      PRGBArray24(Line)[x].B := RoundIntToByte(b+DB);
    end;
    Inc(Line, Delta);
  end;
end;


procedure _SetRGB32(const ABitMap: TBitmap; DR,DG,DB: Integer);
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


procedure _Saturation(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
  Gray         : Integer;
begin
  GetRGB(AColor, r,g, b);
  Gray := (r + g + b) div 3;
  r := RoundIntToByte(Gray + (((r - Gray) * Value) div 255));
  g := RoundIntToByte(Gray + (((g - Gray) * Value) div 255));
  b := RoundIntToByte(Gray + (((b - Gray) * Value) div 255));
  NewColor:= RGB(r,g,b);
end;


procedure _Saturation24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Saturation);
end;

procedure _Saturation32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Saturation);
end;


procedure _Lightness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r + ((255 - r) * Value) div 255);
  g := RoundIntToByte(g + ((255 - g) * Value) div 255);
  b := RoundIntToByte(b + ((255 - b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;

procedure _Lightness24(var ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap24(ABitMap, Value, _Lightness);
end;

procedure _Lightness32(const ABitMap: TBitmap; Value: integer);
begin
 _ProcessBitmap32(ABitMap, Value, _Lightness);
end;

procedure _Darkness(const AColor: TColor;Value: Integer; out NewColor:TColor);
var
  r, g, b      : byte;
begin
  GetRGB(AColor, r,g, b);
  r := RoundIntToByte(r - ((r) * Value) div 255);
  g := RoundIntToByte(g - ((g) * Value) div 255);
  b := RoundIntToByte(b - ((b) * Value) div 255);
  NewColor:= RGB(r,g,b);
end;


procedure _Darkness24(var ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap24(ABitMap, Value, _Darkness);
end;

procedure _Darkness32(const ABitMap: TBitmap; Value: integer);
begin
  _ProcessBitmap32(ABitMap, Value, _Darkness);
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
procedure TBitmap32HueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _Hue)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _Hue);
end;

function TBitmap32HueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Hue(AColor, Value, Result);
end;

{ TBitmap32SaturationFilter }

procedure TBitmap32SaturationFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _ProcessBitmap32(ABitMap, Value, _Saturation)
  else
  if ABitMap.PixelFormat=pf24bit then
    _ProcessBitmap24(ABitMap, Value, _Saturation);
end;

function TBitmap32SaturationFilter.ProcessColor(AColor: TColor): TColor;
begin
  _Saturation(AColor, Value, Result);
end;

{ TBitmap32LightnessFilter }

procedure TBitmap32LightnessFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
  begin
    if Value >= 0 then
      _ProcessBitmap32(ABitMap, Value, _Lightness)
    else
      _ProcessBitmap32(ABitMap, Abs(Value), _Darkness);
  end
  else
  if ABitMap.PixelFormat=pf24bit then
  begin
    if Value >= 0 then
      _ProcessBitmap24(ABitMap, Value, _Lightness)
    else
      _ProcessBitmap24(ABitMap, Abs(Value), _Darkness);
  end;
end;

function TBitmap32LightnessFilter.ProcessColor(AColor: TColor): TColor;
begin
    if Value >= 0 then
      _Lightness(AColor, Value, Result)
    else
      _Darkness(AColor, Abs(Value), Result);
end;

{ TBitmap32SepiaFilter }

procedure TBitmap32SepiaFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _Sepia)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _Sepia);
end;

function TBitmap32SepiaFilter.ProcessColor(AColor: TColor): TColor;
begin
   _Sepia(AColor, Value, Result);
end;

{ TColorFilter }

constructor TColorFilter.Create(AValue: Integer);
begin
 inherited Create;
 FValue:=AValue;
end;


{ TBitmap32BlueFilter }

procedure TBitmap32BlueFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,0,Value)
  else
  if ABitMap.PixelFormat=pf24bit then
   _SetRGB24(ABitMap,0,0,Value);
end;

function TBitmap32BlueFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetBComponent(AColor, Value, Result);
end;

{ TBitmap32RedFilter }

procedure TBitmap32RedFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,Value,0,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,Value,0,0);
end;

function TBitmap32RedFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetRComponent(AColor, Value, Result);
end;

{ TBitmap32GreenFilter }

procedure TBitmap32GreenFilter.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
    _SetRGB32(ABitMap,0,Value,0)
  else
  if ABitMap.PixelFormat=pf24bit then
    _SetRGB24(ABitMap,0,Value,0);
end;

function TBitmap32GreenFilter.ProcessColor(AColor: TColor): TColor;
begin
  _SetGComponent(AColor, Value, Result);
end;

{ TBitmap32BlendBurn }

procedure TBitmap32BlendBurn.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendBurn)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendBurn);
end;

function TBitmap32BlendBurn.ProcessColor(AColor: TColor): TColor;
begin
  _BlendBurn(AColor, Value, Result);
end;

{ TBitmap32BlendMultiply }

procedure TBitmap32BlendMultiply.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendMultiply)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendMultiply);
end;

function TBitmap32BlendMultiply.ProcessColor(AColor: TColor): TColor;
begin
  _BlendMultiply(AColor, Value, Result);
end;

{ TBitmap32BlendAdditive }

procedure TBitmap32BlendAdditive.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendAdditive)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendAdditive);
end;

function TBitmap32BlendAdditive.ProcessColor(AColor: TColor): TColor;
begin
  _BlendAdditive(AColor, Value, Result);
end;

{ TBitmap32BlendDodge }

procedure TBitmap32BlendDodge.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendDodge)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendDodge);
end;

function TBitmap32BlendDodge.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDodge(AColor, Value, Result);
end;

{ TBitmap32BlendOverlay }

procedure TBitmap32BlendOverlay.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendOverlay)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendOverlay);
end;

function TBitmap32BlendOverlay.ProcessColor(AColor: TColor): TColor;
begin
  _BlendOverlay(AColor, Value, Result);
end;

{ TBitmap32BlendLighten }

procedure TBitmap32BlendLighten.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendLighten)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendLighten);
end;

function TBitmap32BlendLighten.ProcessColor(AColor: TColor): TColor;
begin
  _BlendLighten(AColor, Value, Result);
end;

{ TBitmap32BlendDarken }

procedure TBitmap32BlendDarken.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendDarken)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendDarken);
end;

function TBitmap32BlendDarken.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDarken(AColor, Value, Result);
end;

{ TBitmap32BlendScreen }

procedure TBitmap32BlendScreen.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendScreen)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendScreen);
end;

function TBitmap32BlendScreen.ProcessColor(AColor: TColor): TColor;
begin
  _BlendScreen(AColor, Value, Result);
end;

{ TBitmap32BlendDifference }

procedure TBitmap32BlendDifference.ProcessBitmap(ABitMap: TBitmap);
begin
  if ABitMap.PixelFormat=pf32bit then
   _ProcessBitmap32(ABitMap, Value, _BlendDifference)
  else
  if ABitMap.PixelFormat=pf24bit then
   _ProcessBitmap24(ABitMap, Value, _BlendDifference);
end;

function TBitmap32BlendDifference.ProcessColor(AColor: TColor): TColor;
begin
  _BlendDifference(AColor, Value, Result);
end;

end.
