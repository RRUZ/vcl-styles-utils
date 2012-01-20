{**************************************************************************************************}
{                                                                                                  }
{ Unit Vcl.Styles.Utils                                                                            }
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
{ The Original Code is Vcl.Styles.Utils.pas.                                                       }
{                                                                                                  }
{ The Initial Developer of the Original Code is Rodrigo Ruz V.                                     }
{ Portions created by Rodrigo Ruz V. are Copyright (C) 2012 Rodrigo Ruz V.                         }
{ All Rights Reserved.                                                                             }
{                                                                                                  }
{**************************************************************************************************}
unit Vcl.Styles.Utils;

interface
uses
  uHSLUtils,
  System.Classes,
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Styles.Ext,
  Generics.Collections;

type
  TVclStylesUtils = class
  private
     FClone     : Boolean;
     FStream    : TStream;
     FStyleExt  : TCustomStyleExt;
     //FSourceInfo: TSourceInfo;
  public
     procedure SetFilters(Filters : TObjectList<TBitmapFilter>);
     procedure ApplyChanges;
     procedure SaveToFile(const FileName: string);
     //property  SourceInfo: TSourceInfo read FSourceInfo;
     property  StyleExt  :  TCustomStyleExt read FStyleExt;
     constructor Create(const  StyleName : string;Clone:Boolean=False);
     destructor Destroy;override;
  end;


implementation

uses
  System.IOUtils,
  System.SysUtils,
  Vcl.Graphics;


{ TVclStylesUtils }

constructor TVclStylesUtils.Create(const  StyleName : string;Clone:Boolean=False);
var
  FSourceInfo: TSourceInfo;
begin
  TStyleManager.StyleNames;//call DiscoverStyleResources
  FClone   :=Clone;
  FStyleExt:=nil;
  FStream  :=nil;
  if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
  begin
   //FSourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
   if FClone then
   begin
     FStream:=TMemoryStream.Create;
     FSourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
     TStream(FSourceInfo.Data).Position:=0;
     FStream.CopyFrom(TStream(FSourceInfo.Data),TStream(FSourceInfo.Data).Size);
     TStream(FSourceInfo.Data).Position:=0;
     FStream.Position:=0;
   end
   else
   FStream:=TStream(TStyleManager.StyleSourceInfo[StyleName].Data);
   FStyleExt:=TCustomStyleExt.Create(FStream);
  end;
end;


destructor TVclStylesUtils.Destroy;
begin
  if Assigned(StyleExt) then
    StyleExt.Free;
  if FClone and Assigned(FStream) then
    FStream.Free;
  inherited;
end;

procedure TVclStylesUtils.ApplyChanges;
begin
  if Assigned(StyleExt) then
  begin
    FStream.Size:=0;
    StyleExt.CopyToStream(FStream);
    FStream.Seek(0,soFromBeginning);
  end;
end;

procedure TVclStylesUtils.SaveToFile(const FileName: string);
var
  FileStream: TFileStream;
begin
   if FileName<>'' then
   begin
     FileStream:=TFile.Create(FileName);
     try
       StyleExt.CopyToStream(FileStream);
     finally
       FileStream.Free;
     end;
   end;
end;

procedure TVclStylesUtils.SetFilters(Filters: TObjectList<TBitmapFilter>);
var
  LBitmap   : TBitmap;
  BitmapList: TObjectList<TBitmap>;
  Index     : Integer;
  Filter    : TBitmapFilter;
begin
   BitmapList:=StyleExt.BitmapList;
   try
     Index:=0;
     for LBitmap in BitmapList do
     begin
       for Filter in Filters do
         Filter.Apply(LBitmap);
        StyleExt.ReplaceBitmap(Index, LBitmap);
        Inc(Index);
     end;
   finally
     BitmapList.Free;
   end;
end;
              {
procedure TVclStylesUtils.SetStyleColor(Color: TStyleColor; NewColor: TColor);
begin

end;

procedure TVclStylesUtils.SetStyleFontColor(Font: TStyleFont; NewColor: TColor);
begin

end;

procedure TVclStylesUtils.SetSystemColor(Color, NewColor: TColor);
begin

end;
           }
end.
