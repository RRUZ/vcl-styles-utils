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
  Vcl.Styles,
  Vcl.Themes,
  Vcl.Styles.Ext,
  Generics.Collections;

type
  TVclStylesUtils = class
  private
     FStyleExt  : TCustomStyleExt;
     FSourceInfo: TSourceInfo;
  public
     procedure SetFilters(Filters : TObjectList<TBitmap32Filter>);
     procedure ApplyChanges;
     procedure SaveToFile(const FileName: string);
     property  SourceInfo: TSourceInfo read FSourceInfo;
     property  StyleExt  :  TCustomStyleExt read FStyleExt;
     constructor Create(const  StyleName : string);
     destructor Destroy;override;
  end;


implementation

uses
  IOUtils,
  SysUtils,
  Graphics,
  Classes;


{
procedure ApplyFiltersToVclStyle(const  StyleName, FileName: string; Filters : TObjectList<TBitmap32Filter>;Apply:Boolean);
var
  LBitmap   : TBitmap;
  LStyle    : TCustomStyleExt;
  SourceInfo: TSourceInfo;
  BitmapList: TArray<TBitmap>;
  Index     : Integer;
  FileStream: TFileStream;
  Filter    : TBitmap32Filter;

begin
   if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
   begin
    try
       SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
       LStyle:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
       try
         BitmapList:=LStyle.BitmapList;
         Index:=0;
         for LBitmap in BitmapList do
         begin

           for Filter in Filters do
             Filter.Apply(LBitmap);

            LStyle.ReplaceBitmap(Index, LBitmap);
            Inc(Index);
         end;

         //Copy changes to current style
         if Apply then
         begin
           TStream(SourceInfo.Data).Size:=0;
           LStyle.CopyToStream(TStream(SourceInfo.Data));
           TStream(SourceInfo.Data).Seek(0,soFromBeginning);
         end;

         //Save To file
         if FileName<>'' then
         begin
           FileStream:=TFile.Create(FileName);
           try
             LStyle.CopyToStream(FileStream);
           finally
             FileStream.Free;
           end;
         end;

       finally
         LStyle.Free;
       end;
    finally
      LBitmap.Free;
    end;
   end;
end;
    }

{ TVclStylesUtils }

constructor TVclStylesUtils.Create(const  StyleName : string);
begin
  FStyleExt:=nil;
  if (StyleName<>'') and (CompareText('Windows',StyleName)<>0) then
  begin
   FSourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
   FStyleExt:=TCustomStyleExt.Create(TStream(SourceInfo.Data));
  end;
end;


destructor TVclStylesUtils.Destroy;
begin
  if Assigned(StyleExt) then
    StyleExt.Free;
  inherited;
end;

procedure TVclStylesUtils.ApplyChanges;
begin
  if Assigned(StyleExt) then
  begin
    TStream(FSourceInfo.Data).Size:=0;
    StyleExt.CopyToStream(TStream(SourceInfo.Data));
    TStream(SourceInfo.Data).Seek(0,soFromBeginning);
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

procedure TVclStylesUtils.SetFilters(Filters: TObjectList<TBitmap32Filter>);
var
  LBitmap   : TBitmap;
  BitmapList: TObjectList<TBitmap>;
  Index     : Integer;
  Filter    : TBitmap32Filter;
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

end.
