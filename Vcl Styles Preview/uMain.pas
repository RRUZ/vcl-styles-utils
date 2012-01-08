unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
  TFrmMain = class(TForm)
    ListView1: TListView;
    Label1: TLabel;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure ListView1Change(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure FormDestroy(Sender: TObject);
  private
    Loading : Boolean;
    FBitmap : TBitmap;
    FStylesPath : string;
    procedure FillVclStylesList;
    procedure ClearVclStylesList;
  public
  end;

var
  FrmMain: TFrmMain;

implementation
uses
  IOUtils,
  Vcl.Themes,
  uVCLStyleUtils;

{$R *.dfm}

procedure TFrmMain.FormCreate(Sender: TObject);
begin
   Loading:=False;
   FStylesPath:= IncludeTrailingPathDelimiter(ExpandFileName(ExtractFilePath(ParamStr(0))  + '\..\Styles'));
   //ReportMemoryLeaksOnShutdown:=True;
   FBitmap:=TBitmap.Create;
   FBitmap.PixelFormat:=pf32bit;
   FBitmap.Width :=Image1.ClientRect.Width;
   FBitmap.Height:=Image1.ClientRect.Height;
   FillVclStylesList;
end;

procedure TFrmMain.FormDestroy(Sender: TObject);
begin
  ClearVclStylesList;
  FBitmap.Free;
end;

procedure TFrmMain.ListView1Change(Sender: TObject; Item: TListItem;  Change: TItemChange);
var
   LStyle : TVCLStyleExt;
begin
   LStyle:=nil;
   if Assigned(Item.Data) then
     LStyle:=TVCLStyleExt(Item.Data)
   else
   if not Loading then
   begin
     LStyle := TVCLStyleExt.Create(FStylesPath+Item.SubItems[0]);
     Item.Data:= LStyle;
   end;

   if Assigned(LStyle) and not Loading  then
   begin
     LStyle.DrawSampleWindow(FBitmap.Canvas, Image1.ClientRect, Item.SubItems[1]);
     Image1.Picture.Assign(FBitmap);
   end;
end;

procedure TFrmMain.ClearVclStylesList;
var
 i : integer;
begin
 for i:=0 to ListView1.Items.Count-1 do
  if Assigned(ListView1.Items[i].Data) then
    TVCLStyleExt(ListView1.Items[i].Data).Free;
 ListView1.Items.Clear;
end;

procedure TFrmMain.FillVclStylesList;
Var
 StyleName: string;
 FileName : string;
 Item     : TListItem;
 StyleInfo:  TStyleInfo;
 SourceInfo: TSourceInfo;
 VCLStyleExt:TVCLStyleExt;
begin
   Loading:=True;
   {
   for FileName in TDirectory.GetFiles(FStylesPath,'*.vsf') do
   begin
      Item:=ListView1.Items.Add;
      Item.Caption:='File';
      Item.SubItems.Add(ExtractFileName(FileName));
      TStyleManager.IsValidStyle(FileName, StyleInfo);
      Item.SubItems.Add(StyleInfo.Name);
      Item.SubItems.Add(StyleInfo.Author);
      Item.SubItems.Add(StyleInfo.AuthorURL);
      Item.SubItems.Add(StyleInfo.Version);
   end;
                    }



   for StyleName in  TStyleManager.StyleNames do
   if CompareText(StyleName,'Windows')<>0 then
   begin
      Item:=ListView1.Items.Add;
      Item.Caption:='Resource';
      Item.SubItems.Add('');

      SourceInfo:=TStyleManager.StyleSourceInfo[StyleName];
      VCLStyleExt:=TVCLStyleExt.Create(TStream(SourceInfo.Data), False);
      Item.Data  :=VCLStyleExt;
      StyleInfo:=VCLStyleExt.StyleInfo;
      Item.SubItems.Add(StyleInfo.Name);
      Item.SubItems.Add(StyleInfo.Author);
      Item.SubItems.Add(StyleInfo.AuthorURL);
      Item.SubItems.Add(StyleInfo.Version);
   end;

   Loading:=False;


   if ListView1.Items.Count>0 then
    ListView1.Selected:=ListView1.Items.Item[0];
end;



end.
