The **TVclStylesPreview** Component included in the <a href='http://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Ext.pas'>Vcl.Styles.Ext</a> unit allow you preview a VCL Style from a file or resource.

Check the next sample code

```

 var 
   StyleName : string;
   SourceInfo: TSourceInfo;
   LStyle : TCustomStyleServices;
   FPreview : TVclStylesPreview;
 begin
 FPreview:=TVclStylesPreview.Create(Self);
 FPreview.Parent:=PanelPreview;
 FPreview.BoundsRect := PanelPreview.ClientRect;
 StyleName:='Carbon';
 if (StyleName <>'') and (not SameText(StyleName, 'Windows')) then
 begin
   TStyleManager.StyleNames;//call DiscoverStyleResources
   LStyle:=TStyleManager.Style[StyleName];
   FPreview.Caption:=StyleName;
   FPreview.Style:=LStyle;
   TVclStylesPreviewClass(FPreview).Paint;
 end;
 ....
 end;

```

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/VCLStyles_Previewer.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/VCLStyles_Previewer.png)

Check out a sample application [here](https://code.google.com/p/vcl-styles-utils/source/browse/#svn%2Ftrunk%2FVcl%20Styles%20Preview%20(Demo%20App)).