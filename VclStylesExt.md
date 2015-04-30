The [Vcl.Styles.Ext](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Ext.pas) unit extends the [TStyleManager](http://docwiki.embarcadero.com/VCL/en/Themes.TStyleManager) Delphi class adding new properties and methods to list, remove and reload VCL Styles.


Example to remove a vcl style

```
 TStyleManager.RemoveStyle('Carbon');
```



This unit also a define a new class called _TCustomStyleExt_ that descends of the [TCustomStyle](http://docwiki.embarcadero.com/VCL/en/Vcl.Styles.TCustomStyle) and allow you to access to the RAW data of the VCL Styles like bitmaps, fonts and colors.

For example you can modify the vcl styles colors in this way

```
  TCustomStyleExt(TStyleManager.ActiveStyle).SetStyleColor(scEdit, clRed);
  TCustomStyleExt(TStyleManager.Style[StyleName]).SetSystemColor(clBtnFace,clLime);
  TCustomStyleExt(TStyleManager.Style[StyleName]).SetStyleColor(scBorder, clBlue);
  TCustomStyleExt(TStyleManager.Style[StyleName]).SetStyleFontColor(sfButtonTextNormal, clYellow);
```