The [Vcl.Styles.Utils.Menus](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Utils.Menus.pas) unit allows style the popup menus of the VCL Applications.

### Delphi XE2-XE5 ###
The standard VCL Styles (Delphi XE2- XE5) doesn't support Popup menus, this means if you apply any VCL Style to your VCL Application  the popup menus will remain with the Windows native look and feel  (exists some workarounds for this like use a TPopupActionBar, but this only works partially). By using the [Vcl.Styles.Utils.Menus](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Utils.Menus.pas) unit you can overcome this limitation.

> ![https://theroadtodelphi.files.wordpress.com/2014/07/2.png](https://theroadtodelphi.files.wordpress.com/2014/07/2.png)

> ![https://theroadtodelphi.files.wordpress.com/2014/07/4.png](https://theroadtodelphi.files.wordpress.com/2014/07/4.png)

> ![https://theroadtodelphi.files.wordpress.com/2014/01/6.png](https://theroadtodelphi.files.wordpress.com/2014/01/6.png)

### Delphi XE6-XE7 ###

Starting with Delphi XE6 the code to style the popup menus was licensed to Embarcadero, via a non-exclusive proprietary license. So from now you can choose between the Embarcadero version of the style menus or still using the VCL Style Utils. If you want still using the VCL Styles Utils version. you must include the Vcl.Styles.Utils.Menus unit and made the next changes.

```

{$DEFINE UseVCLStyleUtilsMenu}
{$IF CompilerVersion >= 27} 
  {$UNDEF UseVCLStyleUtilsMenu} // comment this line if you want to use the VCL Styles Utils Menus Hooks instead
{$IFEND}

```



### Related articles ###
[VCL Styles Utils and Popup Menus – Major Update](http://theroadtodelphi.wordpress.com/2014/01/16/vcl-styles-utils-and-popup-menus-major-update/)

[VCL Styles Utils, Embarcadero Agreement and Delphi XE6](http://theroadtodelphi.wordpress.com/2014/04/21/vcl-styles-utils-embarcadero-agreement-and-delphi-xe6/)

[XE6 Menu Styling (and a Thank You)](http://blog.marcocantu.com/blog/2014_april_xe6menustyling.html)