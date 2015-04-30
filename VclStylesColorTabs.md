The <a href='http://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.ColorTabs.pas'>Vcl.Styles.ColorTabs</a> unit includes a new style hook for the TTabSheet component.

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/colortabsvclstyles2.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/colortabsvclstyles2.png)

To use this style hook you must include the Vcl.Styles.ColorTabs unit in your uses class after of the Vcl.ComCtrls unit and then register the hook in this way.


```
  TCustomStyleEngine.RegisterStyleHook(TCustomTabControl, TTabColorControlStyleHook);
  TCustomStyleEngine.RegisterStyleHook(TTabControl, TTabColorControlStyleHook);
```

Check a sample application [here](https://code.google.com/p/vcl-styles-utils/source/browse/#svn%2Ftrunk%2FVcl%20Styles%20Color%20Tabs%20(Demo%20App))