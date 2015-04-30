The [Vcl.Styles.WebBrowser](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.WebBrowser.pas) unit includes a interposer class to style the scrollbars and dialogs of the [TWebBrowser](http://docwiki.embarcadero.com/Libraries/XE6/en/SHDocVw.TWebBrowser) component.


The recommended way to use the TVclStylesWebBrowser class is add the Vcl.Styles.WebBrowser unit to the uses clause is some point after of the SHDocVw unit and then use an interposer class like so

```
 type
  TWebBrowser=class(TVclStylesWebBrowser)
```


A sample application can be found [here](https://code.google.com/p/vcl-styles-utils/source/browse/#svn%2Ftrunk%2FVcl%20Styles%20TWebBrowser%20(Demo%20App))


![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbscrollbars.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbscrollbars.png)
![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbjserror.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbjserror.png)
![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbalerts.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/twbalerts.png)

### Related articles ###

[Delphi Vcl Styles and TWebBrowser, source code released.](http://theroadtodelphi.wordpress.com/2012/03/20/delphi-vcl-styles-and-twebbrowser-source-code-released/)

[VCL Styles Utils and TWebBrowser support](http://theroadtodelphi.wordpress.com/2014/02/06/vcl-styles-utils-and-twebbrowser-support/)