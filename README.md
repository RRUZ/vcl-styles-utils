# VCL Styles Utils #
The *VCL Styles Utils* is a Delphi library which extend, fix bugs and add new features to the [RAD Studio VCL Styles](http://docwiki.embarcadero.com/RADStudio/en/VCL_Styles_Overview).

<p align="center">
  <img src="https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Images%20New%20Dialogs/Mix6.png" alt="Mix"/>
</p>

[![](https://theroadtodelphi.files.wordpress.com/2014/07/followrruz.png)](https://twitter.com/RRUZ)

## Features ##
<ul>
 <li>Works in Delphi XE2-XE8</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesExt'>Vcl.Styles.Ext</a> unit extended the VCL Styles adding new properties and methods to list, remove and reload VCL Styles.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesUtils'>Vcl.Styles.Utils</a> unit, allows modify the VCL Styles manipulating the visual elements and fonts colors.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesNC'>TNCControls</a> component which allow you add controls to the Non Client area of the form</li>
</ul> 
![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/NCButtonsMain.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/NCButtonsMain.png)

<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesWebBrowser'> Vcl.Styles.WebBrowser</a> unit, add support for style the scrollbars and dialogs of the TWebBrowser component.</li>
</ul>  
![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/WebBrowserStyledMain.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/WebBrowserStyledMain.png)
<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VCLStylesMenus'>Vcl.Styles.Utils.Menus </a> unit, add support for style the VCL popup menus, system and Shell menus.</li>
</ul> 
![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/VCLStyles%20PopUp/Menu.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/VCLStyles%20PopUp/Menu.png)
<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesSysControls'>Vcl.Styles.SysControls </a> add support for style the Standard Windows dialogs.</li>
</ul>  
![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/DialogsMain.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/DialogsMain.png)
<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesFormStyleHooks'>Vcl.Styles.FormStyleHooks</a> unit includes some additional Style Hooks for TForms.</li>
</ul>   
 ![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/FormHooksMain.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/FormHooksMain.png)
<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VCLStylesUxTheme'>Task Dialogs</a> support.</li>
</ul>
 
 ![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Task%20Dialogs/TaskDialogs2.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Task%20Dialogs/TaskDialogs2.png)

<ul>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesFixes'>Vcl.Styles.Fixes</a> unit Fix several QC reports related to the VCL Styles.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesPreview'>TVclStylesPreview</a> component to preview a VCL Style.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesHook'>Vcl.Styles.Hook</a> unit patch some WinApi and UxTheme functions, in order to fix the highlight colors and some visual elements.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/VclStylesColorTabs'>Vcl.Styles.ColorTabs</a> unit includes a new Style hook for the TPageControl and TTabSheet components.</li>
 <li><a href='https://github.com/RRUZ/vcl-styles-utils/wiki/TVclStylesSystemMenu'>TVclStylesSystemMenu</a> component for select a VCL Style from the system Menu.</li>
</ul>

## Installation ##

<ul>
 <li>Unzip or checkout the files of the library in a writable folder.</li>
 <li>Under Tools, Environment Options, Library, add the directory where the VCL Styles Utils library have been installed (Ex : C:\Delphi\Libs\vcl-styles-utils\Common) to the Win32 and Win64 library path.<br>
</li>
</ul>

**Note** : If you want to use the Vcl.Styles.Hooks unit you must also include the [Delphi Detours Library](https://code.google.com/p/delphi-detours-library/) files in your lib/search path Ex : C:\Delphi\Libs\vcl-styles-utils\Common\delphi-detours-library

