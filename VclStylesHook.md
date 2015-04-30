The <a href='http://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Hooks.pas'>Vcl.Styles.Hook</a> unit,  patch a set of WinApi and UxTheme methods in order to fix the system colors used by some VCL components and also replace some elements of the Windows Native Theme patching the DrawThemeBackground and DrawThemeBackgroundEx methods.

# Screenshots #

As you can see some controls uses the native windows theme to draw the highlight colors.

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/full.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/full.png)

Including the Vcl.Styles.Hook unit in your project the system colors are replaced by the proper VCL Style colors.

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/Full_Fix.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/Full_Fix.png)

When you uses a TListView setting the CheckBoxes property to True and with the VCL Styles enabled the control will look like so

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_08_19-demo.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_08_19-demo.png)

Now including the VCL.Styles.Hooks unit to your project the checkbox control is properly drawn using the current active VCL Style.

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_08_55-demo.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_08_55-demo.png)

Also this unit add support for styling the Listview groups.
ListView with VCL Styles

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_11_23-demo.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_11_23-demo.png)

ListView with VCL Styles + Vcl.Styles.Hooks unit

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_12_45-demo.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_12_45-demo.png)

A similar improvement was applied to the TTreeview controls. by default the opened and closed glyphs are draw using the native look and feel

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/treeview_nohook.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/treeview_nohook.png)

And now using the VCL.Styles.Hooks unit

![https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/treeview_hook.png](https://dl.dropboxusercontent.com/u/12733424/Blog/VCl%20Utils/Hooks%20Images/treeview_hook.png)

DateTime Controls

The Styling of the TMonthCalendar and TDatetimepicker components is one of the limitations of the VCL Styles, because such components are owner-draw by the system and doesn’t allow to customize the look and feel when the native themes are enabled also only the newest versions of Delphi includes a partial styling support for such components. The Vcl.Styles.Hooks add styling support for these controls.

TDateTimePicker and TMonthCalendar with VCL Styles

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_29_24-demo-rad-studio-xe7-umain-running-built.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_29_24-demo-rad-studio-xe7-umain-running-built.png)


TDateTimePicker and TMonthCalendar with VCL Styles + Vcl.Styles.Hooks unit

![https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_54_31-demo-embarcadero-rad-studio-xe2-vcl-styles-hooks-running-built.png](https://theroadtodelphi.files.wordpress.com/2014/11/2014-11-04-11_54_31-demo-embarcadero-rad-studio-xe2-vcl-styles-hooks-running-built.png)


For more info check the articles

http://theroadtodelphi.wordpress.com/2014/08/06/vcl-styles-utils-new-feature/

http://theroadtodelphi.wordpress.com/2013/12/07/vcl-styles-utils-project-new-addition-patch-for-system-colors/

http://theroadtodelphi.wordpress.com/2014/11/04/vcl-styles-utils-major-update-dialogs-progressbar-datetimepicker-listview-and-more/