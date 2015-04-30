The [Vcl.Styles.NC](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.NC.pas) unit allows add controls in the Non Client Area of the forms through the TNCControls component, you can try a sample application [here](https://code.google.com/p/vcl-styles-utils/source/browse/#svn%2Ftrunk%2FVcl%20Styles%20Utils%20NC%20(Demo%20App)).

### sample code ###
The next code shows how add a button to the title bar of the form.

```
procedure TForm1.FormCreate(Sender: TObject);
begin
 NCControls:=TNCControls.Create(Self);
 //Add a NC Button
 NCControls.List.Add(TNCButton.Create(NCControls));
 //Set the style of the button
 NCControls.List[0].Style := nsSplitButton;
 //Set the style of the image
 NCControls.List[0].ImageStyle := isGrayHot;
 //Set the image list
 NCControls.List[0].Images := ImageList1;
 NCControls.List[0].ImageIndex := 3;
 //Set the bounds
 NCControls.List[0].BoundsRect := Rect(30,5,100,25);
 NCControls.List[0].Caption := 'Menu';
 //Assign the menu and events.
 NCControls.List[0].DropDownMenu:= PopupMenu1;
 NCControls.List[0].OnClick := ButtonNCClick;
end;

```

#### Screenshoots ####

> ![http://theroadtodelphi.files.wordpress.com/2014/08/4.png](http://theroadtodelphi.files.wordpress.com/2014/08/4.png)
> ![http://theroadtodelphi.files.wordpress.com/2014/08/3.png](http://theroadtodelphi.files.wordpress.com/2014/08/3.png)

### Related Articles ###

[VCL Styles Utils – New Feature : Non Client Area Controls](http://theroadtodelphi.wordpress.com/2014/08/24/vcl-styles-utils-new-feature-non-client-area-controls/)