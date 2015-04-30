The [Vcl.Styles.Utils](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Utils.pas) unit contain a set classes to modify the VCL Styles in run-time manipulating the visual elements and fonts.

> Currently all these operations are supported.

### HSL ###

> Modify the Hue, Saturation and Lightness for the visual elements and fonts of the VCL Styles.

> ![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerHSL.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerHSL.png)

### RGB ###

> Change the values of the RGB components for the visual elements and fonts of the VCL Styles.

![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerRGB.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerRGB.png)



### Blend ###

> Use any color and available blend effect (Burn, Additive, Dodge, Overlay, Difference, Lighten, Darken, Screen) to customize the visual elements and fonts colors of the VCL Styles.


![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerBlend.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/EqualizerBlend.png)

Check this form with the _Carbon_ style applied.

![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/CarbonSimple.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/CarbonSimple.png)

```

//This code shows how you can add a Overlay blend effect to an existing vcl style
//and then apply the changes in run-time.
procedure TFrmMain.FormCreate(Sender: TObject);
var
  VclStylesUtils : TVclStylesUtils;
  Filters        : TObjectList<TBitmapFilter>;
begin
  //create the instance to the  TVclStylesUtils using the carbon vcl style
  VclStylesUtils:=TVclStylesUtils.Create('Carbon');
  //create the filter list to apply
  Filters:=TObjectList<TBitmapFilter>.Create;
  try
    //create a TBitmap32BlendOverlay filter and add to the list
    Filters.Add(TBitmap32BlendOverlay.Create(clYellow));
    //set the elements to be affected
    VclStylesUtils.Elements:=VclStylesUtils.Elements+ [vseBitmaps ,vseSysColors, vseStyleColors];
    //set the filters
    VclStylesUtils.SetFilters(Filters);
    //Apply the changes to the style
    VclStylesUtils.ApplyChanges;
    //reload the modified style
    TStyleManager.ReloadStyle('Carbon');
  finally
    VclStylesUtils.Free;
    Filters.Free;
  end;
end;

```

And this is the result after of apply the above code

![http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/CarbonSimple_After.png](http://dl.dropbox.com/u/12733424/Blog/Vcl%20Utils/CarbonSimple_After.png)



# Videos #

<a href='http://www.youtube.com/watch?feature=player_embedded&v=MxT1LR-JtvY' target='_blank'><img src='http://img.youtube.com/vi/MxT1LR-JtvY/0.jpg' width='425' height=344 /></a>
<a href='http://www.youtube.com/watch?feature=player_embedded&v=zmQuC74zaKk' target='_blank'><img src='http://img.youtube.com/vi/zmQuC74zaKk/0.jpg' width='425' height=344 /></a>