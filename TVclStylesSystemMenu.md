The TVclStylesSystemMenu component included in the [Vcl.Styles.Utils.SystemMenu](https://code.google.com/p/vcl-styles-utils/source/browse/trunk/Common/Vcl.Styles.Utils.SystemMenu.pas) unit allow to select a Vcl Style from the system Menu.

http://theroadtodelphi.files.wordpress.com/2014/02/cobalt.png?w=820


To use this component, only you need create an new instance passing a reference to the form.

```

procedure TForm1.FormCreate(Sender: TObject);
begin
  TVclStylesSystemMenu.Create(Self);
end;

```


### Related articles ###

[A new way to select and apply a VCL Style in Runtime](http://theroadtodelphi.wordpress.com/2014/08/22/a-new-way-to-select-and-apply-a-vcl-style-in-runtime/)