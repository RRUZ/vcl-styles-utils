program VisualStylesEQU;

uses
  Vcl.Forms,
  Vcl.Themes,
  Vcl.Styles,
  uHueSat in 'uHueSat.pas' {FrmHueSat},
  uHSLUtils in '..\Common\uHSLUtils.pas',
  Vcl.Styles.Utils in '..\Common\Vcl.Styles.Utils.pas',
  Vcl.Styles.Ext in '..\Common\Vcl.Styles.Ext.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmHueSat, FrmHueSat);
  Application.Run;
end.
