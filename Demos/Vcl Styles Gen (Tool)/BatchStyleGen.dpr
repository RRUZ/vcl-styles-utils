program BatchStyleGen;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  uHSLUtils in '..\..\Common\uHSLUtils.pas',
  Vcl.Styles.Ext in '..\..\Common\Vcl.Styles.Ext.pas',
  Vcl.Styles.Utils in '..\..\Common\Vcl.Styles.Utils.pas',
  PngFunctions in '..\Extras\PngFunctions.pas',
  PngImageList in '..\Extras\PngImageList.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
