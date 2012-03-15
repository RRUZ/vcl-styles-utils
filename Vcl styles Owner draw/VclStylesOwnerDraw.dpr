program VclStylesOwnerDraw;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.OwnerDrawFix in '..\Common\Vcl.Styles.OwnerDrawFix.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Amethyst Kamri');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
