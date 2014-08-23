program Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.FormStyleHooks in '..\Common\Vcl.Styles.FormStyleHooks.pas',
  Vcl.Styles.NC in '..\Common\Vcl.Styles.NC.pas',
  Unit1 in 'Unit1.pas' {Form1},
  Unit2 in 'Unit2.pas' {Form2},
  Vcl.Styles.Utils.SystemMenu in '..\Common\Vcl.Styles.Utils.SystemMenu.pas',
  Unit3 in 'Unit3.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  TStyleManager.TrySetStyle('Turquoise Gray');
  Application.CreateForm(TFrmMain, FrmMain);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
