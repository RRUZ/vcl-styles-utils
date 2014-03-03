unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.Styles, Vcl.Themes, Vcl.Menus, Vcl.Buttons, Vcl.ImgList,
  Vcl.ExtDlgs, Vcl.ComCtrls;

type
  TForm1 = class(TForm)
    LblStyles: TLabel;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    StaticText1: TStaticText;
    CheckBox1: TCheckBox;
    OpenDialog1: TOpenDialog;
    ColorDialog1: TColorDialog;
    FontDialog1: TFontDialog;
    ReplaceDialog1: TReplaceDialog;
    FindDialog1: TFindDialog;
    PrinterSetupDialog1: TPrinterSetupDialog;
    PageSetupDialog1: TPageSetupDialog;
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    I1: TMenuItem;
    C2: TMenuItem;
    S1: TMenuItem;
    D1: TMenuItem;
    B1: TMenuItem;
    BreakItem11: TMenuItem;
    R1: TMenuItem;
    RadioItem11: TMenuItem;
    D2: TMenuItem;
    I2: TMenuItem;
    C3: TMenuItem;
    StaticText2: TStaticText;
    PopupMenu2: TPopupMenu;
    R2: TMenuItem;
    RightToLeftItem11: TMenuItem;
    RightToLeftItem12: TMenuItem;
    RightToLeftItem13: TMenuItem;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Contents2: TMenuItem;
    Contents3: TMenuItem;
    Contents4: TMenuItem;
    Button1: TButton;
    OpenPictureDialog1: TOpenPictureDialog;
    OpenTextFileDialog1: TOpenTextFileDialog;
    PrintDialog1: TPrintDialog;
    SpeedButton10: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    procedure C3Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Vcl.Styles.Utils.SysControls, FileCtrl;
{$R *.dfm}

procedure TForm1.About1Click(Sender: TObject);
begin
  ShowMessage('EnJoY !!');
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  FDir : string;
begin

   SelectDirectory('Select Directory', ExtractFileDrive(FDir), FDir,
             [sdNewUI, sdNewFolder])
end;

procedure TForm1.C3Click(Sender: TObject);
begin
  ShowMessage('Hi');
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  TSysStyleManager.Enabled := TCheckBox(Sender).Checked;
end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBox1.Items[ComboBox1.ItemIndex]);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  s: string;
begin
{$IFDEF DEBUG}
  ReportMemoryLeaksOnShutdown := True;
{$ENDIF }
  for s in TStyleManager.StyleNames do
    ComboBox1.Items.Add(s);

  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf(TStyleManager.ActiveStyle.Name);
end;

procedure TForm1.SpeedButton10Click(Sender: TObject);
begin
PrintDialog1.Execute(Handle);
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  MessageBox(Handle, 'This is a simple Yes/No MessageBox .',
    'MessageBox Caption ', MB_YESNO or MB_ICONQUESTION);
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
{$IFNDEF DEBUG}
  raise Exception.Create('Error Message');
{$ENDIF}
end;

procedure TForm1.SpeedButton3Click(Sender: TObject);
begin
  FontDialog1.Execute();
end;

procedure TForm1.SpeedButton4Click(Sender: TObject);
begin
  ColorDialog1.Execute();
end;

procedure TForm1.SpeedButton5Click(Sender: TObject);
begin
  ReplaceDialog1.Execute();
end;

procedure TForm1.SpeedButton6Click(Sender: TObject);
begin
  FindDialog1.Execute();
end;

procedure TForm1.SpeedButton7Click(Sender: TObject);
begin
  PageSetupDialog1.Execute();
end;

procedure TForm1.SpeedButton8Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute();
end;

procedure TForm1.SpeedButton9Click(Sender: TObject);
begin
 if OpenDialog1.Execute then
  ShowMessage(Format('%s', [OpenDialog1.FileName]));
end;

end.
