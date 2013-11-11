unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.Buttons, Vcl.StdCtrls, Vcl.Styles, Vcl.Themes,
  Vcl.Menus, Vcl.Samples.Spin, Vcl.ImgList;

type
  TMain = class(TForm)
    Edit1: TEdit;
    SpeedButton1: TSpeedButton;
    ListBox1: TListBox;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Close1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    Label1: TLabel;
    Label2: TLabel;
    PopupMenu1: TPopupMenu;
    Item11: TMenuItem;
    CheckedItem1: TMenuItem;
    DisabledItem1: TMenuItem;
    Item21: TMenuItem;
    N4: TMenuItem;
    Item31: TMenuItem;
    SubItem11: TMenuItem;
    SubItem21: TMenuItem;
    SubItem31: TMenuItem;
    SubItem41: TMenuItem;
    SubItem12: TMenuItem;
    SubItem22: TMenuItem;
    PrinterSetupDialog1: TPrinterSetupDialog;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SaveDialog1: TSaveDialog;
    SpeedButton4: TSpeedButton;
    FontDialog1: TFontDialog;
    ColorDialog1: TColorDialog;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    FindDialog1: TFindDialog;
    PageSetupDialog1: TPageSetupDialog;
    SpeedButton7: TSpeedButton;
    SpeedButton8: TSpeedButton;
    SpeedButton9: TSpeedButton;
    TaskDialog1: TTaskDialog;
    SpeedButton10: TSpeedButton;
    OpenDialog1: TOpenDialog;
    ReplaceDialog1: TReplaceDialog;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    N5: TMenuItem;
    SpeedButton11: TSpeedButton;
    PrintDialog1: TPrintDialog;
    OpenDialog2: TOpenDialog;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ListBox1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure SpeedButton3Click(Sender: TObject);
    procedure SpeedButton4Click(Sender: TObject);
    procedure SpeedButton5Click(Sender: TObject);
    procedure SpeedButton6Click(Sender: TObject);
    procedure SpeedButton7Click(Sender: TObject);
    procedure SpeedButton8Click(Sender: TObject);
    procedure SpeedButton9Click(Sender: TObject);
    procedure SpeedButton10Click(Sender: TObject);
    procedure SpeedButton11Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Main: TMain;

implementation

uses
  Vcl.Styles.SysControls;

{$R *.dfm}


procedure TMain.Button1Click(Sender: TObject);
begin
  PrintDialog1.Execute(Handle)
end;

procedure TMain.FormCreate(Sender: TObject);
var
  s: string;
begin
  ReportMemoryLeaksOnShutdown := True;
  for s in TStyleManager.StyleNames do
    ListBox1.Items.Add(s);
end;

procedure TMain.ListBox1Click(Sender: TObject);
var
  i: Integer;
  s: string;
begin
  i := ListBox1.ItemIndex;
  s := ListBox1.Items[i];
  TStyleManager.SetStyle(s);
end;

procedure TMain.SpeedButton10Click(Sender: TObject);
begin
  TaskDialog1.Caption := 'Caption Text';
  TaskDialog1.ExpandedText := 'ExpandedText ..';
  TaskDialog1.FooterText := 'FooterText..';
  TaskDialog1.Execute;
end;

procedure TMain.SpeedButton11Click(Sender: TObject);
begin
  raise Exception.Create('Error Message');
end;

procedure TMain.SpeedButton1Click(Sender: TObject);
begin
  UseLatestCommonDialogs := False;
  if OpenDialog2.Execute() then
    Edit1.Text := OpenDialog2.FileName;
end;

procedure TMain.SpeedButton2Click(Sender: TObject);
begin
  PrinterSetupDialog1.Execute(Handle)
end;

procedure TMain.SpeedButton3Click(Sender: TObject);
begin
  //Caption := PopupMenu1.Items[0].Caption;
  MessageBox(Handle, 'This is a simple Yes/No MessageBox .',
    'MessageBox Caption ', MB_YESNO or MB_ICONQUESTION);
end;

procedure TMain.SpeedButton4Click(Sender: TObject);
begin
  UseLatestCommonDialogs := False;
  SaveDialog1.Execute(Handle);
end;

procedure TMain.SpeedButton5Click(Sender: TObject);
begin
  ColorDialog1.Execute(Handle);
end;

procedure TMain.SpeedButton6Click(Sender: TObject);
begin
  FontDialog1.Execute(Handle);
end;

procedure TMain.SpeedButton7Click(Sender: TObject);
begin
  FindDialog1.Execute(Handle);
end;

procedure TMain.SpeedButton8Click(Sender: TObject);
begin
  PageSetupDialog1.Execute;
end;

procedure TMain.SpeedButton9Click(Sender: TObject);
begin
  ReplaceDialog1.Execute(Handle)
end;

end.
