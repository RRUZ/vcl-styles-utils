unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Menus, Vcl.StdCtrls, Vcl.ImgList,
  Vcl.ComCtrls, Vcl.ToolWin, Vcl.Styles.NC;

type
  TFrmMain = class(TForm)
    MainMenu1: TMainMenu;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Repeat1: TMenuItem;
    Cut1: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    PasteSpecial1: TMenuItem;
    Find1: TMenuItem;
    Replace1: TMenuItem;
    GoTo1: TMenuItem;
    Links1: TMenuItem;
    Object1: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    File1: TMenuItem;
    New1: TMenuItem;
    Open1: TMenuItem;
    Save1: TMenuItem;
    SaveAs1: TMenuItem;
    Print1: TMenuItem;
    PrintSetup1: TMenuItem;
    Exit1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    Help2: TMenuItem;
    Contents2: TMenuItem;
    Index1: TMenuItem;
    Commands1: TMenuItem;
    Procedures1: TMenuItem;
    Keyboard1: TMenuItem;
    SearchforHelpOn2: TMenuItem;
    Tutorial1: TMenuItem;
    HowtoUseHelp2: TMenuItem;
    About2: TMenuItem;
    BtnSample1: TButton;
    ImageList1: TImageList;
    Button1: TButton;
    Button3: TButton;
    CheckBox1: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure BtnSample1Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
     NCControls : TNCControls;
  public
    { Public declarations }
    procedure ButtonNCClick(Sender: TObject);
  end;

var
  FrmMain: TFrmMain;

implementation

uses
 Vcl.Styles.Utils.SystemMenu,
 Unit1, Unit2, Unit3;

{$R *.dfm}


procedure TFrmMain.BtnSample1Click(Sender: TObject);
var
 LForm : TForm1;
begin
   LForm:= TForm1.Create(Self);
   LForm.Show();
end;

procedure TFrmMain.Button1Click(Sender: TObject);
var
 LForm : TForm2;
begin
   LForm:= TForm2.Create(Self);
   LForm.Show();
end;


procedure TFrmMain.Button3Click(Sender: TObject);
var
 LForm : TForm3;
begin
   LForm:= TForm3.Create(Self);
   LForm.Show();
end;

procedure TFrmMain.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('You clicked the button %s', [TNCButton(Sender).Name]));
end;

procedure TFrmMain.CheckBox1Click(Sender: TObject);
begin
  NCControls.Visible:=CheckBox1.Checked;
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
 i : integer;
begin
  ReportMemoryLeaksOnShutdown:=True;
  TVclStylesSystemMenu.Create(Self);

  NCControls:=TNCControls.Create(Self);
   for i:=0 to 10 do
   begin
      NCControls.List.Add(TNCButton.Create(NCControls));
      NCControls.List[i].Name      := Format('NCButton%d',[i+1]);
      NCControls.List[i].Hint      := Format('Hint for NCButton%d',[i+1]);
      NCControls.List[i].ShowHint  := True;
      NCControls.List[i].Caption   :='';
      NCControls.List[i].Style     :=nsTranparent;
      NCControls.List[i].ImageStyle:=isGrayHot;
      NCControls.List[i].Images    :=ImageList1;
      NCControls.List[i].ImageIndex:=i;
      NCControls.List[i].ImageAlignment := TImageAlignment.iaCenter;
      NCControls.List[i].BoundsRect:=Rect(30+(i*20),5,50+(i*20),25);
      NCControls.List[i].OnClick   := ButtonNCClick;
   end;
end;


end.
