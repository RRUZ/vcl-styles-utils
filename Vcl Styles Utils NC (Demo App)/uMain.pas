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
    BtnDropDownMenu: TButton;
    ImageList1: TImageList;
    BtnStyles: TButton;
    BtnCustomStyle: TButton;
    CheckBoxNCVisible: TCheckBox;
    BtnAlpha: TButton;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BtnDropDownMenuClick(Sender: TObject);
    procedure CheckBoxNCVisibleClick(Sender: TObject);
    procedure BtnStylesClick(Sender: TObject);
    procedure BtnCustomStyleClick(Sender: TObject);
    procedure BtnAlphaClick(Sender: TObject);
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
 uButtonsStyles, uCustomStyles, uDropdown, uAlphaGradient;

{$R *.dfm}


procedure TFrmMain.BtnDropDownMenuClick(Sender: TObject);
var
 LForm : TFrmDropDown;
begin
   LForm:= TFrmDropDown.Create(Self);
   LForm.Show();
end;

procedure TFrmMain.BtnStylesClick(Sender: TObject);
var
 LForm : TFrmButtonsStyles;
begin
   LForm:= TFrmButtonsStyles.Create(Self);
   LForm.Show();
end;


procedure TFrmMain.BtnAlphaClick(Sender: TObject);
var
 LForm : TFrmAlphaGradient;
begin
   LForm:= TFrmAlphaGradient.Create(Self);
   LForm.Show();
end;
procedure TFrmMain.BtnCustomStyleClick(Sender: TObject);
var
 LForm : TFrmCustomStyles;
begin
   LForm:= TFrmCustomStyles.Create(Self);
   LForm.Show();
end;

procedure TFrmMain.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('You clicked the button %s', [TNCButton(Sender).Name]));
end;

procedure TFrmMain.CheckBoxNCVisibleClick(Sender: TObject);
begin
  NCControls.Visible:=CheckBoxNCVisible.Checked;
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
      NCControls.Add(TNCButton.Create(NCControls));
      NCControls[i].Name      := Format('NCButton%d',[i+1]);
      NCControls[i].Hint      := Format('Hint for NCButton%d',[i+1]);
      NCControls[i].ShowHint  := True;
      NCControls[i].Caption   :='';
      NCControls[i].Style     :=nsTranparent;
      NCControls[i].ImageStyle:=isGrayHot;
      NCControls[i].Images    :=ImageList1;
      NCControls[i].ImageIndex:=i;
      NCControls[i].ImageAlignment := TImageAlignment.iaCenter;
      NCControls[i].BoundsRect:=Rect(30+(i*20),5,50+(i*20),25);
      NCControls[i].OnClick   := ButtonNCClick;
   end;
end;


end.
