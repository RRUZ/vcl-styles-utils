unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Styles.NC, Vcl.Dialogs, Vcl.ImgList, Vcl.Menus,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    PopupMenu1: TPopupMenu;
    Window1: TMenuItem;
    NewWindow1: TMenuItem;
    Tile1: TMenuItem;
    Cascade1: TMenuItem;
    ArrangeAll1: TMenuItem;
    Hide1: TMenuItem;
    Show1: TMenuItem;
    N1: TMenuItem;
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
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    Help1: TMenuItem;
    Contents1: TMenuItem;
    SearchforHelpOn1: TMenuItem;
    HowtoUseHelp1: TMenuItem;
    About1: TMenuItem;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
     NCControls : TNCControls;
  public
    { Public declarations }
    procedure ButtonNCClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TForm1.ButtonNCClick(Sender: TObject);
begin
 ShowMessage('Hello');
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  NCControls:=TNCControls.Create(Self);
  NCControls.List.Add(TNCButton.Create(NCControls));
  NCControls.List[0].Style       := nsSplitButton;
  NCControls.List[0].ImageStyle  := isGrayHot;
  NCControls.List[0].Images      := ImageList1;
  NCControls.List[0].ImageIndex  := 3;
  NCControls.List[0].BoundsRect  := Rect(30,5,100,25);
  NCControls.List[0].Caption     := 'Menu';
  NCControls.List[0].DropDownMenu:= PopupMenu1;
  NCControls.List[0].OnClick     := ButtonNCClick;
end;

end.
