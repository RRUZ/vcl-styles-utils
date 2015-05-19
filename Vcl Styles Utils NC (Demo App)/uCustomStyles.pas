unit uCustomStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls;

type
  TFrmCustomStyles = class(TForm)
    ImageList1: TImageList;
    ComboBox1: TComboBox;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
  private
    { Private declarations }
    NCControls : TNCControls;
    procedure ButtonNCClick(Sender: TObject);
    procedure ButtonNCDropDownClick(Sender: TObject);
  public
    { Public declarations }
  end;

implementation

uses
 Vcl.Themes;

{$R *.dfm}

procedure TFrmCustomStyles.ButtonNCClick(Sender: TObject);
begin

end;

procedure TFrmCustomStyles.ButtonNCDropDownClick(Sender: TObject);
begin

end;

procedure TFrmCustomStyles.ComboBox1Change(Sender: TObject);
begin
  NCControls.StyleServices := TStyleManager.Style[ComboBox1.Text];
  Perform(WM_NCPAINT, 0, 0);;
end;

procedure TFrmCustomStyles.FormCreate(Sender: TObject);
var
 s : string;
 LIndex : Integer;

begin
  for s in TStyleManager.StyleNames do
   if not SameText(s, 'Windows') then
    ComboBox1.Items.Add(s);

  ComboBox1.ItemIndex:=ComboBox1.Items.IndexOf('Auric');

  NCControls:=TNCControls.Create(Self);
  NCControls.StyleServices := TStyleManager.Style[ComboBox1.Text];
  LIndex:=NCControls.Add(TNCButton.Create(NCControls));
  NCControls[LIndex].Style       := nsSplitButton;
  NCControls[LIndex].ImageStyle  := isGrayHot;
  NCControls[LIndex].Images      := ImageList1;
  NCControls[LIndex].ImageIndex  := 0;
  NCControls[LIndex].BoundsRect  := Rect(30, 0, 140, 25);
  NCControls[LIndex].Caption     := 'nsSplitButton';
  NCControls[LIndex].Name        := 'nsSplitButton';
  NCControls[LIndex].OnClick     := ButtonNCClick;
  NCControls[LIndex].OnDropDownClick := ButtonNCDropDownClick;

  LIndex:=NCControls.Add(TNCButton.Create(NCControls));
  NCControls[LIndex].Style       := nsPushButton;
  NCControls[LIndex].ImageStyle  := isNormal;
  NCControls[LIndex].Images      := ImageList1;
  NCControls[LIndex].ImageIndex  := 1;
  NCControls[LIndex].BoundsRect  := Rect(145, 0, 255, 25);
  NCControls[LIndex].Caption     := 'nsPushButton';
  NCControls[LIndex].Name        := 'nsPushButton';
  NCControls[LIndex].OnClick     := ButtonNCClick;


  LIndex:=NCControls.Add(TNCButton.Create(NCControls));
  NCControls[LIndex].Style       := nsTranparent;
  NCControls[LIndex].ImageStyle  := isGrayHot;
  NCControls[LIndex].Images      := ImageList1;
  NCControls[LIndex].ImageIndex  := 2;
  NCControls[LIndex].BoundsRect  := Rect(260, 0, 370, 25);
  NCControls[LIndex].Caption     := 'nsTranparent';
  NCControls[LIndex].Name        := 'nsTranparent';
  NCControls[LIndex].OnClick     := ButtonNCClick;

  LIndex:=NCControls.Add(TNCButton.Create(NCControls));
  NCControls[LIndex].Style       := nsSplitTrans;
  NCControls[LIndex].ImageStyle  := isGrayHot;
  NCControls[LIndex].Images      := ImageList1;
  NCControls[LIndex].ImageIndex  := 3;
  NCControls[LIndex].BoundsRect  := Rect(375, 0, 500, 25);
  NCControls[LIndex].Caption     := 'nsSplitTrans';
  NCControls[LIndex].Name        := 'nsSplitTrans';
  NCControls[LIndex].OnClick     := ButtonNCClick;
  NCControls[LIndex].OnDropDownClick := ButtonNCDropDownClick;
end;

end.
