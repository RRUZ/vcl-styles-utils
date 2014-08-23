unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList;

type
  TForm2 = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    NCControls : TNCControls;
    procedure ButtonNCClick(Sender: TObject);
    procedure ButtonNCDropDownClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.dfm}

procedure TForm2.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('you clicked the %s button', [TNCButton(Sender).Name]));
end;

procedure TForm2.ButtonNCDropDownClick(Sender: TObject);
begin
  ShowMessage(Format('you clicked the DropDown of the %s button', [TNCButton(Sender).Name]));
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  NCControls:=TNCControls.Create(Self);
  NCControls.List.Add(TNCButton.Create(NCControls));
  NCControls.List[0].Style       := nsSplitButton;
  NCControls.List[0].ImageStyle  := isGrayHot;
  NCControls.List[0].Images      := ImageList1;
  NCControls.List[0].ImageIndex  := 0;
  NCControls.List[0].BoundsRect  := Rect(30, 0, 140, 25);
  NCControls.List[0].Caption     := 'nsSplitButton';
  NCControls.List[0].Name        := 'nsSplitButton';
  NCControls.List[0].OnClick     := ButtonNCClick;
  NCControls.List[0].OnDropDownClick := ButtonNCDropDownClick;

  NCControls.List.Add(TNCButton.Create(NCControls));
  NCControls.List[1].Style       := nsPushButton;
  NCControls.List[1].ImageStyle  := isNormal;
  NCControls.List[1].Images      := ImageList1;
  NCControls.List[1].ImageIndex  := 1;
  NCControls.List[1].BoundsRect  := Rect(145, 0, 255, 25);
  NCControls.List[1].Caption     := 'nsPushButton';
  NCControls.List[1].Name        := 'nsPushButton';
  NCControls.List[1].OnClick     := ButtonNCClick;


  NCControls.List.Add(TNCButton.Create(NCControls));
  NCControls.List[2].Style       := nsTranparent;
  NCControls.List[2].ImageStyle  := isGrayHot;
  NCControls.List[2].Images      := ImageList1;
  NCControls.List[2].ImageIndex  := 2;
  NCControls.List[2].BoundsRect  := Rect(260, 0, 370, 25);
  NCControls.List[2].Caption     := 'nsTranparent';
  NCControls.List[2].Name        := 'nsTranparent';
  NCControls.List[2].OnClick     := ButtonNCClick;

  NCControls.List.Add(TNCButton.Create(NCControls));
  NCControls.List[3].Style       := nsSplitTrans;
  NCControls.List[3].ImageStyle  := isGrayHot;
  NCControls.List[3].Images      := ImageList1;
  NCControls.List[3].ImageIndex  := 3;
  NCControls.List[3].BoundsRect  := Rect(375, 0, 500, 25);
  NCControls.List[3].Caption     := 'nsSplitTrans';
  NCControls.List[3].Name        := 'nsSplitTrans';
  NCControls.List[3].OnClick     := ButtonNCClick;
  NCControls.List[3].OnDropDownClick := ButtonNCDropDownClick;

end;

end.
