unit uButtonsStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls;

type
  TFrmButtonsStyles = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    NCControls : TNCControls;
    procedure ButtonNCClick(Sender: TObject);
    procedure ButtonNCDropDownClick(Sender: TObject);
  public
  end;


implementation

uses
 Vcl.GraphUtil;

{$R *.dfm}

procedure TFrmButtonsStyles.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('you clicked the %s button', [TNCButton(Sender).Name]));
end;

procedure TFrmButtonsStyles.ButtonNCDropDownClick(Sender: TObject);
begin
  ShowMessage(Format('you clicked the DropDown of the %s button', [TNCButton(Sender).Name]));
end;

procedure TFrmButtonsStyles.FormCreate(Sender: TObject);
begin
  NCControls:=TNCControls.Create(Self);
  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[0].Style       := nsSplitButton;
  NCControls[0].ImageStyle  := isGrayHot;
  NCControls[0].Images      := ImageList1;
  NCControls[0].ImageIndex  := 0;
  NCControls[0].BoundsRect  := Rect(30, 1, 140, 26);
  NCControls[0].Caption     := 'nsSplitButton';
  NCControls[0].Name        := 'nsSplitButton';
  NCControls[0].OnClick     := ButtonNCClick;
  NCControls[0].OnDropDownClick := ButtonNCDropDownClick;

  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[1].Style       := nsPushButton;
  NCControls[1].ImageStyle  := isNormal;
  NCControls[1].Images      := ImageList1;
  NCControls[1].ImageIndex  := 1;
  NCControls[1].BoundsRect  := Rect(145, 1, 255, 26);
  NCControls[1].Caption     := 'nsPushButton';
  NCControls[1].Name        := 'nsPushButton';
  NCControls[1].OnClick     := ButtonNCClick;


  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[2].Style       := nsTranparent;
  NCControls[2].ImageStyle  := isGrayHot;
  NCControls[2].Images      := ImageList1;
  NCControls[2].ImageIndex  := 2;
  NCControls[2].BoundsRect  := Rect(260, 1, 370, 26);
  NCControls[2].Caption     := 'nsTranparent';
  NCControls[2].Name        := 'nsTranparent';
  NCControls[2].OnClick     := ButtonNCClick;

  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[3].Style       := nsSplitTrans;
  NCControls[3].ImageStyle  := isGrayHot;
  NCControls[3].Images      := ImageList1;
  NCControls[3].ImageIndex  := 3;
  NCControls[3].BoundsRect  := Rect(375, 1, 500, 26);
  NCControls[3].Caption     := 'nsSplitTrans';
  NCControls[3].Name        := 'nsSplitTrans';
  NCControls[3].OnClick     := ButtonNCClick;
  NCControls[3].OnDropDownClick := ButtonNCDropDownClick;

  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[4].Style        := nsAlpha;
  NCControls[4].AlphaColor   := clGreen;
  NCControls[4].AlphaHotColor:= clRed;
  NCControls[4].FontColor   := clWhite;
  NCControls[4].HotFontColor:= clYellow;
  NCControls[4].ImageStyle  := isGrayHot;
  NCControls[4].Images      := ImageList1;
  NCControls[4].ImageIndex  := 7;
  NCControls[4].BoundsRect  := Rect(505, 1, 570, 26);
  NCControls[4].Caption     := 'nsAlpha';
  NCControls[4].Name        := 'nsAlpha';
  NCControls[4].OnClick     := ButtonNCClick;

  NCControls.Add(TNCButton.Create(NCControls));
  NCControls[5].Style        := nsGradient;

  NCControls[5].StartColor   := clWebDarkOrange;
  NCControls[5].EndColor     := clRed;
  NCControls[5].Direction    := TGradientDirection.gdHorizontal;

  NCControls[5].FontColor    := clWhite;
  NCControls[5].HotFontColor := clYellow;

  NCControls[5].ImageStyle   := isGrayHot;
  NCControls[5].Images       := ImageList1;
  NCControls[5].ImageIndex   := 8;
  NCControls[5].BoundsRect   := Rect(575, 1, 660, 26);
  NCControls[5].Caption      := 'nsGradient';
  NCControls[5].Name         := 'nsGradient';
  NCControls[5].OnClick      := ButtonNCClick;
end;

end.
