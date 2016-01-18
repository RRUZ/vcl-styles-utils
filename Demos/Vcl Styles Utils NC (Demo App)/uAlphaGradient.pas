unit uAlphaGradient;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.StdCtrls;

type
  TFrmAlphaGradient = class(TForm)
    ImageList1: TImageList;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    NCControls : TNCControls;
     procedure ButtonNCClick(Sender: TObject);
 public
    { Public declarations }
  end;

implementation

uses
 Vcl.GraphUtil;


{$R *.dfm}

procedure TFrmAlphaGradient.ButtonNCClick(Sender: TObject);
begin
 if Sender is TNCButton then
  ShowMessage(Format('You clicked the %s button', [TNCButton(Sender).Name]));
end;

procedure TFrmAlphaGradient.FormCreate(Sender: TObject);
begin
  NCControls:=TNCControls.Create(Self);
  NCControls.Images      := ImageList1;
  NCControls.ButtonsList.Add;
  NCControls[0].Style       := nsAlpha;
  NCControls[0].ImageStyle  := isNormal;
  NCControls[0].ImageIndex  := 0;
  NCControls[0].BoundsRect  := Rect(30, 5, 120, 26);
  NCControls[0].Caption     := 'nsAlpha1';
  NCControls[0].Name        := 'nsAlpha1';
  NCControls[0].AlphaColor   := clWebLavender;
  NCControls[0].AlphaHotColor:= clWebAliceBlue;
  NCControls[0].FontColor   := clWhite;
  NCControls[0].HotFontColor:= clYellow;
  NCControls[0].OnClick     := ButtonNCClick;

  NCControls.ButtonsList.Add;
  NCControls[1].Style       := nsAlpha;
  NCControls[1].ImageStyle  := isGrayHot;
  NCControls[1].ImageIndex  := 1;
  NCControls[1].BoundsRect  := Rect(125, 5, 215, 26);
  NCControls[1].Caption     := 'nsAlpha2';
  NCControls[1].Name        := 'nsAlpha2';
  NCControls[1].AlphaColor   := clWebOrange;
  NCControls[1].AlphaHotColor:= clWebOrangeRed;
  NCControls[1].FontColor   := clWebWhite;
  NCControls[1].HotFontColor:= clWebWhite;
  NCControls[1].OnClick     := ButtonNCClick;


  NCControls.ButtonsList.Add;
  NCControls[2].Style       := nsAlpha;
  NCControls[2].ImageStyle  := isGrayHot;
  NCControls[2].ImageIndex  := 2;
  NCControls[2].BoundsRect  := Rect(220, 5, 310, 26);
  NCControls[2].Caption     := 'nsAlpha3';
  NCControls[2].Name        := 'nsAlpha3';
  NCControls[2].AlphaColor   := clWebGreenYellow;
  NCControls[2].AlphaHotColor:= clWebDarkOrange;
  NCControls[2].FontColor   := clWebWhite;
  NCControls[2].HotFontColor:= clWebWhite;
  NCControls[2].OnClick     := ButtonNCClick;

  NCControls.ButtonsList.Add;
  NCControls[3].Style        := nsGradient;
  NCControls[3].StartColor   := clWebSkyBlue;
  NCControls[3].EndColor     := clWebChocolate;
  NCControls[3].Direction    := TGradientDirection.gdHorizontal;
  NCControls[3].FontColor    := clWhite;
  NCControls[3].HotFontColor := clYellow;
  NCControls[3].ImageStyle  := isGrayHot;
  NCControls[3].ImageIndex  := 3;
  NCControls[3].BoundsRect  := Rect(315, 5, 415, 26);
  NCControls[3].Caption     := 'nsGradient1';
  NCControls[3].Name        := 'nsGradient1';
  NCControls[3].OnClick     := ButtonNCClick;

  NCControls.ButtonsList.Add;
  NCControls[4].Style        := nsGradient;
  NCControls[4].StartColor   := clWebSeashell;
  NCControls[4].EndColor     := clWebGray;
  NCControls[4].Direction    := TGradientDirection.gdVertical;
  NCControls[4].FontColor   := clWhite;
  NCControls[4].HotFontColor:= clYellow;
  NCControls[4].ImageStyle  := isGrayHot;
  NCControls[4].ImageIndex  := 7;
  NCControls[4].BoundsRect  := Rect(420, 5, 520, 26);
  NCControls[4].Caption     := 'nsGradient2';
  NCControls[4].Name        := 'nsGradient2';
  NCControls[4].OnClick     := ButtonNCClick;


  NCControls.ButtonsList.Add;
  NCControls[5].Style        := nsGradient;
  NCControls[5].StartColor   := clWebDarkOrange;
  NCControls[5].EndColor     := clRed;
  NCControls[5].Direction    := TGradientDirection.gdHorizontal;
  NCControls[5].FontColor    := clWhite;
  NCControls[5].HotFontColor := clYellow;
  NCControls[5].ImageStyle   := isGrayHot;
  NCControls[5].ImageIndex   := 8;
  NCControls[5].BoundsRect   := Rect(525, 5, 625, 26);
  NCControls[5].Caption      := 'nsGradient3';
  NCControls[5].Name         := 'nsGradient3';
  NCControls[5].OnClick      := ButtonNCClick;
end;

end.
