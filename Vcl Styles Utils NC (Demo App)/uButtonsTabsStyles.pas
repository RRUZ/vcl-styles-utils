unit uButtonsTabsStyles;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Styles.NC, Vcl.ImgList, Vcl.ComCtrls,
  Vcl.ExtCtrls;

type
  TFrmButtonsTabsStyle = class(TForm)
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
  private
    NCControls : TNCControls;
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TFrmButtonsTabsStyle.FormCreate(Sender: TObject);
begin
  NCControls:=TNCControls.Create(Self);
  NCControls.Images      := ImageList1;
  NCControls.ButtonsList.Add;
  NCControls[0].Style       := nsTab;
  NCControls[0].ImageStyle  := isGrayHot;
  NCControls[0].ImageIndex  := 0;
  NCControls[0].BoundsRect  := Rect(30, 1, 140, 26);
  NCControls[0].Caption     := 'Text Tab1';
  NCControls[0].Name        := 'nsTab1';
  //NCControls[0].OnClick     := ButtonNCClick;
  //NCControls[0].OnDropDownClick := ButtonNCDropDownClick;

  NCControls.ButtonsList.Add;
  NCControls[1].Style       := nsTab;
  NCControls[1].ImageStyle  := isGrayHot;
  NCControls[1].ImageIndex  := 1;
  NCControls[1].BoundsRect  := Rect(141, 1, 251, 26);
  NCControls[1].Caption     := 'Text Tab2';
  NCControls[1].Name        := 'nsTab2';
  //NCControls[1].OnClick     := ButtonNCClick;

  NCControls.ButtonsList.Add;
  NCControls[2].Style       := nsTab;
  NCControls[2].ImageStyle  := isGrayHot;
  NCControls[2].ImageIndex  := 3;
  NCControls[2].BoundsRect  := Rect(252, 1, 362, 26);
  NCControls[2].Caption     := 'Text Tab3';
  NCControls[2].Name        := 'nsTab3';
  //NCControls[2].OnClick     := ButtonNCClick;
end;

end.
