unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ImgList,
  Vcl.ExtCtrls, Vcl.Mask;

type
  TFrmMain = class(TForm)
    ListBox1: TListBox;
    Edit1: TEdit;
    ComboBox1: TComboBox;
    ComboBoxEx1: TComboBoxEx;
    ImageList1: TImageList;
    Memo1: TMemo;
    MaskEdit1: TMaskEdit;
    ColorBox1: TColorBox;
    RichEdit1: TRichEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
    procedure CMStyleChanged(var Message: TMessage); message CM_STYLECHANGED;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.Hooks;

{$R *.dfm}

procedure TFrmMain.Button1Click(Sender: TObject);
begin
//  TStyleManager.SetStyle('Windows');
end;

procedure TFrmMain.CMStyleChanged(var Message: TMessage);
begin

end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  i :   integer;
  ExItem : TComboExItem;
begin
  ReportMemoryLeaksOnShutdown:=True;
  for i := 1 to 20 do
    ListBox1.Items.Add(Format('Item %d',[i]));

  for i := 1 to 20 do
    ComboBox1.Items.Add(Format('Item %d',[i]));

  for i := 0 to 6 do
  begin
   ExItem:= ComboBoxEx1.ItemsEx.Add;
   ExItem.Caption := Format('Item %d',[i+1]);
   ExItem.ImageIndex := i;
  end;

 with RichEdit1 do
 begin
   SelStart := GetTextLen;

   SelAttributes.Size := 13;

   SelAttributes.Style := [fsBold];
   SelAttributes.Color := clRed;
   SelText := 'VCL Styles ';

   SelAttributes.Color := clGreen;
   SelText := 'RichEdit';

   SelAttributes.Color := clWindowText;
   SelText := ' Delphi ';

   SelAttributes.Style := [fsItalic];
   SelAttributes.Color := clBlue;
   SelText := 'Demo';

   SelText := #13#10;

   SelAttributes.Size := 8;
   SelAttributes.Color := clYellow;
   SelText := 'a final line';
 end;
end;


end.
