unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtDlgs, Vcl.ExtCtrls,
  Vcl.Menus;

type
  TFrmMain = class(TForm)
    OpenPictureDialog1: TOpenPictureDialog;
    GroupBox1: TGroupBox;
    EditNCImage: TEdit;
    RadioButtonNCImage: TRadioButton;
    RadioButtonNCColor: TRadioButton;
    ColorBoxNC: TColorBox;
    BtnSetNCImage: TButton;
    GroupBox2: TGroupBox;
    EditBackImage: TEdit;
    RadioButtonBackImage: TRadioButton;
    RadioButtonBackColor: TRadioButton;
    ColorBoxBackground: TColorBox;
    BtnSetBackImage: TButton;
    CheckBoxNC: TCheckBox;
    CheckBoxBack: TCheckBox;
    ComboBoxStyles: TComboBox;
    Label1: TLabel;
    CheckBoxMerge: TCheckBox;
    procedure ColorBoxNCGetColors(Sender: TCustomColorBox; Items: TStrings);
    procedure ColorBoxNCChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure RadioButtonNCColorClick(Sender: TObject);
    procedure BtnSetNCImageClick(Sender: TObject);
    procedure CheckBoxNCClick(Sender: TObject);
    procedure BtnSetBackImageClick(Sender: TObject);
    procedure CheckBoxBackClick(Sender: TObject);
    procedure RadioButtonBackColorClick(Sender: TObject);
    procedure ColorBoxBackgroundChange(Sender: TObject);
    procedure ComboBoxStylesChange(Sender: TObject);
    procedure CheckBoxMergeClick(Sender: TObject);
  private
    procedure SetNCColor;
    procedure SetNCImage;

    procedure SetBackColor;
    procedure SetBackImage;

    procedure RefreshControls;
  public
    { Public declarations }
  end;

var
  FrmMain: TFrmMain;

implementation

uses
  Vcl.Styles.FormStyleHooks,
  Vcl.GraphUtil,
  Vcl.Styles,
  Vcl.Themes;

{$R *.dfm}

procedure TFrmMain.BtnSetNCImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    EditNCImage.Text := OpenPictureDialog1.FileName;
    SetNCImage;
  end;
end;

procedure TFrmMain.BtnSetBackImageClick(Sender: TObject);
begin
  if OpenPictureDialog1.Execute then
  begin
    EditBackImage.Text := OpenPictureDialog1.FileName;
    SetBackImage;
  end;
end;

procedure TFrmMain.CheckBoxBackClick(Sender: TObject);
begin
  TFormStyleHookColor.BackGroundSettings.Enabled := CheckBoxBack.Checked;
  RefreshControls;
end;

procedure TFrmMain.CheckBoxMergeClick(Sender: TObject);
begin
 TFormStyleHookColor.MergeImages:=CheckBoxMerge.Checked;
 SendMessage(Handle, WM_NCPAINT, 0, 0);
 RefreshControls;
end;

procedure TFrmMain.CheckBoxNCClick(Sender: TObject);
begin
  TFormStyleHookColor.NCSettings.Enabled := CheckBoxNC.Checked;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.ColorBoxBackgroundChange(Sender: TObject);
begin
  SetBackColor;
end;

procedure TFrmMain.ColorBoxNCChange(Sender: TObject);
begin
  SetNCColor;
end;

procedure TFrmMain.ColorBoxNCGetColors(Sender: TCustomColorBox;
  Items: TStrings);
Var
  Item: TIdentMapEntry;
begin
  Items.Clear;
  for Item in WebNamedColors do
    Items.AddObject(Item.Name, TObject(Item.Value));
end;

procedure TFrmMain.ComboBoxStylesChange(Sender: TObject);
begin
  TStyleManager.SetStyle(ComboBoxStyles.Text);
end;

procedure TFrmMain.FormCreate(Sender: TObject);
var
  style: string;
begin
  ReportMemoryLeaksOnShutdown := True;
  ColorBoxNC.Selected := TFormStyleHookColor.NCSettings.Color;
  ColorBoxBackground.Selected := TFormStyleHookColor.BackGroundSettings.Color;

  for style in TStyleManager.StyleNames do
    ComboBoxStyles.Items.Add(style);

  ComboBoxStyles.ItemIndex := ComboBoxStyles.Items.IndexOf
    (TStyleManager.ActiveStyle.Name);
end;

procedure TFrmMain.RadioButtonBackColorClick(Sender: TObject);
begin
  BtnSetBackImage.Enabled := not RadioButtonBackColor.Checked;
  EditBackImage.Enabled := not RadioButtonBackColor.Checked;
  ColorBoxBackground.Enabled := RadioButtonBackColor.Checked;

  TFormStyleHookColor.BackGroundSettings.UseColor :=
    RadioButtonBackColor.Checked;
  RefreshControls;
end;

procedure TFrmMain.RadioButtonNCColorClick(Sender: TObject);
begin
  BtnSetNCImage.Enabled := not RadioButtonNCColor.Checked;
  EditNCImage.Enabled := not RadioButtonNCColor.Checked;
  ColorBoxNC.Enabled := RadioButtonNCColor.Checked;
  TFormStyleHookColor.NCSettings.UseColor := RadioButtonNCColor.Checked;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.RefreshControls;
Var
  LIndex: Integer;
begin
  for LIndex := 0 to ComponentCount - 1 do
    if Components[LIndex] is TWinControl then
    begin
      TWinControl(Components[LIndex]).Invalidate;
      TWinControl(Components[LIndex]).Perform(WM_PAINT, 0, 0);
    end;

  Invalidate;
  SendMessage(Handle, WM_PAINT, 0, 0);
end;

procedure TFrmMain.SetBackColor;
begin
  TFormStyleHookColor.BackGroundSettings.UseColor := True;
  TFormStyleHookColor.BackGroundSettings.Color := ColorBoxBackground.Selected;
  RefreshControls;
end;

procedure TFrmMain.SetBackImage;
begin
  TFormStyleHookColor.BackGroundSettings.UseImage := True;
  TFormStyleHookColor.BackGroundSettings.ImageLocation := EditBackImage.Text;
  RefreshControls;
end;

procedure TFrmMain.SetNCColor;
begin
  TFormStyleHookColor.NCSettings.UseColor := True;
  TFormStyleHookColor.NCSettings.Color := ColorBoxNC.Selected;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

procedure TFrmMain.SetNCImage;
begin
  TFormStyleHookColor.NCSettings.UseImage := True;
  TFormStyleHookColor.NCSettings.ImageLocation := EditNCImage.Text;
  SendMessage(Handle, WM_NCPAINT, 0, 0);
end;

initialization

  TStyleManager.Engine.RegisterStyleHook(TFrmMain, TFormStyleHookColor);
  TFormStyleHookColor.NCSettings.Color := clWebDarkSlategray;
  TFormStyleHookColor.BackGroundSettings.Color := clWebDarkOliveGreen;
  TFormStyleHookColor.MergeImages:=True;

end.
