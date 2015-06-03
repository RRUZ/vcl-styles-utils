// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
// JCL_DEBUG_EXPERT_GENERATEJDBG OFF
program Demo;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FrmMain},
  System.IOUtils,
  System.SysUtils,
  System.Types,
  Windows,
  Vcl.Themes,
  Vcl.Styles,
  Vcl.Styles.FormStyleHooks in '..\Common\Vcl.Styles.FormStyleHooks.pas',
  Vcl.Styles.NC in '..\Common\Vcl.Styles.NC.pas',
  Vcl.Styles.Utils.SystemMenu in '..\Common\Vcl.Styles.Utils.SystemMenu.pas',
  uDropdown in 'uDropdown.pas' {FrmDropDown},
  uButtonsStyles in 'uButtonsStyles.pas' {FrmButtonsStyles},
  uCustomStyles in 'uCustomStyles.pas' {FrmCustomStyles},
  Vcl.Styles.Utils.Menus in '..\Common\Vcl.Styles.Utils.Menus.pas',
  Vcl.Styles.Utils.SysControls in '..\Common\Vcl.Styles.Utils.SysControls.pas',
  Vcl.Styles.Utils.SysStyleHook in '..\Common\Vcl.Styles.Utils.SysStyleHook.pas',
  Vcl.Styles.Utils.Graphics in '..\Common\Vcl.Styles.Utils.Graphics.pas',
  DDetours in '..\Common\delphi-detours-library\DDetours.pas',
  uAlphaGradient in 'uAlphaGradient.pas' {FrmAlphaGradient},
  uButtonsTabsStyles in 'uButtonsTabsStyles.pas' {FrmButtonsTabsStyle};

{$R *.res}

function PathCanonicalize(lpszDst: PChar; lpszSrc: PChar): LongBool; stdcall; external 'shlwapi.dll' name 'PathCanonicalizeW';

function ResolvePath(const RelPath, BasePath: string): string;
var
  lpszDst: array[0..MAX_PATH-1] of char;
begin
  PathCanonicalize(@lpszDst[0], PChar(IncludeTrailingPathDelimiter(BasePath) + RelPath));
  Exit(lpszDst);
end;

procedure LoadVCLStyles;
var
  f, s : string;
  LFiles : TStringDynArray;
begin
  s:=ExtractFilePath(ParamStr(0));
  LFiles:=TDirectory.GetFiles(s, '*.vsf');
  if Length(LFiles)>0 then
   for f in TDirectory.GetFiles(s, '*.vsf') do
     TStyleManager.LoadFromFile(f)
  else
  begin
    s:=ResolvePath('..\..\..\Styles',ExtractFilePath(ParamStr(0)));
    for f in TDirectory.GetFiles(s, '*.vsf') do
      TStyleManager.LoadFromFile(f);
  end;
end;

begin
  LoadVCLStyles;
  TStyleManager.TrySetStyle('Auric');
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFrmMain, FrmMain);
  Application.Run;
end.
