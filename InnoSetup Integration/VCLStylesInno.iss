#define MyAppName 'VCL Styles for Inno Setup'
#define MyAppVersion GetFileVersion('Win32\Release\VclStylesinno.dll')
[Setup]
AppName={#MyAppName}
AppVerName={#MyAppName} {#MyAppVersion}
AppVersion={#MyAppVersion}
AppCopyright=The Road To Delphi
DefaultDirName={pf}\The Road To Delphi\VCL Styles Inno
DefaultGroupName=The Road To Delphi
Compression=lzma
SolidCompression=true
WizardImageFile=images\WizModernImage-IS_Green.bmp
WizardSmallImageFile=images\WizModernSmallImage-IS_Green.bmp
OutputDir=.\Output
OutputBaseFilename=SetupVCLStylesInno
VersionInfoVersion={#MyAppVersion}
VersionInfoCompany=The Road To Delphi
VersionInfoDescription=VCL Styles for Inno Setup
InternalCompressLevel=max

[Files]
Source: Win32\Release\VclStylesinno.dll; DestDir: {app}
;Source: Win32\Debug\VclStylesinno.dll; DestDir: {app}
Source: Styles\Amakrits.vsf; DestDir: {app}\Styles\
Source: Styles\AmethystKamri.vsf; DestDir: {app}\Styles\
Source: Styles\AquaGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\AquaLightSlate.vsf; DestDir: {app}\Styles\
Source: Styles\Auric.vsf; DestDir: {app}\Styles\
Source: Styles\BlueGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\Carbon.vsf; DestDir: {app}\Styles\
Source: Styles\CharcoalDarkSlate.vsf; DestDir: {app}\Styles\
Source: Styles\CobaltXEMedia.vsf; DestDir: {app}\Styles\
Source: Styles\CyanDusk.vsf; DestDir: {app}\Styles\
Source: Styles\CyanNight.vsf; DestDir: {app}\Styles\
Source: Styles\EmeraldLightSlate.vsf; DestDir: {app}\Styles\
Source: Styles\GoldenGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\GreenGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\IcebergClassico.vsf; DestDir: {app}\Styles\
Source: Styles\khaki.vsf; DestDir: {app}\Styles\
Source: Styles\LavenderClassico.vsf; DestDir: {app}\Styles\
Source: Styles\LightGreen.vsf; DestDir: {app}\Styles\
Source: Styles\lilac.vsf; DestDir: {app}\Styles\
Source: Styles\MetroBlack.vsf; DestDir: {app}\Styles\
Source: Styles\MetroBlue.vsf; DestDir: {app}\Styles\
Source: Styles\MetroGreen.vsf; DestDir: {app}\Styles\
Source: Styles\Orange.vsf; DestDir: {app}\Styles\
Source: Styles\OrangeGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\Pink.vsf; DestDir: {app}\Styles\
Source: Styles\RubyGraphite.vsf; DestDir: {app}\Styles\
Source: Styles\SapphireKamri.vsf; DestDir: {app}\Styles\
Source: Styles\sepia.vsf; DestDir: {app}\Styles\
Source: Styles\Sky.vsf; DestDir: {app}\Styles\
Source: Styles\SlateClassico.vsf; DestDir: {app}\Styles\
Source: Styles\SmokeyQuartzKamri.vsf; DestDir: {app}\Styles\
Source: Styles\TurquoiseGray.vsf; DestDir: {app}\Styles\
Source: Styles\YellowGraphite.vsf; DestDir: {app}\Styles\
Source: Samples\CodeClasses.iss; DestDir: {app}\Samples\
Source: Samples\VCLStylesDemo.iss; DestDir: {app}\Samples\
Source: Samples\Readme-German.txt; DestDir: {app}\Samples\
Source: Samples\Components.iss; DestDir: {app}\Samples\
Source: Samples\MyProg.chm; DestDir: {app}\Samples\
Source: Samples\MyProg.exe; DestDir: {app}\Samples\
Source: Samples\Readme.txt; DestDir: {app}\Samples\
Source: Samples\Readme-Dutch.txt; DestDir: {app}\Samples\
Source: Images\WizModernImage-IS.bmp; DestDir: {app}\Images\
Source: Images\WizModernImage-IS_BW.bmp; DestDir: {app}\Images\
Source: Images\WizModernImage-IS_Green.bmp; DestDir: {app}\Images\
Source: Images\WizModernImage-IS_Orange.bmp; DestDir: {app}\Images\
Source: Images\WizModernImage-IS_Purple.bmp; DestDir: {app}\Images\
Source: Images\WizModernSmallImage-IS.bmp; DestDir: {app}\Images\
Source: Images\WizModernSmallImage-IS_BW.bmp; DestDir: {app}\Images\
Source: Images\WizModernSmallImage-IS_Green.bmp; DestDir: {app}\Images\
Source: Images\WizModernSmallImage-IS_Orange.bmp; DestDir: {app}\Images\
Source: Images\WizModernSmallImage-IS_Purple.bmp; DestDir: {app}\Images\

[Code]
// Import the LoadVCLStyle function from VclStylesInno.DLL
procedure LoadVCLStyle(VClStyleFile: String); external 'LoadVCLStyleW@files:VclStylesInno.dll stdcall';
// Import the UnLoadVCLStyles function from VclStylesInno.DLL
procedure UnLoadVCLStyles; external 'UnLoadVCLStyles@files:VclStylesInno.dll stdcall';

function InitializeSetup(): Boolean;
begin
	ExtractTemporaryFile('Amakrits.vsf');
	LoadVCLStyle(ExpandConstant('{tmp}\Amakrits.vsf'));
	Result := True;
end;

procedure DeinitializeSetup();
begin
	UnLoadVCLStyles;
end;
[Dirs]
Name: {app}\Styles
Name: {app}\Samples
Name: {app}\Images
