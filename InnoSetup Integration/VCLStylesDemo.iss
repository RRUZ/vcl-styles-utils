; -- Example1.iss --
; Demonstrates copying 3 files and creating an icon.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=VCL Styles Example
AppVerName=VCL Styles Example v1.0
AppVersion=1.0.0.0
AppCopyright=The Road To Delphi
DefaultDirName={pf}\The Road To Delphi
DefaultGroupName=The Road To Delphi
Compression=lzma
;Compression=none
SolidCompression=true
WizardImageFile=images\WizModernImage-IS_BW.bmp
WizardSmallImageFile=images\WizModernSmallImage-IS_BW.bmp
OutputDir=.\Output
OutputBaseFilename=Setup
VersionInfoVersion=1.0.0.0
VersionInfoCompany=The Road To Delphi
VersionInfoDescription=VCL Styles Setup
VersionInfoTextVersion=1, 0, 0, 0
;#define VCLStylesPath "{localappdata}\VCLStylesPath"
InternalCompressLevel=max


[Files]
Source: Win32\Release\VclStylesinno.dll; DestDir: {app}; Flags: dontcopy
;Source: Win32\Debug\VclStylesinno.dll; DestDir: {app}; Flags: dontcopy
;Source: ..\Styles\LavenderClassico.vsf; DestDir: {app}
;Source: ..\Styles\LightGreen.vsf; DestDir: {app}
;Source: ..\Styles\lilac.vsf; DestDir: {app}
;Source: ..\Styles\MetroBlack.vsf; DestDir: {app}
;Source: ..\Styles\MetroBlue.vsf; DestDir: {app}
;Source: ..\Styles\MetroGreen.vsf; DestDir: {app}
;Source: ..\Styles\Orange.vsf; DestDir: {app}
;Source: ..\Styles\OrangeGraphite.vsf; DestDir: {app}
;Source: ..\Styles\Pink.vsf; DestDir: {app}
;Source: ..\Styles\RubyGraphite.vsf; DestDir: {app}
;Source: ..\Styles\SapphireKamri.vsf; DestDir: {app}
;Source: ..\Styles\sepia.vsf; DestDir: {app}
;Source: ..\Styles\Sky.vsf; DestDir: {app}
;Source: ..\Styles\SlateClassico.vsf; DestDir: {app}
;Source: ..\Styles\SmokeyQuartzKamri.vsf; DestDir: {app}
;Source: ..\Styles\TurquoiseGray.vsf; DestDir: {app}
;Source: ..\Styles\YellowGraphite.vsf; DestDir: {app}
Source: ..\Styles\Amakrits.vsf; DestDir: {app}; Flags: dontcopy
;Source: ..\Styles\AmethystKamri.vsf; DestDir: {app}
;Source: ..\Styles\AquaGraphite.vsf; DestDir: {app}
;Source: ..\Styles\AquaLightSlate.vsf; DestDir: {app}
;Source: ..\Styles\Auric.vsf; DestDir: {app}
;Source: ..\Styles\BlueGraphite.vsf; DestDir: {app}
;Source: ..\Styles\Carbon.vsf; DestDir: {app}
;Source: ..\Styles\CharcoalDarkSlate.vsf; DestDir: {app}
;Source: ..\Styles\CobaltXEMedia.vsf; DestDir: {app}
;Source: ..\Styles\CyanDusk.vsf; DestDir: {app}
;Source: ..\Styles\CyanNight.vsf; DestDir: {app}
;Source: ..\Styles\EmeraldLightSlate.vsf; DestDir: {app}
;Source: ..\Styles\GoldenGraphite.vsf; DestDir: {app}
;Source: ..\Styles\GreenGraphite.vsf; DestDir: {app}
;Source: ..\Styles\IcebergClassico.vsf; DestDir: {app}
;Source: ..\Styles\khaki.vsf; DestDir: {app}
;Source: Win32\Debug\DllHookTest.dll; DestDir: {app}

[Icons]
Name: {group}\Uninstall =ISSkin; Filename: {app}\unins000.exe


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
