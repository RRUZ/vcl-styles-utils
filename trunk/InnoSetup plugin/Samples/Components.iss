; -- Components.iss --
; Demonstrates a components-based installation.

; SEE THE DOCUMENTATION FOR DETAILS ON CREATING .ISS SCRIPT FILES!

[Setup]
AppName=My Program
AppVersion=1.5
DefaultDirName={pf}\My Program
DefaultGroupName=My Program
UninstallDisplayIcon={app}\MyProg.exe
OutputDir=userdocs:Inno Setup Examples Output

[Types]
Name: full; Description: Full installation
Name: compact; Description: Compact installation
Name: custom; Description: Custom installation; Flags: iscustom

[Components]
Name: program; Description: Program Files; Types: full compact custom; Flags: fixed
Name: help; Description: Help File; Types: full
Name: readme; Description: Readme File; Types: full
Name: readme\en; Description: English; Flags: exclusive
Name: readme\de; Description: German; Flags: exclusive

[Files]
Source: MyProg.exe; DestDir: {app}; Components: program
Source: MyProg.chm; DestDir: {app}; Components: help
Source: Readme.txt; DestDir: {app}; Components: readme\en; Flags: isreadme
Source: Readme-German.txt; DestName: Liesmich.txt; DestDir: {app}; Components: readme\de; Flags: isreadme
Source: ..\VclStylesinno.dll; DestDir: {app}; Flags: dontcopy
;Source: ..\Win32\Release\VclStylesinno.dll; DestDir: {app}; Flags: dontcopy
Source: ..\Styles\Amakrits.vsf; DestDir: {app}; Flags: dontcopy
[Icons]
Name: {group}\My Program; Filename: {app}\MyProg.exe

[Code]

//Import the LoadVCLStyle function from VclStylesInno.DLL
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
