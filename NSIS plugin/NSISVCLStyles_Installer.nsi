  
  !include "MUI2.nsh"
  !addplugindir "Win32\Release_ANSI" 
;--------------------------------
;General
  SetCompressor lzma
  ;!insertmacro MUI_LANGUAGE "English"
  ;Name and file
  Name "VCL Styles for NSIS"
  OutFile "Output\NSISVCLStyles.exe"  
  InstallDir "$PROGRAMFILES\The Road To Delphi\NSISVCLStyles"
  InstallDirRegKey HKCU "Software\NSISVCLStyles" ""
  RequestExecutionLevel admin
  !define _VERSION "1.0.0.9"
  VIProductVersion "${_VERSION}"
  VIAddVersionKey  "ProductName" "VCL Styles for NSIS"
  VIAddVersionKey  "CompanyName" "The Road To Delphi"
  VIAddVersionKey  "FileVersion" "${_VERSION}"
  VIAddVersionKey  "InternalName" "NSISVCLStyles.exe"
  VIAddVersionKey  "FileDescription" "VCL Styles for NSIS"
  VIAddVersionKey  "LegalCopyright" "MPL 1.1"  
;--------------------------------
;Interface Settings

  !define MUI_ABORTWARNING

;--------------------------------
;Pages

  ;!insertmacro MUI_PAGE_COMPONENTS
  !insertmacro MUI_PAGE_DIRECTORY
  !insertmacro MUI_PAGE_INSTFILES
  
  !insertmacro MUI_UNPAGE_CONFIRM
  !insertmacro MUI_UNPAGE_INSTFILES
  
;--------------------------------
;Languages
 
  !insertmacro MUI_LANGUAGE "English"

;--------------------------------
;Installer Sections
Var NSIS
Var NSISMajorVersion

Section "" 		
    ReadRegDWORD $NSISMajorVersion HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\\NSIS" "VersionMajor"
	;IfErrors 0 done 
	;ClearErrors	
	;MessageBox MB_OK "$NSISMajorVersion"
	
    ReadRegStr $NSIS HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\\NSIS" "InstallLocation"
    IfErrors 0 begin
    ClearErrors
    begin:
	;MessageBox MB_OK "$NSIS"
	IfFileExists "$NSIS" 0 done
    ;DetailPrint "$NSIS"
	;MessageBox MB_OK "$NSIS"
	
	${If} $NSISMajorVersion > 2
	IfFileExists "$NSIS\Plugins\x86-ansi" 0 done
	SetOutPath "$NSIS\Plugins\x86-ansi"
	File "Win32\Release_ANSI\NSISVCLStyles.dll"	

	IfFileExists "$NSIS\Plugins\x86-unicode" 0 done
	SetOutPath "$NSIS\Plugins\x86-unicode"
	File "Win32\Release_UNICODE\NSISVCLStyles.dll"	
	${Else}
	IfFileExists "$NSIS\Plugins" 0 done
	SetOutPath "$NSIS\Plugins"
	File "Win32\Release_ANSI\NSISVCLStyles.dll"		
	${EndIf}

    SetOutPath "$INSTDIR"
    ;ADD YOUR OWN FILES HERE...
    File "c:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\VclStyleDesigner.exe"
    File "c:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\VclStyleTest.exe"		
	SetOutPath "$INSTDIR\Styles"
	File "..\Styles\Amakrits.vsf"
	File "..\Styles\AmethystKamri.vsf"
	File "..\Styles\AquaGraphite.vsf"
	File "..\Styles\AquaLightSlate.vsf"
	File "..\Styles\Auric.vsf"
	File "..\Styles\BlueGraphite.vsf"
	File "..\Styles\Carbon.vsf"
	File "..\Styles\CharcoalDarkSlate.vsf"
	File "..\Styles\CobaltXEMedia.vsf"
	File "..\Styles\CyanDusk.vsf"
	File "..\Styles\CyanNight.vsf"
	File "..\Styles\EmeraldLightSlate.vsf"
	File "..\Styles\GoldenGraphite.vsf"
	File "..\Styles\GreenGraphite.vsf"
	File "..\Styles\IcebergClassico.vsf"
	File "..\Styles\khaki.vsf"
	File "..\Styles\LavenderClassico.vsf"
	File "..\Styles\LightGreen.vsf"
	File "..\Styles\lilac.vsf"
	File "..\Styles\MetroBlack.vsf"
	File "..\Styles\MetroBlue.vsf"
	File "..\Styles\MetroGreen.vsf"
	File "..\Styles\Orange.vsf"
	File "..\Styles\OrangeGraphite.vsf"
	File "..\Styles\Pink.vsf"
	File "..\Styles\RubyGraphite.vsf"
	File "..\Styles\SapphireKamri.vsf"
	File "..\Styles\sepia.vsf"
	File "..\Styles\Sky.vsf"
	File "..\Styles\SlateClassico.vsf"
	File "..\Styles\SmokeyQuartzKamri.vsf"
	File "..\Styles\TurquoiseGray.vsf"
	File "..\Styles\YellowGraphite.vsf"  
	SetOutPath "$INSTDIR\Scripts"
    File "Scripts\example1.nsi"
    File "Scripts\example1_SkinUninstaller.nsi"	
    File "Scripts\unicode.nsi"		
	SetOutPath "$INSTDIR\Scripts\Modern UI"	
    File "Scripts\Modern UI\Basic.nsi"		
    File "Scripts\Modern UI\HeaderBitmap.nsi"		
    File "Scripts\Modern UI\MultiLanguage.nsi"		
    File "Scripts\Modern UI\StartMenu.nsi"		
    File "Scripts\Modern UI\WelcomeFinish.nsi"		
	SetOutPath "$INSTDIR\Scripts\nsDialogs"	
    File "Scripts\nsDialogs\example.nsi"		
    File "Scripts\nsDialogs\InstallOptions.nsi"	
    File "Scripts\nsDialogs\timer.nsi"	
    File "Scripts\nsDialogs\welcome.nsi"	
    File "Scripts\nsDialogs\welcome_colors.nsi"	
	;Store installation folder
    WriteRegStr HKCU "Software\NSISVCLStyles" "" $INSTDIR
	WriteUninstaller "$INSTDIR\Uninstall.exe"
    Goto completed	
    ;Create uninstaller
    done:
    DetailPrint "NSIS installation data was not found"	
	;MessageBox MB_OK "File not found."
	completed:
SectionEnd

;--------------------------------
;Descriptions

  ;Assign language strings to sections
  ;!insertmacro MUI_FUNCTION_DESCRIPTION_BEGIN
  ;!insertmacro MUI_DESCRIPTION_TEXT ${SecDummy} $(DESC_SecDummy)
  ;!insertmacro MUI_FUNCTION_DESCRIPTION_END

;--------------------------------
;Uninstaller Section

Section "Uninstall"   
  ;ADD YOUR OWN FILES HERE...
  Delete "$INSTDIR\Uninstall.exe"
  ;Remove all the Program Files.
  ;RMDir /r $INSTDIR\Scripts\Modern UI  
  RMDir /r $INSTDIR\Scripts  
  RMDir /r $INSTDIR\Styles    
  RMDir /r $INSTDIR  
  
  ReadRegStr $NSIS HKLM "SOFTWARE\Microsoft\Windows\CurrentVersion\Uninstall\\NSIS" "InstallLocation"
  IfErrors 0 begin
  ClearErrors
  begin:  
  Delete "$NSIS\Plugins\NSISVCLStyles.dll"    
  Delete "$NSIS\Plugins\x86-ansi\NSISVCLStyles.dll"  
  Delete "$NSIS\Plugins\x86-unicode\NSISVCLStyles.dll"
  DeleteRegKey /ifempty HKCU "Software\NSISVCLStyles"
SectionEnd

Function .onInit
  InitPluginsDir
  File /oname=$PLUGINSDIR\Amakrits.vsf "..\Styles\Amakrits.vsf"
  NSISVCLStyles::LoadVCLStyle $PLUGINSDIR\Amakrits.vsf  
FunctionEnd

Function un.onInit
  InitPluginsDir
  File /oname=$PLUGINSDIR\Amakrits.vsf "..\Styles\Amakrits.vsf"
  NSISVCLStyles::LoadVCLStyle  $PLUGINSDIR\Amakrits.vsf
FunctionEnd
