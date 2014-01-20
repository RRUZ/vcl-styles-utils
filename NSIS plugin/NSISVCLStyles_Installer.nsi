  !include "MUI2.nsh"
  !addplugindir "Win32\Release" 
;--------------------------------
;General

  ;Name and file
  Name "VCL Styles for NSIS"
  OutFile "Output\NSISVCLStyles.exe"
  SetCompressor lzma
  InstallDir "$PROGRAMFILES\The Road To Delphi\NSISVCLStyles"
  InstallDirRegKey HKCU "Software\NSISVCLStyles" ""
  RequestExecutionLevel admin
  !define _VERSION "1.0.0.2"
  VIProductVersion "${_VERSION}"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "ProductName" "VCL Styles for NSIS"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "CompanyName" "The Road To Delphi"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "FileVersion" "${_VERSION}"
  VIAddVersionKey /LANG=${LANG_ENGLISH} "InternalName" "NSISVCLStyles.exe"
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

Section "" 
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
    SetOutPath "${NSISDIR}\Plugins"	
    File "Win32\Release\NSISVCLStyles.dll"
	SetOutPath "$INSTDIR\Scripts"
    File "Scripts\example1.nsi"
    File "Scripts\example1_SkinUninstaller.nsi"	
	SetOutPath "$INSTDIR\Scripts\Modern UI"	
    File "Scripts\Modern UI\Basic.nsi"		
    File "Scripts\Modern UI\HeaderBitmap.nsi"		
    File "Scripts\Modern UI\MultiLanguage.nsi"		
    File "Scripts\Modern UI\StartMenu.nsi"		
    File "Scripts\Modern UI\WelcomeFinish.nsi"		
	;Store installation folder
  WriteRegStr HKCU "Software\NSISVCLStyles" "" $INSTDIR
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
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
  Delete "${NSISDIR}\Plugins\NSISVCLStyles.dll"
  DeleteRegKey /ifempty HKCU "Software\NSISVCLStyles"
SectionEnd

Function .onInit
  SetOutPath $TEMP
  File /oname=Amakrits.vsf "..\Styles\Amakrits.vsf"
  NSISVCLStyles::LoadVCLStyleA /NOUNLOAD $TEMP\Amakrits.vsf
  Delete $TEMP\Amakrits.vsf
FunctionEnd

Function un.onInit
 SetOutPath $TEMP
 File /oname=Amakrits.vsf "..\Styles\Amakrits.vsf"
 NSISVCLStyles::LoadVCLStyleA /NOUNLOAD $TEMP\Amakrits.vsf
FunctionEnd
