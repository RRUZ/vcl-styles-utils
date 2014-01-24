; example1_SkinUninstaller.nsi
;

;--------------------------------
CRCCheck off
!addplugindir "..\Win32\Release" 
; The name of the installer
Name "Example1"

; The file to write
OutFile "example1.exe"

; The default installation directory
InstallDir $DESKTOP\Example1

; Request application privileges for Windows Vista
RequestExecutionLevel user

;--------------------------------

; Pages

Page directory
Page instfiles

;--------------------------------

; The stuff to install
Section "" ;No components page, name is not important

  ; Set output path to the installation directory.
  SetOutPath $INSTDIR
  
  ; Put file there
  File example1_SkinUninstaller.nsi
  
  ;Create uninstaller
  WriteUninstaller "$INSTDIR\Uninstall.exe"
  
SectionEnd ; end the section

Function .onInit
  InitPluginsDir
  ;Get the skin file to use
  File /oname=$PLUGINSDIR\Amakrits.vsf "..\Styles\Amakrits.vsf"
  ;Load the skin using the LoadVCLStyleA function
  NSISVCLStyles::LoadVCLStyleA $PLUGINSDIR\Amakrits.vsf  
FunctionEnd

Function un.onInit
  InitPluginsDir
  File /oname=$PLUGINSDIR\Amakrits.vsf "..\Styles\Amakrits.vsf"
  NSISVCLStyles::LoadVCLStyleA  $PLUGINSDIR\Amakrits.vsf
FunctionEnd
