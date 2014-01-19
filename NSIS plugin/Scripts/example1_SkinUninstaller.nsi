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
  SetOutPath $TEMP
  File /oname=Amakrits.vsf "Amakrits.vsf"
  NSISVCLStyles::LoadVCLStyleA /NOUNLOAD $TEMP\Amakrits.vsf
  Delete $TEMP\Amakrits.vsf
FunctionEnd

Function un.onInit
 SetOutPath $TEMP
 File /oname=Amakrits.vsf "Amakrits.vsf"
 NSISVCLStyles::LoadVCLStyleA /NOUNLOAD $TEMP\Amakrits.vsf
FunctionEnd
