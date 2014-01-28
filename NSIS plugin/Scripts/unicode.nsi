; unicode is not enabled by default
; unicode installers will not be able to run on Windows 9x!
Unicode true
!addplugindir "..\Win32\Release_UNICODE" 
Name "Unicode Games"
OutFile "unicode.exe"

ShowInstDetails show

XPStyle on

Section "Unicode in UI"

	DetailPrint "Hello World!"
	DetailPrint "שלום עולם!"
	DetailPrint "مرحبا العالم!"
	DetailPrint "こんにちは、世界！"
	DetailPrint "你好世界！"
	DetailPrint "привет мир!"
	DetailPrint "안녕하세요!"

	DetailPrint "${U+00A9}" # arbitrary unicode chars

SectionEnd

Section "Unicode in Files"

	# TODO add file I/O unicode function examples

SectionEnd


Function .onInit
   InitPluginsDir
   ;Get the skin file to use
   File /oname=$PLUGINSDIR\Amakrits.vsf "..\Styles\Amakrits.vsf"
   ;Load the skin using the LoadVCLStyle function
   NSISVCLStyles::LoadVCLStyle $PLUGINSDIR\Amakrits.vsf  
FunctionEnd
