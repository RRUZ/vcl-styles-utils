BRCC32 VersionInfoUNICODE.rc
call "C:\Program Files (x86)\Embarcadero\RAD Studio\9.0\bin\rsvars.bat"
msbuild.exe "NSISVCLStyles.dproj" /target:clean;build /p:Platform=Win32 /p:config=Release_UNICODE
pause