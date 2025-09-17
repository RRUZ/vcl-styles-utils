@Echo OFF
FOR %%G IN (12,13,14,15,17,18,19,20,21,22,23,37) DO (
    call Demo_Build.bat %%G
)
PAUSE