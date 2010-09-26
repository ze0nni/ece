REM Выставляем нужные переменные
Set TARGET=vforthcmd
rem Set ARGS="-O3"

rem Очистка экрана
echo off
cls

cd "src"

del "..\bin\VForthCmd_fpc.exe"

fpc "%TARGET%.dpr" %ARGS%
copy %TARGET%.exe "..\bin\VForthCmd_fpc.exe"
del %TARGET%.exe
del *.o
del *.ppu
del *.or

cd "..\bin"

start VForthCmd_fpc.exe

pause
taskkill /F /IM VForthCmd_fpc.exe