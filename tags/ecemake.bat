REM Выставляем нужные переменные
Set TARGET=ece
Set ARGS=-O3

rem Очистка экрана
echo off
cls

del %TARGET%.exe
fpc %ARGS% %TARGET%.dpr
del *.o
del *.ppu
del *.or


"start %TARGET%.exe E:\projects\ece\AppWindow.pas
start %TARGET%.exe

pause
taskkill /F /IM %TARGET%.exe