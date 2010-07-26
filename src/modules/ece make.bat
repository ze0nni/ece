REM Выставляем нужные переменные
Set TARGET=autospace
Set ARGS=-O3

rem Очистка экрана
echo off
cls

del %TARGET%.dll
fpc %ARGS% %TARGET%.dpr
del *.o
del *.ppu
del *.or
pause