cd "src"

fpc -Fu".\VForth\src\" -O3 "ece.dpr"
move ece.exe .\..\bin\ece_fpc.exe
del ece.exe
cd .\..\bin
ece_fpc.exe

pause