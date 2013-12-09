@echo off
set MYLIB=paz
path %path%;.\support
echo Assembling...
masm /mx /t a.asm;
if errorlevel 1 goto stop
echo Linking...
link /noi /tiny /nologo a.obj,a.com,nul,%MYLIB%.lib;
if errorlevel 1 goto stop
echo Executing...
echo --------------------------------------------------------------------
a
:stop
pause
del a.asm
del a.obj
del a.com
