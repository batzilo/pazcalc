@echo off
if exist paz.lib del paz.lib

support\masm -mx src\stdio\printi.asm;
support\masm -mx src\stdio\readi.asm;
support\masm -mx src\stdio\printc.asm;
support\masm -mx src\stdio\readc.asm;
support\masm -mx src\stdio\printb.asm;
support\masm -mx src\stdio\readb.asm;
support\masm -mx src\stdio\printr.asm;
support\masm -mx src\stdio\readr.asm;
support\masm -mx src\stdio\prints.asm;
support\masm -mx src\stdio\reads.asm;

support\lib paz.lib /NOIGNORECASE +printi.obj +readi.obj;
support\lib paz.lib /NOIGNORECASE +printc.obj +readc.obj;
support\lib paz.lib /NOIGNORECASE +printb.obj +readb.obj;
support\lib paz.lib /NOIGNORECASE +printr.obj +readr.obj;
support\lib paz.lib /NOIGNORECASE +prints.obj +reads.obj;

support\masm -mx src\math\abs.asm;
support\masm -mx src\math\fabs.asm;
support\masm -mx src\math\sqrt.asm;
support\masm -mx src\math\sin.asm;
support\masm -mx src\math\cos.asm;
support\masm -mx src\math\tan.asm;
support\masm -mx src\math\arctan.asm;
support\masm -mx src\math\exp.asm;
support\masm -mx src\math\ln.asm;
support\masm -mx src\math\pi.asm;

support\lib paz.lib /NOIGNORECASE +abs.obj fabs.obj +sqrt.obj;
support\lib paz.lib /NOIGNORECASE +sin.obj +cos.obj +tan.obj +arctan.obj;
support\lib paz.lib /NOIGNORECASE +exp.obj +ln.obj +pi.obj;

support\masm -mx src\stdlib\trunc.asm;
support\masm -mx src\stdlib\round.asm;
support\masm -mx src\stdlib\ord.asm;
support\masm -mx src\stdlib\chr.asm;
support\masm -mx src\stdlib\exit.asm;

support\lib paz.lib /NOIGNORECASE +trunc.obj +round.obj;
support\lib paz.lib /NOIGNORECASE +ord.obj +chr.obj;
support\lib paz.lib /NOIGNORECASE +exit.obj;

support\masm -mx src\auxil\new.asm;
support\masm -mx src\auxil\dispose.asm;
support\masm -mx src\auxil\formati.asm;
support\masm -mx src\auxil\formatr.asm;
support\masm -mx src\auxil\parsei.asm;
support\masm -mx src\auxil\parser.asm;

support\lib paz.lib /NOIGNORECASE +new.obj +dispose.obj;
support\lib paz.lib /NOIGNORECASE +formati.obj +formatr.obj;
support\lib paz.lib /NOIGNORECASE +parsei.obj +parser.obj;

support\lib paz.lib /NOIGNORECASE, paz.lst;
ren paz.lib paz.lib
ren paz.lst paz.lst

del *.obj
del *.bak
