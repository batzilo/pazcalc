#!/bin/bash

# do.sh - Run Compiler with testcases

if ! test -f pazcalc
then
    echo "No pazcalc found, making..."
    make
    make clean
    echo "Done"
fi


echo "Copying pazcalc to testcases directory"
cp pazcalc testcases


cd testcases

for f in *.paz
do
    if test -f "$f"
    then
        echo "========================================" 
        echo "Working with file $f"
        #echo
        #cat $f
        #echo
        ./pazcalc $f > temp.out
        cat temp.out | grep "<error>"
        echo
        echo
        #cat temp.out
        echo
    fi
done

rm *.imm
rm *.asm
rm temp.out
rm pazcalc
cd ..
