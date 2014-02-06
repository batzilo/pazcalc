#!/bin/bash

# do.sh - Run Compiler with testcases

if ! test -f pazcalc
then
    make
    make clean
fi

cp pazcalc testcases
cd testcases

for f in *.paz
do
    if test -f "$f"
    then
        echo "========================================" 
        echo "Working with file $f"
        echo
        cat $f
        echo
        ./pazcalc < $f
        echo
        echo
        echo
    fi
done

rm pazcalc
cd ..
