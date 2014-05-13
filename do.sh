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
        ./pazcalc $f > temp.out
        cat temp.out | grep "<error>"
        echo
        echo
        cat temp.out
        echo
    fi
done

rm temp.out
rm pazcalc
cd ..
