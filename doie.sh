#!/bin/bash

# doie.sh do intermediate code examples

cp pazcalc examples
cd examples

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
