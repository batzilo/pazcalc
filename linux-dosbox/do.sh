#!/bin/bash

MYLANG=paz
MYCOMP=pazcalc

function die () {
  printf "FAILED!!!\n"
  rm -f *.asm a.*
  read -p "Press <ENTER> to continue...  "
  exit 1
}

while [ "$1" != "" ]; do
  f="${1/%.$MYLANG}"

  echo "--------------------------------------------------------------------"
  printf "%-40s" "$f"
  rm -f *.asm a.*
  cp -f "$f".$MYLANG a.$MYLANG
  ./$MYCOMP a.$MYLANG || die
  printf "compilation succeeded\n"

  dosbox run.bat -exit >& /dev/null
  #dosemu -dumb -input ran.bat -exit

  shift
done

rm -f *.asm a.*

exit 0
