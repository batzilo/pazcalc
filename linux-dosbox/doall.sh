#!/bin/bash

MYLANG=pazcal

start="$1"

for f in Correct/[0-9]*.$MYLANG Wrong/[0-9]*.$MYLANG ; do
  t=${f/%.$MYLANG}
  if [[ "$t" < "$start" ]]; then
    continue
  fi
  ./do.sh $t
done

exit 0
