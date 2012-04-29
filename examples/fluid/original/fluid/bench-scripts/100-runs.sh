#!/bin/bash

c=1

plotDir="plot-data"
runs=100
steps=100
cores=4
sizes="64"

if [ ! -d "$plotDir" ]
then
   mkdir plot-data
fi

for size in $sizes
do
   while [ $c -le $runs ]
   do
      temp=`(time ~/src/fluid/dist/build/fluid/fluid --batch-mode --width=$size --maxSteps=$steps +RTS -N$cores -qg) 2>&1 \
      | grep "real" \
      | cut -f2 \
      | cut -d"s" -f1`
      a=`echo $temp | cut -d"m" -f1`
      a="$a `echo $temp | cut -d"m" -f2`"
      #echo $a
      perl -e 'print ($ARGV[0] * 60 + $ARGV[1]);' $a
      ((c++))
   done
   c=1
done
