#!/bin/bash

c=1
sum=0
runs=$1
steps=$2

while [ $c -le $runs ]
do
   a=`(time ./dist/build/fluid/fluid --batch-mode --maxSteps=$steps +RTS -N2) 2>&1 \
   | grep "real" \
   | cut -f2 \
   | cut -d"m" -f2 \
   | cut -d"s" -f1`

   sum=`perl -e 'print ($ARGV[0] + $ARGV[1]);' $sum $a`
   #echo $sum

   ((c++))
done

echo `perl -e 'print ($ARGV[0] / $ARGV[1]),"\n";' $sum $runs`
