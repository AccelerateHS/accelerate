#!/bin/bash

c=1
sum=0
cores=1
maxCores=8
runs=10
steps=100

echo "=== Running full benchmark ==="
echo "Cores range = $cores - $maxCores"
echo "Number of runs per core = $runs"
echo "Number of steps per run = $steps"

while [ $cores -le $maxCores ]; do
   echo "== Running on $cores cores =="
   while [ $c -le $runs ]; do
      
      a=`(time ./dist/build/fluid/fluid --batch-mode --maxSteps=$steps +RTS -N$cores) 2>&1 \
      | grep "real" \
      | cut -f2 \
      | cut -d"m" -f2 \
      | cut -d"s" -f1`

      sum=`perl -e 'print ($ARGV[0] + $ARGV[1]);' $sum $a`
      #echo $sum

      ((c++))
   done
   echo "Ave = ",`perl -e 'print ($ARGV[0] / $ARGV[1]),"\n";' $sum $runs`

   ((cores++))
   sum=0
   c=0
done
