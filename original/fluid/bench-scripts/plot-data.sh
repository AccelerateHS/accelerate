#!/bin/bash

plotDir="plot-data"
sizes="64 128 256"
#sizes="64 128"
#sizes="512"
#sizes="256"
runs=10
steps=100

#Check if directory for plot-data exists, create if it doesn't
if [ ! -d "$plotDir" ]
then
   mkdir plot-data
fi

for size in $sizes
do
   echo "BENCHMARK FOR $size"
   #Plot data file header comment
   echo "# Data for running simulators with array size of $size" > $plotDir/$size.dat
   echo "# Cores  Haskell-qg Haskell   C Time" >> $plotDir/$size.dat

   #Only run C version once as it does not vary over number of cores
   echo "Running C version..."
   cTime=`~/src/stam-benchmarking/runBenchmark.sh $runs $size`
   echo "In $runs runs of size $size average is: $cTime"

   #Run Haskell benchmark over cores 1-8
   echo "Running Haskell version..."
   cores=1
   maxCores=8

   c=1
   sum=0
   sum2=0
   while [ $cores -le $maxCores ]
   do
      echo "- $cores -"
      while [ $c -le $runs ]
      do
         temp=`(time ~/src/fluid/dist/build/fluid/fluid --batch-mode --width=$size --maxSteps=$steps +RTS -N$cores -qg) 2>&1 \
         | grep "real" \
         | cut -f2 \
         | cut -d"s" -f1`
         a=`echo $temp | cut -d"m" -f1`
         a="$a `echo $temp | cut -d"m" -f2`"
         echo $a
         sum=`perl -e 'print ($ARGV[0] + ($ARGV[1] * 60 + $ARGV[2]));' $sum $a`
         ((c++))
      done
      ave=`perl -e 'print ($ARGV[0] / $ARGV[1]);' $sum $runs`
      echo "Average time of $ave"

      echo "Running with parallel GC"
      c2=1
      while [ $c2 -le $runs ]
      do
         temp2=`(time ~/src/fluid/dist/build/fluid/fluid --batch-mode --width=$size --maxSteps=$steps +RTS -N$cores) 2>&1 \
         | grep "real" \
         | cut -f2 \
         | cut -d"s" -f1`
         a2=`echo $temp2 | cut -d"m" -f1`
         a2="$a2 `echo $temp2 | cut -d"m" -f2`"
         echo $a2
         sum2=`perl -e 'print ($ARGV[0] + ($ARGV[1] * 60 + $ARGV[2]));' $sum2 $a2`
         #echo "----sum=" $sum2
         ((c2++))
      done
      ave2=`perl -e 'print ($ARGV[0] / $ARGV[1]);' $sum2 $runs`
      echo "Average time of $ave2"
      

      echo "Writing results to $currsize.dat..."
      echo "  $cores      $ave     $ave2    $cTime" >> $plotDir/$size.dat

      ((cores++))
      sum=0
      c=1
      ave=0
      temp=0
      sum2=0
      c2=1
      ave2=0
      temp2=0
   done

   echo "Finished benchmark for size $currSize"
   echo ""
   echo ""
done
echo "BENCHMARK COMPLETE"
