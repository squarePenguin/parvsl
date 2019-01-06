#! /bin/bash

time $TO/bin/redcsl -w ../packages/$1/$2.tst | tee csl-$2.log
time ./fastvsl-arith -i fastrcore-arith.img  ../packages/$1/$2.tst | tee arith-$2.log

echo "==============================================================="
echo $1
diff -b csl-$2.log arith-$2.log | tee $2.diff
 