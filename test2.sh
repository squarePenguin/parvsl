#! /bin/bash

pkg=${2:-"$1"}
printf "Testing package $pkg from directory $1%n"

time $TO/bin/redcsl -w <<XXX | tee csl-$pkg.log
off echo,int;
load_package $pkg;
in "../packages/$1/$pkg.tst";
quit;
XXX

time ./fastvsl-arith -i fastreduce-arith.img  <<XXX | tee arith-$pkg.log
off echo,int;
load_package $pkg;
in "../packages/$1/$pkg.tst";
quit;
XXX

echo "==============================================================="
echo $1
diff -b csl-$pkg.log arith-$pkg.log | tee $pkg.diff
