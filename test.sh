#! /bin/bash

#     ./test.sh module_name
# Test a single Reduce module, leaving the log in testlogs/module_name.rlg
# and compare that with the reference log in the Reduce tree leaving
# differences in testlogs/module_name.diffs.
# Optionally and foir the benefit of use from Makefile one can pass the
# argument as the name of the log file.

mod=${1%%.rlg}
mod=${mod##testlogs/}

w=`grep -v "^(" package.map | grep "($mod "`

echo grep == $w

case $w in
*[\ ]test[\ ]*)
  echo OK
  ;;
*)
  printf "Module $mod not recognized as one with a test file\n"
  exit 1
  ;;
esac

mod="${w#*\(}"
mod="${mod%%\ *}"
echo mod=$mod

case $w in
*[\ ]core[\ ]*)
  ;;
*)
  pkg="load_package $mod; "
  ;;
esac

echo "pkg=$pkg"

dir="${w#*\"}"
dir="${dir%%\"*}"

./vsl -ireduce.img <<EOF | tee testlogs/$mod.rlg
off int;
$pkg on echo;
showtime;
in "../packages/$dir/$mod.tst";
showtime;
quit;
EOF

./parvsl -iparreduce.img <<EOF | tee testlogs/par$mod.rlg
off int;
$pkg on echo;
showtime;
in "../packages/$dir/$mod.tst";
showtime;
quit;
EOF

diff -b testlogs/$mod.rlg ../packages/$dir | tee testlogs/$mod.diff
diff -b testlogs/$mod.rlg testlogs/par$mod.rlg | tee testlogs/par$mod.diff

exit 0
