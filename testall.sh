#! /bin/bash

#     ./testall.sh
#
# Run test.sh on every module of Reduce that has a test file.
# Note that plenty of these will not run at all and that everything
# will be so slow that running ALL tests is liable to be an overnight task.

for x in `grep -v ^% package.map | grep \ test\  | \
          sed -e 's/".*$//;s/(//'`
do
  printf "About to test $x\n"
  time ./test.sh $x
  printf "\n\n\n"
done

exit 0
