#!/bin/env bash

NUMRUNS=5
TIMEOUT=1

status_file="babygroe.log"

CMD="./vsl -i rcore.img babygroe.red"

count=0

for i in `seq $NUMRUNS`; do
    timeout $TIMEOUT $CMD 1>$status_file 2>$status_file
    retval=$?
    if [ $retval -ne 0 ]; then
        count=$(($count + 1))
    fi
done

echo "${count}/${NUMRUNS} tests timed out"