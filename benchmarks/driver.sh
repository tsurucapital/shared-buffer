#!/bin/bash

set -e

BUFNAME="aBuf"
SZ="10000000"

$( timeout 12 ./Writer $BUFNAME $SZ ) &
sleep 0.5
COUNT=$( ./Reader $BUFNAME $SZ )

# rm /run/shm/aBuf; # rm /run/shm/sem.aBuf ;
echo $COUNT
