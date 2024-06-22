#!/bin/bash

rm -f check
rm -f hack

gcc -O1 -ggdb -o check check.c
gcc -O1 -ggdb -o hack hack.c

