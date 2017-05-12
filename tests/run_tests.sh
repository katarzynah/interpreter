#!/bin/bash

declare -a tests=("test_dec" "test_fun_dec" "test1")

for i in "${tests[@]}"
do
    echo "$i"
    ./Interpret tests/$i.txt > tests/tmp.out
    diff tests/$i.out tests/tmp.out
done