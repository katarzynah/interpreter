#!/bin/bash

declare -a tests=("test_dec" "test_fun_dec" "test1" "test_proc" "test_rekur" "test_proc_env")

for i in "${tests[@]}"
do
    echo "$i"
    ./Interpret tests/$i.txt > tests/tmp.out
    diff tests/$i.out tests/tmp.out
done