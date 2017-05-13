#!/bin/bash

declare -a tests=("test_dec" "test_fun_dec" "test1" "test_proc" "test_rekur" "test_proc_env" "test_silnia" "test_str_int" "test_array")
declare -a bad=("test_proc_assign" "test_embedded_fun")

for i in "${tests[@]}"
do
    echo "$i"
    ./Interpret tests/$i.txt > tests/tmp.out
    diff tests/$i.out tests/tmp.out
done

for i in "${bad[@]}"
do
    echo "$i"
    ./Interpret bad/$i.txt > bad/tmp.out 2> bad/tmp.err
    diff bad/$i.out bad/tmp.out
    diff bad/$i.err bad/tmp.err
done