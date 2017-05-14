#!/bin/bash

declare -a tests=("test_dec" "test_str_int" "test_for" "test_while" "test_collatz" "test_array" "test_proc_env" "test_embedded_fun" "test_array_fun")
declare -a bad=("test_double_var_block" "test_no_begin" "test_wrong_symbol" "test_unknown_id" "test_unknown_fun_id" "test_bad_arg_number" "test_div_zero" "test_index_out_of_bounds" "test_proc_assign" "test_for_not_int" "test_while_not_bool" "test_arithm_not_int")

for i in "${tests[@]}"
do
    echo "$i"
    ./Interpret good/$i.txt > good/tmp.out
    diff good/$i.out good/tmp.out
done

for i in "${bad[@]}"
do
    echo "$i"
    ./Interpret bad/$i.txt > bad/tmp.out 2> bad/tmp.err
    diff bad/$i.out bad/tmp.out
    diff bad/$i.err bad/tmp.err
done