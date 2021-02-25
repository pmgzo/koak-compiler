#!/bin/bash

declare -a arr=( "exampleKoak/test_fib_iterative.kk" "exampleKoak/test.kk" "exampleKoak/test_subject1.kk" "exampleKoak/test_fib_recursive.kk" "exampleKoak/test_simple_calcul.kk" "exampleKoak/test_subject2.kk" )




mv tests/exec_test.sh .
#######################################################
make re

for arg in "${arr[@]}"; do
    rm -f *.o
    echo -e "\033[1;35m"./koak $arg"\033[0m"
    ./koak $arg && gcc main.c *.o && ./a.out
    # ./exec_test.sh $arg
done
#######################################################
mv exec_test.sh tests
