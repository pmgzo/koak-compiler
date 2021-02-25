#!/bin/bash

declare -a arr=( "exampleKoak/test_fib_iterative.kk" "exampleKoak/test.kk" "exampleKoak/test_subject1.kk" "exampleKoak/test_fib_recursive.kk" "exampleKoak/test_simple_calcul.kk" "exampleKoak/test_subject2.kk" )




mv tests/test.sh .
#######################################################
make re

for arg in "${arr[@]}"; do
    rm -f *.o
    echo -e "\033[1;35m"./koak $arg"\033[0m"
    ./koak $arg && gcc tests/main.c exampleKoak/*.o && ./a.out || echo -e "\033[1;91m"FAILED"\033[0m"

    # ./exec_test.sh $arg
done
#######################################################
mv test.sh tests
