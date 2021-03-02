#!/bin/bash
NC='\033[0m'


RED='\033[0;31m'
GREEN='\033[0;32m'
BRed='\033[1;31m'
BGreen='\033[1;32m'
URed='\033[4;31m'
UGreen='\033[4;32m'
BgRed='\033[41m'
BgGreen='\033[42m'
IRed='\033[0;91m'
IGreen='\033[0;92m'
BIRed='\033[1;91m'
BIGreen='\033[1;92m'

BLUE='\033[0;34m'
PURPLE='\033[0;35m'
CYAN='\033[0;36m'


# declare -a arr=( "exampleKoak/test_fib_iterative.kk" "exampleKoak/test.kk" "exampleKoak/test_subject1.kk" "exampleKoak/test_fib_recursive.kk" "exampleKoak/test_simple_calcul.kk" "exampleKoak/test_subject2.kk" )

declare -a arr=(exampleKoak/*.kk)

cp tests/test.sh .
#######################################################
# make fclean
make

for arg in "${arr[@]}"; do
    rm -f exampleKoak/*.o
    echo -e "\033[1;35m"./koak $arg"\033[0m"
    # ./koak $arg > .log && gcc tests/main.c exampleKoak/*.o && ./a.out $arg || echo -e "\033[1;91m"FAILED"\033[0m"
    ./koak $arg && gcc tests/main.c exampleKoak/*.o && ./a.out $arg || echo -e "\033[1;91m"FAILED"\033[0m"

    # ./exec_test.sh $arg
done
#######################################################
rm -f test.sh
