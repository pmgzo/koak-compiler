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




# declare -a arr=( "4 1 1 2 2" "5 1 2 3 2" "8 4 6 3 6" "12 3 9 1 6")
declare -a arr=$@

diff mine/file_koak_${arr// /_} valid_file/file_koak_${arr// /_} | cat -e && diff mine/file_koak_${arr// /_} valid_file/file_koak_${arr// /_} > /dev/null && echo -e "${GREEN}Test koak ${BGreen}${arr}${GREEN} OK${NC}" || echo -e "${RED}Test koak ${BRed}${arr}${RED} FAILED${NC}"





# for arg in "${arr[@]}"; do
#         diff mine/file_309pollution_${arg// /_} valid_file/file_309pollution_${arg// /_} | cat -e && diff mine/file_309pollution_${arg// /_} valid_file/file_309pollution_${arg// /_} > /dev/null && echo -e "${GREEN}Test 309pollution ${BGreen}${arg}${GREEN} OK${NC}" || echo -e "${RED}Test 309pollution ${BRed}${arg}${RED} FAILED${NC}"
# done




# for f in $( ls testFiles/ ); do
#         diff mine/file_309pollution_$f valid_file/file_309pollution_$f | cat -e && diff mine/file_309pollution_$f valid_file/file_309pollution_$f > /dev/null && echo -e "${GREEN}Test 309pollution ${BGreen}${arg// /_}${GREEN} OK${NC}" || echo -e "${RED}Test 309pollution ${BRed}${arg// /_}${RED} FAILED${NC}"
# done
