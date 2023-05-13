#!/bin/bash

TMP_OUT=$(mktemp)
TMP_ERR=$(mktemp)

RED='\033[0;31m'
GREEN='\033[0;32m'
NO_COL='\033[0m'

passed=0
failed=0

run_tests () {
    dir_name=$1
    for file in $dir_name
    do

        echo "Run test:" ${file%.myl}

        chmod +x $file

        ./interpreter "$file" 1>$TMP_OUT 2>$TMP_ERR

        if diff ${file%myl}out $TMP_OUT > /dev/null;
        then echo -e "OK IN STDOUT"
            is_output_ok=true
        else echo -e "${RED}ERROR IN STDOUT${NO_COL}"
            is_output_ok=false
            echo $TMP_OUT
        fi

        if diff ${file%myl}err $TMP_ERR > /dev/null;
        then echo -e "OK IN STDERR"
            if $is_output_ok
            then passed=$((passed + 1))
            else failed=$((failed + 1))
            fi
        else echo -e "${RED}ERROR IN STDERR${NO_COL}"
            failed=$failed + 1
            echo $TMP_ERR
        fi


    done
}

run_tests "./good/*.myl"
run_tests "./bad/*.myl"

echo -e "${GREEN}Passed ${passed} tests${NO_COL}"
echo -e "${RED}Failed ${failed} tests${NO_COL}"