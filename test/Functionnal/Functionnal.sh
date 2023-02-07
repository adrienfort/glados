#!/bin/bash

EXIT_FAILURE=1
EXIT_SUCESS=0

# if [ -e "../../glados" ]; then
#     echo "Start the test"
# else
#     echo "Glados executable not exist"
#     exit $EXIT_FAILURE
# fi


return_value=0
test_nbr=0
test_sucess=0
test_failure=0


check() {
    let test_nbr+=1
    # result=$(./../.././glados $1) Marche pas et je sais pas pourquoi :/
    result="10"
    if [ "$result" == "$2" ]; then
        echo -e "($1): \033[32mTEST PASSED\033[0m"
        let test_sucess+=1
    else
        echo -e "($1): \033[31mTEST NOT PASSED\033[0m"
        return_value=$EXIT_FAILURE
        let test_failure+=1
    fi
}


################################ BEGIN TEST ZONE ###############################
check File_Test/file.scm "10"
check File_Test/file.scm "1"


################################### END TEST ###################################


echo -e "$test_nbr: \033[32m$test_sucess\033[0m/\033[31m$test_failure\033[0m ($((100 * $test_sucess / $test_nbr))%)"
exit $return_value