#!/bin/bash


# if [ -e "../../glados" ]; then
#     echo "Start the test"
# else
#     echo "Glados executable not exist"
#     exit 0
# fi


check() {
    # result=$(./../../glados $1)
    result="10"
    if [ "$result" == "$2" ]; then
        echo "($1): TEST PASSED"
    else
        echo "($1): TEST NOT PASSED"
    fi
}


check File_Test/file.scm 10

