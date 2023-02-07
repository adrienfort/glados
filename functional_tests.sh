#!/bin/bash

EXIT_FAILURE=1
EXIT_SUCESS=0

check_executable() {
    if [ -e "./glados" ]; then
        echo "Glados executable found, functional tests starting..."
    else
        echo "Glados executable not found"
        exit $EXIT_FAILURE
    fi
}

test() {
    result=$(./glados $1);

    if [[ $result == $2 ]]; then
        echo "$3: [x]"
    else
        echo "$3: [ ]"
        exit $EXIT_FAILURE
    fi
}

check_executable
test test_example.scm "x 1" "Sample test"

exit $EXIT_SUCESS