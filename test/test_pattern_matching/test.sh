#!/bin/bash

match=./test/test_pattern_matching/test.txt
file1=./test/test_pattern_matching/test1.step.out
temp1=./test/test_pattern_matching/temp1.test

./compiler.native $match -step > $temp1
diff --brief <(sort $temp1) <(sort $file1) >/dev/null
comp_value_match=$?

if [ $comp_value_match -ne 0 ]
then
    echo "Error: Test case did not pass for evaluation of pattern matching."
else
    echo "Test suite passed for pattern matching."
fi
