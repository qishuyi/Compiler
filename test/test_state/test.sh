#!/bin/bash

ref=./test/test_state/test.txt
file1=./test/test_state/test1.step.out
temp1=./test/test_state/temp1.test
while=./test/test_state/test2.txt
file2=./test/test_state/test2.step.out
temp2=./test/test_state/temp2.test
whileonlist=./test/test_state/test3.txt
file3=./test/test_state/test3.step.out
temp3=./test/test_state/temp3.test

./compiler.native $ref -step > $temp1
diff --brief <(sort $temp1) <(sort $file1) >/dev/null
comp_value_ref=$?

./compiler.native $while -step > $temp2
diff --brief <(sort $temp2) <(sort $file2) >/dev/null
comp_value_while=$?

./compiler.native $whileonlist -step > $temp3
diff --brief <(sort $temp3) <(sort $file3) >/dev/null
comp_value_whileonlist=$?

if [ $comp_value_ref -ne 0 ]
then
    echo "Error: Test case did not pass for evaluation of reference."
elif [ $comp_value_while -ne 0 ]
then
    echo "Error: Test case did not pass for evaluation of while loop."
elif [ $comp_value_whileonlist -ne 0 ]
then
    echo "Error: Test case did not pass for evaluation of while loop on list."
else
    echo "Test suite passed for state."
fi
