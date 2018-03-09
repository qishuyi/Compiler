#!/bin/bash

step_function=./test/test_funcs/test1.src
file1=./test/test_funcs/test1.step.out
step_recursion=./test/test_funcs/test2.src
file2=./test/test_funcs/test2.step.out
temp1=./test/test_funcs/temp1.test
temp2=./test/test_funcs/temp2.test

./compiler.native $step_function -step > $temp1
diff --brief <(sort $temp1) <(sort $file1) >/dev/null
comp_value_function=$?

./compiler.native $step_recursion -step > $temp2
diff --brief <(sort $temp2) <(sort $file2) >/dev/null
comp_value_recursion=$?

if [ $comp_value_function -ne 0 ]
then
    echo "Error: Test case did not pass for small step function evaluation."
elif [ $comp_value_recursion -ne 0 ]
then
    echo "Error: Test case did not pass for small step recursion evaluation."
else
    echo "Test suite passed."
fi
