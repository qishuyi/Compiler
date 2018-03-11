#!/bin/bash

let_binding=./test/test_typecheck/test.txt
file1=./test/test_typecheck/test1.step.out
recursion=./test/test_typecheck/test2.txt
file2=./test/test_typecheck/test2.step.out
basic_ops=./test/test_typecheck/test3.txt
file3=./test/test_typecheck/test3.step.out
unit=./test/test_typecheck/test4.txt
file4=./test/test_typecheck/test4.step.out
pair=./test/test_typecheck/test5.txt
file5=./test/test_typecheck/test5.step.out
temp1=./test/test_typecheck/temp1.test
temp2=./test/test_typecheck/temp2.test
temp3=./test/test_typecheck/temp3.test
temp4=./test/test_typecheck/temp4.test
temp5=./test/test_typecheck/temp5.test

./compiler.native $let_binding -step > $temp1
diff --brief <(sort $temp1) <(sort $file1) >/dev/null
comp_value_let_binding=$?

./compiler.native $recursion -step > $temp2
diff --brief <(sort $temp2) <(sort $file2) >/dev/null
comp_value_recursion=$?

./compiler.native $basic_ops -step > $temp3
diff --brief <(sort $temp3) <(sort $file3) >/dev/null
comp_value_basic_ops=$?

./compiler.native $unit -step > $temp4
diff --brief <(sort $temp4) <(sort $file4) >/dev/null
comp_value_unit=$?

./compiler.native $pair -step > $temp5
diff --brief <(sort $temp5) <(sort $file5) >/dev/null
comp_value_pair=$?

if [ $comp_value_let_binding -ne 0 ]
then
    echo "Error: Test case did not pass for let_binding evaluation."
elif [ $comp_value_recursion -ne 0 ]
then
    echo "Error: Test case did not pass for recursion evaluation."
elif [ $comp_value_basic_ops -ne 0 ]
then
    echo "Error: Test case did not pass for basic operation evaluation."
elif [ $comp_value_unit -ne 0 ]
then
    echo "Error: Test case did not pass for unit evaluation."
elif [ $comp_value_pair -ne 0 ]
then
    echo "Error: Test case did not pass for pair evaluation."
else
    echo "Test suite passed for typechecking."
fi
