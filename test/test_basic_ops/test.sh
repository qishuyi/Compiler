#!/bin/bash

file1=./test/test_basic_ops/test1.src
file2=./test/test_basic_ops/test1.lex.out
file3=./test/test_basic_ops/test1.parse.out
emptyfile=./test/test_basic_ops/test2.src
file4=./test/test_basic_ops/test2.lex.out
file5=./test/test_basic_ops/test2.parse.out
extratoken=./test/test_basic_ops/test3.src
file6=./test/test_basic_ops/test3.lex.out
file7=./test/test_basic_ops/test3.parse.out
temp1=./test/test_basic_ops/temp1.test
temp2=./test/test_basic_ops/temp2.test
temp3=./test/test_basic_ops/temp3.test

./compiler.native $file1 -lex > $temp1
diff --brief <(sort $temp1) <(sort $file2) >/dev/null
comp_value_lex1=$?

./compiler.native $file1 -parse > $temp1
diff --brief <(sort $temp1) <(sort $file3) >/dev/null
comp_value_parse1=$?

./compiler.native $emptyfile -lex > $temp2
diff --brief <(sort $temp2) <(sort $file4) >/dev/null
comp_value_lex2=$?

./compiler.native $emptyfile -parse > $temp2
diff --brief <(sort $temp2) <(sort $file5) >/dev/null
comp_value_parse2=$?

./compiler.native $extratoken -lex > $temp3
diff --brief <(sort $temp3) <(sort $file6) >/dev/null
comp_value_lex3=$?

./compiler.native $extratoken -parse > $temp3
diff --brief <(sort $temp3) <(sort $file7) >/dev/null
comp_value_parse3=$?

if [ $comp_value_lex1 -ne 0 ]
then
    echo "Error: Test case 1 did not pass for lexing."
elif [ $comp_value_parse1 -ne 0 ]
then
    echo "Error: Test case 1 did not pass for parsing."
elif [ $comp_value_lex2 -ne 0 ]
then
    echo "Error: Test case 2 did not pass for lexing."
elif [ $comp_value_parse2 -ne 0 ]
then
    echo "Error: Test case 2 did not pass for parsing."
elif [ $comp_value_lex3 -ne 0 ]
then
    echo "Error: Test case 3 did not pass for lexing."
elif [ $comp_value_parse3 -ne 0 ]
then
    echo "Error: Test case 3 did not pass for parsing."
else
    echo "Test suite passed."
fi
