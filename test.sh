#!/bin/bash

file1=test.out
file2=tmp.txt

#Common cases
#3 inputs
./project.native foo bar baz > $file2
./project.native -length foo bar baz >> $file2
./project.native foo bar -length baz >> $file2
#1 inputs
./project.native foo >> $file2
./project.native -length foo >> $file2
#No input
./project.native >> $file2
./project.native -length >> $file2
#Help
./project.native -help >> $file2
./project.native --help >> $file2
./project.native -help foo bar baz >> $file2
./project.native foo bar baz -help >> $file2

diff --brief <(sort $file1) <(sort $file2) >/dev/null
comp_value=$?

if [ $comp_value -eq 0 ]
then
    echo "Correct output. Test suite passed"
else
    echo "Wrong output. Revise."
fi
