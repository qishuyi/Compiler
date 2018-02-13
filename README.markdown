# OcamlCompiler by Shuyi Qi
Building a compiler with Ocaml  
## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.  
### Prerequisites
To run this program, you will need:  
1. OPAM, the Ocaml package manager  
2. The latest version of Ocaml  
3. OcamlBuild, a build manager for Ocaml  
### Running the program and the tests
To build the program, you type the following command:  
```
make
```  
For this particular CLI program, it accepts three flags:  
```-length``` : if this flag is present in the command-line argument list, then instead of printing the arguments, one per line, you print the length of the arguments, one per line.  
```-help``` or ```--help``` :  if this flag is present in the command-line argument list, then instead of printing the arguments, the program prints a help message explaining the supported command-line flags and then exits.  
If no flags are present, the program echos the command-line arguments given to the program back to the user, one argument per line.  

To clean the project directory of any compile-time cruft such as compiled programs and object files, execute the following command:  
```
make clean
```
To run the test suite, execute the following command:  
```
make test
```  
The bash script ```test.sh``` encodes the process of running the program, directing the program's output to the file ```tmp.txt```, and comparing ```tmp.txt``` with ```test.out``` using the ```diff```program, while ```test.out``` contains the predetermined output of the program.
  
You can run the program with ```./project.native -length foo bar baz```  
See the example below:  
```
./project.native -length foo bar baz
```  
# Changelog  
All notable changes to this project will be documented in this file.  

The format is based on [Keep a Changelog](http://keepachangelog.com/en/1.0.0/)
and this project adheres to [Semantic Versioning](http://semver.org/spec/v2.0.0.html).  
  
## [1.0.0] - 2018-02-05  
### Added  
- A simple CLI program that echos the command-line arguments given to the program back to the user, one argument per line  
- A ```Makefile``` that allows us to (a) build the project with a single command ```make``` and (b) clean the project directory of any compile-time cruft such as compiled programs and object files with ```make clean```  
- A test suite that can be run with ```make test```  
- This README.markdown file

## [2.0.0] - 2018-02-12
### Added
- A compilation pipeline that supports in our compiler for a small arithmetic language build on S-expressions.  
- Implementation of the compilation pipeline for numbers, arithmetic operations (addition, subtraction, multiplication, division), booleans, less-than-or-equal comparison, if-then-else statement, floating point literals and the relevant operations over them (including the ```NaN``` - "Not a number" - constant).
- In short, the core arithmetic language is defined as follows:
```e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
    | true | false | (<= e1 e2) | (if e1 e2 e3)
    | f | NaN
```  
- Features that need more clarifications:
  1. In the case of division, ```e2``` should not have integer value ```0```.  
  2. Boolean constants are ```true``` and ```false```.  
  3. Less-than-or-equal comparison takes two integer expressions or two float expressions as input and will throw an error if the inputs have a type mismatch.  
  4. (a) If-then-else takes a boolean expression and two expressions as input.  
     (b) The semantics of if-then-else is to evaluate ```e1``` to a boolean value. If the boolean value is true, then evaluate ```e2```, otherwise evaluate ```e3```.  
     (c) e2 and e3 must be of the same type.
  5. Here a floating-point literal is defined as a number with a decimal point. And there exists at least one decimal to the left and right of the decimal point.
  6. All arithmetic operations (addition, subtraction, multiplication, division) will produce NaN if at least one of the arguments is a ```NaN```.
  7. Less-than-or-equal comparison will return ```false``` if at least one of the arguments is a ```NaN```.
  7. In all above cases of type mismatches, the compiler will give a descriptive statement about the error and exit.  

