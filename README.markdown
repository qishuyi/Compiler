# MyCompiler by Shuyi Qi
Building a compiler with Ocaml
## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.
### Prerequisites
To run this program, you will need:
OPAM, the Ocaml package manager
The latest version of Ocaml
OcamlBuild, a build manager for Ocaml
## Running the program and the tests
To build the program, you type the following command:
```
make
```
You can execute the program in the following format: ```./project.native [flags][args]```
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
You can execute the program in the format: ./project.native [flags] [args]
See the example below:
```
./project.native -length foo bar baz
```