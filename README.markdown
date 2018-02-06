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
You can execute the program in the format: ./project.native [flags] [args]
See the example below:
```
./project.native -length foo bar baz
```
To run the test suite, execute the following command:
```
make test
```