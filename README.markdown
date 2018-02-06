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