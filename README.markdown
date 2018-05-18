# OcamlCompiler by Shuyi Qi
Building a compiler with Ocaml  
## Source-to-source translation to a C-like language
My bash script failed to work so I put the four test files in the root directory and to run the test, do:
```
make
```
and then do:
```
./compiler.native (test file name)
```
to see the value evaluated from the compiler that I wrote. To see the small-step evaluation, add the ```-step``` flag at the end of the command-line arguments.
To see source-to-source translation to the C-like language, pass in ```-cil``` flag at the end of the command-line arguments:
```
./compiler.native (test file name) -cil > temp.c
```
This command will output the result of the translation to the ```temp.c``` file. We can then compile it using gcc and run the program. Both ways should produce the same results. To see what language features this source-to-source translation supports, see the changelog below.

## Getting Started
These instructions will get you a copy of the project up and running on your local machine for development and testing purposes.  
### Prerequisites
To run this program, you will need:  
1. OPAM, the Ocaml package manager  
2. The latest version of Ocaml  
3. OcamlBuild, a build manager for Ocaml
4. Menhir, the ocaml parser  
### Running the program and the tests
To build the program, you type the following command:  
```
make
```  
For this particular CLI program, it accepts three flags:  
```-lex``` : if this flag is present, the program will print to the console the stream of tokens along with their positions in the input file, and exit.
```-parse``` : if this flag is present, the program will print to the console the resulting abstract syntax tree (AST), and exit.
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
The bash script ```test.sh``` encodes the process of running the program, directing the program's output to the  ```.test``` files , and comparing the ```.txt``` files with the ```.out``` files using the ```diff``` program, while ```.out``` files contain the predetermined output of the program.
  
To run the program, use a command in to following format: ```./compiler.native [filename] [-flag]```  
See the example below:  
```
./compiler.native ./test/test1.src -lex
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
```
e ::= n | (+ e1 e2) | (- e1 e2) | (* e1 e2) | (/ e1 e2)
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

## [3.0.0] - 2018-02-22
### Changed
- Changed the handwritten lexer and parser on branch ```assignment-02``` to using the Ocaml lexer and parser: Ocamllex and Menhir  
  The manuals that I used are: [Ocamllex](https://courses.softlab.ntua.gr/compilers/2015a/ocamllex-tutorial.pdf) and [Menhir](http://gallium.inria.fr/~fpottier/menhir/manual.pdf)  
- Changed the core language to the following syntax:
```
e ::= n | (e) | e1 + e1 | e1 - e2 | e1 * e2 | e1 / e2
    | true | false | e1 <= e2 | if e1 then e2 else e3
```  
so that the operators take on the more familiar infix style  
### Added  
- Added test cases to the hand-rolled lexer and parser on branch ```assignment-02```. The test cases work as follows  
  Added a pair of flags ```-lex``` and ```-parse``` to the compiler.  
  When given the ```-lex``` flag, it prints out the resulting stream of tokens to the console, and the exit.  
  When given the ```-parse``` flag, it prints the resulting abstract syntax tree, and then exit.

## [3.0.1] - 2018-02-22
### Added
- Line-column information to the error messages.  
  Consider the code below:  
  ```
  if
	3 <= 5
  then
	3 + 5
  else
	true
  ```
  Our lexical stream used to look as follows:
  ```
  ['if',false,'then',3,'else',true]
  ```
  Now it will look as follows:
  ```
  ['if' (1 : 1),false (2 : 6),'then' (3 : 1),3 (4 : 6),'else' (5 : 1),true (6 : 6)]
  ```
  With this information, when we encounter an error during interpretation, our error message will contain the column-line information of where exactly in the file the error occurs. The error message for the above code is as follows:
  ```
  Fatal error: exception Failure(" (4 : 6)The syntax 'if a1 then a2 else a3' 
  requires a2 and a3 to be of the same type")
  ```
### Changed
- Updated test cases so that the stream of tokens produced by the lexer will include line-column information.

## [4.0.1] - 2018-03-07
### Added
- Added both let-binds, functions and recursions to the language along with variables. The syntax now looks like the following:  
  ```
  e ::= n | b | e1 (+) e2 | if e1 then e2 else e3
      | x | let x = e1 in e2 | fun x -> e | e1 e2
      | fix f x -> e
  ```
### Changed
- Changed from big-step semantics to small-step semantics. Now specifying the flag ```-step``` in the command when running the program, the program will print out small-step evaluation for the given expression. 
- For instance, the following expression
  ```
  let max2 =
      fun x -> fun y -> if x > y then x else y
    in
    let a = 5 in
    let b = 2 in
    (max2 a) b
  ```
  will be evaluated step-by-step to the following:
  ```
  (let max2 = (fun x -> (fun y -> (if (> x y) x y))) in (let a = 5 in (let b = 2 in ((max2 a) b))))
  (let a = 5 in (let b = 2 in (((fun x -> (fun y -> (if (> x y) x y))) a) b)))
  (let b = 2 in (((fun x -> (fun y -> (if (> x y) x y))) 5) b))
  (((fun x -> (fun y -> (if (> x y) x y))) 5) 2)
  ((fun x -> (fun y -> (if (> x y) x y))) 5)
  ((fun y -> (if (> 5 y) 5 y)) 2)
  (if true 5 2)
  5
  5
  ```
## [5.0.1] - 2018-03-11
### Changes
- Improved the small-step semantics so that more specific steps are shown during evaluation.  
### Added
- Introduced a typechecking phase to the compiler, thereby converting it to target a * statically-typed programming language * where typechecking is performed during the compilation phase rather than when the program runs. This removes the cost of runtime checks as well as allow the developer to catch bugs in their program earlier in the development process.  
- The syntax is extended to as follows:
  ```
  e ::= n | b | e1 (+) e2 | if e1 then e2 else e3
    | x | let x : t = e1 in e2
    | e1 e2 | fun (x:t1) : t2 -> e | fix f (x:t1) : t2 -> e

  t ::= int | bool | t1 -> t2
  ```
  In the above language, we’ve added an additional syntactic form for types (denoted by the metavariable t) as well as full type annotations for lets and functions so we know the type of bound variables. With this language, we introduced a typechecking phase to the compiler that runs before evaluation and ensures that the program is well-formed before compile time.  
- Added type ```unit``` to the language, specifically added the following extension to the language:
  ```
  e ::= () | ...
  t ::= unit | ...
  ```
  By introducing the ```unit``` type, we are thus able to specify a * Don't care * value.  
- Added ** pairs ** to the language. A pair data structure allows us to group together two related values. We create a pair by calling ```(e1, e2)```, extract the first value using ```fst``` and the second value using ```snd```. The two elements of a pair structure does not need to be of the same type.
  The following extension is added to the syntax:  
  ```
  e ::= (e1, e2) | fst e | snd e | ...
  t ::= t1 * t2 | ...
  ```
- Added ** lists ** to the language. A list data structure is either an empty list ```[] : t``` (t specifies the type of the empty list so that we don't need to worry about infer it later) or a series of cons: ```e1 :: e2``` puts a single value ```e1``` onto the head of a list ```e2```.
  The extensions added to the syntax are as follows:
  ```
  e ::= [] : t | e1 :: e2 | hd e | tl e | empty e
  t ::= [t]
  ```
  Our list implementation supports three functions:
  1. ```hd e``` returns the head element of the list ```e```.
  2. ```tl e``` returns the list resulting from stripping the head element off list e.
  3. ```empty e``` returns ```true``` iff the list ```e``` is the empty list and ```false``` if it is not empty.

## [6.0.1] - 2018-03-14
### Added
- Added reference calls to the language, specifically extended the compiler to handle the following:
  ```
  e ::= (e) | n | b | e1 (+) e2 | if e1 then e2 else e3
    | x | let x : t = e1 in e2
    | e1 e2 | fun (x:t1) : t2 -> e | fix f (x:t1) : t2 -> e
    | ()
    | (e1, e2) | fst e | snd e
    | ref e | e1 := e2 | !e | e1 ; e2

    t ::= int | bool | t1 -> t2 | unit | <t>
  ```
  where ```<t>``` denotes the type of a reference cell. ```Ptr(n)``` is an internal expression, functioning as the intermediate state between a reference cell and an expression. It is visible only during the process of evaluation and is hidden from the user.
  In ```Ptr(n)```, n is the unique key associated with a particular expression. We use an association list to store ```int * expression``` pairs and look up an expression with ```n``` using ```List.assoc```.
- Added while loops to the language and let it handle the additional syntax below:
  ```
  e ::= ... | while e1 do e2 end
  ```
  In our language, a finished ```while``` loop evaluates to the unit value and only updates the value each time it goes inside the loop.
  
## [7.0.1] - 2018-05-18
### Changed
- Changed the source language from expression-based to declaration-based. 
- There are two types of declarations: *variant declaration* and *function declaration*.
- The function calls do not depend on the order of their declarations. The compiler will read in all the declarations at once and find the function named ```main``` to execute.
- Added function signatures to typechecking and separated the typecheckers into two: one for typechecking declarations and the other for expressions so that we can typecheck when translating.
### Added
- Source to source translation to a C-like language.
- Added statements to the target language and completed conversion. The resulting code can be compiled using ```gcc```.
- The conversion supports:
   ```
  e ::= (e) | n | b | e1 (+) e2 | if e1 then e2 else e3
    | x | let x : t = e1 in e2
    | e1 (e list)
    | ()
    | (e1, e2) | fst e | snd e
    | ref e | e1 := e2 | !e | e1 ; e2
    | while e1 do e2 end

    t ::= int | bool | unit | <t>
  ```
- Note: due to implementation of the pair structure, pairs cannot be returned from a function and will only work within let-bindings because the pair struct is declared inside each function. 
