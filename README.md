# Aardvark
A fuzzy animal compiler

### Overview
This project is a (soon-to-be) compiler written in Haskell for [CSC 312](http://www.cs.grinnell.edu/~osera/courses/csc312/18sp).

### Building the Project
Clone the repository onto your host computer. The project has not been tested on non-Linux machines. The project requires that Haskell Stack is installed on the host machine.

To build the project type
```
stack build
```
from the root directory of the project.

### Running the Project
To run the project type
```
stack exec aardvark-exe -- the arguments you wish to invoke
```
with any arguments you wish to invoke on the program.

Use the --help flag to explore additional commands.

### Testing the Project
To run the included tests type
```
stack test
```
from the root of the project.

### Grammar

Syntax:
```
e ::= n | e1 + e2 | e1 - e2 | e1 * e2) | e1 / e2
    | true | false | (<= e1 e2) | if e1 e2 e3 | func var -> e
    | fix var var -> e
```

Where n is a number of the form of an integer or float and var is an alphabetical string for a variable name.

## Changelog
#### 3/8/2018
- Addition of functions and fix functions
- Revamped test suite
- Implemented small-step semantics

#### 2/18/2018
##### New FEatures
- Addition of if statements
- No longer using LISP-like syntax
- Can use lexer and parser flags to see lexer and parser output
- Updated Tests
- Position information in AST

##### Changed Features
- Syntax has been changed
- Alex and Happy for lexing and parsing

##### Known Bugs
- N/A

#### 2/7/2018
##### New Features
- Basic compilation and evaluation of LISP-like expressions
- Input strings to the program now are compiled and evaluated with the evaluation printed to the screen

##### Changed Features
- Removed old command-line functionality

#### Known Bugs
- Current extrinsic tests are not longer relevant and need to be updated

#### 1/30/2019
##### New Features
- Basic command-line functionality
- Extrinsic test foundation added
##### Changed Features
- N/A
##### Known Bugs
- N/A
