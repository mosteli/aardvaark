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
stack exec aardvark -- the arguments you wish to invoke
```
with any arguments you wish to invoke on the program.

There is one flag: --help.

### Testing the Project
To run the included tests type
```
stack test
```
from the root of the project.

### Grammar

```
EAdd ::= ELit n + ELit n | EFloat n + EFloat n
EMul ::= ELit n * ELit n | EFloat n * EFloat n
EDiv ::= ELit n `div` ELit n | EFloat n / EFloat n
ELit ::= [0-9]+
EFloat ::= [0-9]+.[0-9]+
NaN ::= NaN
ETrue = true
EFalse = false
```

## Changelog

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
