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

There are two flags: --help and --length. Including --help in the arguments will yield appropriate instructions.

### Testing the Project
To run the included tests type
```
stack test
```
from the root of the project.

## Changelog

#### 1/30/2019
##### New Features
- Basic command-line functionality
- Extrinsic test foundation added
##### Changed Features
- N/A
##### Known Bugs
- N/A
