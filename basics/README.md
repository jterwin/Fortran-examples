# Basics

## Basic fortran syntax [hello world](hello_world.f90)
* the basic Fortan syntax in introduced.
* Fortran code is made up of blocks, in this example we introduce you
to the *PROGRAM* block.
* compile this code using
```
gfortran hello_world.f90 -o hello.x
```
and then run it by entering
```
./hello.x
```


## Data types and arrays
* integers and floating point
* arrays
* precision
* structures
* allocatable arrays


## Subroutines and functions
* It is often convenient to organize code into separate blocks, and to
  call these blocks from the main program. Two types of blocks are
  called *SUBROUTINE* and *FUNCTION*
* A function takes a set of inputs, and returns a value based off of
those inputs
* A subroutine takes a set of arguments that it can use and/or change
  (i.e. there can be multiple inputs and outputs).


## Split source code over multiple files
* We can move some of the code (subroutines and functions) to a
  separate file, and compile many files together into a single
  executable.
* This is a great way to reuse parts of your code.


## Modules
* Modules are a fortran block that can contain both data and
  subroutines. They can be used similar to classes in C to encapsulate
  data and the methods that operate on that data (really its just a
  nice way to keep your program organized).
* Modules also have the nice feature that an interface is created for
  any subroutine or function in the module. This is the .mod file, and
  is similar to a .h file from C. The interface is a bit of code that
  tells the compiler exactly what the inputs and outputs are, so the
  compiler can link everything together properly.

## Reading and writing to files
* text files
* binary files
