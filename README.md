# Fortran examples

This guide is meant as a short introduction to Fortran 90 basics, but
moreso as a set of examples of good practices in developing large
numerical projects.

Fortran has been in use since the old punch card days, and there has
been many versions. Fortran 90 has most of the features of a modern
programming language, and is well suited for numerical modeling. 

## Basics
* basic fortran syntax
* precision and arrays
* subroutines and functions
* split source code over multiple files
* reading and writing to text files

## Root finding
* compare bisection, secant, and Newton's methods
* pass functions as arguments

## ODE's
* simple numerical odes
* systems of odes
* Runge-Kutta

## LAPACK
* call LAPACK subroutines
* use Fortran 95 wrapping
* use MKL

## Optimization
* Newton's method for non-linear systems
* minimize a scalar function
* non-linear least squares
  * Gauss–Newton algorithm
  * Levenberg–Marquardt algorithm

## PDE's
* 1d elliptical
* other boundary conditions
* 2d elliptical
* parabolic
