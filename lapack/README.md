# LAPACK

## call LAPACK subroutines

call LAPACK using the standard FORTRAN 77 way

## use Fortran 95 wrapping

There is a project called LAPACK 95, which is just a wrapper around
the standard LAPACK library. But the subroutines are included in a
module, and the interfaces will choose the numerical/precision types
for you!

Since this project is not standard on most systems, I can't expect the
library to be available. So instead I write my own wrappers for the
most common LAPACK subroutines, which can then be used as a template
for other subroutines when needed.

## use MKL

The Math Kernal Library is a numerical library included with the Intel
compiler suite that has been optimized for the architecture. We can
use the standard LAPACK subroutine calls, or there is the LAPACK 95
subroutine calls if we link to the right library.

Due to small naming differences, my preferred way is to use my own
wrappers for the LAPACK 95 calls.

## testing/timing

Examples showing the differences between gfortran and ifort, as well
as LAPACK vs LAPACK 95. Running this code on the cluster we can see
the acceleration of using ifort over gfortran.
