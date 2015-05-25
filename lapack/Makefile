
all: tlapack.x tlapack95.x
#all: tmkl.x tmkl95.x

#
# build instruction using lapack and gfortran
#

FCFLAGS1 = -Wall -g -fbounds-check
FCFLAGS1 = -O3

LDFLAGS1 = -lblas -llapack
#LDFLAGS1 = -framework vecLib
#LDFLAGS1 = -framework accelerate

test_lapack.o: test_lapack.f90
	gfortran $(FCFLAGS1) -c -o test_lapack.o test_lapack.f90

tlapack.x: test_lapack.o
	gfortran $(FCFLAGS1) -o tlapack.x test_lapack.o $(LDFLAGS1) 



#
# build instruction using lapack95 and gfortran (on macbook pro)
#

LAPACK95_ROOT=/Users/jterwin/Downloads/LAPACK95

FCFLAGS2 = -O3
FCFLAGS2 += -I$(LAPACK95_ROOT)/lapack95_modules

LDFLAGS2 = -L$(LAPACK95_ROOT) -llapack95 -lblas -llapack

test_lapack95.o: test_lapack95.f90
	gfortran $(FCFLAGS2) -c -o $@ $^

tlapack95.x: test_lapack95.o
	gfortran $(FCFLAGS2) -o $@ $^ $(LDFLAGS2) 




#
# build instruction using MKL and ifort
#

FCFLAGS3 = -O3 -mkl=sequential
# following line is needed on HIPAS
#FCFLAGS3 += -I$(MKLROOT)/include/em64t/lp64
LDFLAGS3 = -lmkl_lapack95_lp64


test_mkl95.o: test_mkl95.f90
	ifort $(FCFLAGS3) -c -o $@ $^

tmkl95.x: test_mkl95.o
	ifort $(FCFLAGS3) -o $@ $^ $(LDFLAGS3)


tmkl.x: test_lapack.f90
	ifort $(FCFLAGS3) -o $@ $^


# Utility targets
.PHONY: clean veryclean

clean:
	rm -f *.o *.mod *.MOD

veryclean: clean
	rm -f *~ *.x
