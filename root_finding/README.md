# Root Finding

## Bisection method [ex1.f90](ex1.f90)
* find the root (x-intercept) of a function by successively narrowing
  in on it. Requires knowing the interval that the root is in. (see [Bisection_method](en.wikipedia.org/wiki/Bisection_method))
	
## Secant method [ex2.f90](ex2.f90) 
* uses the slope between two points to quickly converge to the root. (see [Secant_method](en.wikipedia.org/wiki/Secant_method))

## Newton's method [ex3.f90](ex3.f90) 
* Newton's method uses the function and its derivative to quickly
  converge to the root. (see [Newton_method](en.wikipedia.org/wiki/Newton%27s_method))
  
## Pass functions as arguments [ex4.f90](ex4.f90) 
* By passing the function name as an arguement, our methods become
  more useful, and dont need to be modified for new projects. 
* The **interface** block is used to tell the compiler that the input is
a function, along with its inputs and return values.
* With **user-defined precision** (i.e. *real(dp)*), we need to have the
  definition of the precision (i.e. *dp*) defined in a module that can
  be included by the interface block. 
