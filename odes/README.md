# ODE's

To solve ordinary differential equation, we discretize the solution in
time, and approximate the solution at these time steps.

discuss "Implicit versus explicit"

discuss "error" and "order of accuracy"

## Euler method
See [Euler method](en.wikipedia.org/wiki/Euler_method). This is the
simplest method, which can be easily derived by Taylor series. It is
only first order accurate (meaning the error is propotional to the
step size) so it can become innacurate if the step-size is chosen too
big. 


## Backward Euler
See
[Backward-Euler](en.wikipedia.org/wiki/Backward_Euler_method). This is
an implicit method, meaning the derivative is calculated at the new
time-step, but since the solution there is unkown at first, we need to
find a solution that is consistent with the derivative. Hence you need
to use a fixed-point or root-finding method (like Newton) to get a
solution at each time step. It is still first order, but is much more
stable than the standard Euler.

Another method is the trapeziodal Euler method, which uses the average
of the current step and the advanced step. Therefor this is an
implicit method, but we gain some stability. The extension of this to
PDE's is the Crank-Nicolson methods, which is much more common,
therefore I will wait until PDE's to give this.

## Runge-Kutta
See
[Runge-Kutta methods](http://en.wikipedia.org/wiki/Runge–Kutta_methods). Runge-Kutta
methods are a set of iterative methods used to solve ODE's. There are
both implicit and explicit formulations, and of many orders. The most
common are the 4th order (RK4), and the adaptive Runge-Kutta-Fehlberg
method (RK45). This last one compares a 4th order with a 5th order to
get an error estimate, which is then used to adjust the
step-size. This way a error tolerance can be maintained using a
minumum number of steps.


## Systems of ODE's




## Velocity Verlet
See[Verlet integrator](en.wikipedia.org/wiki/Verlet_integration#Velocity_Verlet). This
is a method for solving second order differential equations, and
therefore is well suited for simulated motion (e.g. planetary bodies,
molecules in the exosphere). The assumption is that the acceleration
(force) does not depend on the velocity. This method is second order
accurate (compare to first order of Euler methods).
