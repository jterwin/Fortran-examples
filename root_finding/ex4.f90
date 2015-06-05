!  To pass functions as an input we can use the interface block, which inluces
!    stating the kind and size of the input and outputs. If we use user-defined
!    real precision then this must be in a module that can be used by the interface
!    block
module types
  integer, parameter :: dp=selected_real_kind(15)
end module types


!
! put these subroutines in a module that can be re-used
!
module subs

  use types
  implicit none

contains


  !
  !
  !
  subroutine bisect(fun, ain, bin, xopt)

    ! - Here is the interface for the function fun, we need only
    !   state the inputs with intent, size, and type/precision.
    ! (Note: we need to use "types" inside the interace to
    !        get user-defined precision)
    interface
       function fun(x)
         use types
         real(dp) :: fun
         real(dp), intent(in) :: x
       end function fun
    end interface

    real(dp), intent(in) :: ain, bin
    real(dp), intent(out) :: xopt
    logical, optional, intent(in) :: verbose

    integer, parameter :: MAX_ITERS = 50
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: a, b, fa, fb, c, fc

    a = ain
    fa = fun(a)
    b = bin
    fb = fun(b)

    do i = 1,MAX_ITERS

       c = (a+b)*0.5_dp
       fc = fun(c)

       write(*,*) i, p, fp

       if (abs(b-c) < TOL .or. abs(fc) < TOL) then
          exit
       end if

       if (fa*fc < 0.0_dp) then
          b = c
          fb = fc
       else
          a = c
          fa = fc
       end if
    end do

    xopt = (a+b)/2

  end subroutine bisect


  !
  !
  !
  subroutine secant(fun, x0, x1, xopt)

    interface
       function fun(x)
         use types
         real(dp) :: fun
         real(dp), intent(in) :: x
       end function fun
    end interface

    real(dp), intent(in) :: x0, x1
    real(dp), intent(out) :: xopt

    integer, parameter :: MAX_ITERS = 20
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: p, p0, p1, fp, fp0, fp1, delp

    p0 = x0
    p1 = x1
    fp0 = fun(p0)
    fp1 = fun(p1)

    do i = 1, MAX_ITERS

       delp = - fp1*(p1 - p0)/(fp1 - fp0)
       p = p1 + delp
       fp = fun(p)

       write(*,*) i, p, fp

       if (abs(delp) < TOL .or. abs(fp) < TOL) then
          exit
       else
          p0 = p1
          fp0 = fp1
          p1 = p
          fp1 = fp
       end if

    end do

  end subroutine secant


  !
  !
  !
  subroutine newton(fun, funp, x0, xopt, verbose)

    interface
       function fun(x)
         use types
         real(dp) :: fun
         real(dp), intent(in) :: x
       end function fun
       function funp(x)
         use types
         real(dp) :: funp
         real(dp), intent(in) :: x
       end function funp
    end interface

    real(dp), intent(in) :: x0
    real(dp), intent(out) :: xopt

    integer, parameter :: MAX_ITERS = 20
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: p, fp, fpp, delp
    logical :: write_steps


    ! if called with optional input verbose=.true., then
    !  print out iteration steps.
    write_steps = .False.
    if (present(verbose)) then
       if (verbose) write_steps = .True.
    end if

    ! initial guess
    p = x0
    fp = fun(p)

    ! newton loop
    do i = 1, MAX_ITERS

       fpp = funp(p)

       delp = - fp/fpp
       p = p + delp
       fp = fun(p)

       if (write_steps) write(*,*) i, c, fc

       if (abs(delp) < TOL .or. abs(fp) < TOL) then
          exit
       end if

    end do

    ! reached max_steps, then stop program with error message
    if (i .eq. MAX_ITERS) stop "newton's method did not converge (MAX_ITERS)"

    xopt = p
    return

  end subroutine newton

end module subs


!
!
!
program root_finding

  use types
  use subs

  implicit none

  real(dp) :: xopt

  write(*,*) ""
  write(*,*) "f = x**3 + 4.0_dp*x**2 - 10.0_dp"
  
  write(*,*) ""
  write(*,*) "bisection method"
  call bisect(myfun, 1.0_dp, 2.0_dp, xopt)

  write(*,*) ""
  write(*,*) "secant method"
  call secant(myfun, 1.0_dp, 2.0_dp, xopt)

  write(*,*) ""
  write(*,*) "newton's method"
  call newton(myfun, myfunprime, 1.0_dp, xopt, verbose=.true.)


  write(*,*) ""
  write(*,*) "f = x - cos(x)"
    
  write(*,*) ""
  write(*,*) "bisection method"
  call bisect(myfun2, 0.7_dp, 0.8_dp, xopt)

  write(*,*) ""
  write(*,*) "secant method"
  call secant(myfun2, 0.7_dp, 0.8_dp, xopt)

  write(*,*) ""
  write(*,*) "newton's method"
  call newton(myfun2, myfunprime2, 0.7_dp, xopt, verbose=.true.)

contains

  !
  ! this function
  !
  function myfun(x) result(f)

    real(dp) :: f
    real(dp), intent(in) :: x

    f = x**3 + 4.0_dp*x**2 - 10.0_dp
    return

  end function myfun

  function myfunprime(x) result(fp)

    real(dp) :: fp
    real(dp), intent(in) :: x

    fp = 3.0_dp*x**2 + 8.0_dp*x
    return

  end function myfunprime


  !
  ! or this function
  !
  function myfun2(x) result(f)

    real(dp) :: f
    real(dp), intent(in) :: x

    f = x - cos(x)
    return

  end function myfun2

  function myfunprime2(x) result(fp)

    real(dp) :: fp
    real(dp), intent(in) :: x

    fp = 1.0_dp + sin(x)
    return

  end function myfunprime2


end program root_finding
