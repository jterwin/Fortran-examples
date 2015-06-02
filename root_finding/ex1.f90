program root_finding

  implicit none

  integer, parameter :: dp = kind(1.0d0)
  
  integer :: i, nstep
  real(dp) :: x, f

  nstep = 20
  
  do i = 1,nstep
     x = 1 + real(i)/nstep
     write(*,*) x, myfun(x)
  enddo

  write(*,*) ""
  write(*,*) "bisection method"
  call bisect(1.0_dp, 2.0_dp, x, f)

contains

  !
  ! this function
  !
  function myfun(x) result(f)

    real(dp), intent(in) :: x
    real(dp) :: f
    
    f = x**3 + 4.0_dp*x**2 - 10.0_dp
    return
    
  end function myfun

  
  !
  !
  !
  !
  subroutine bisect(ain, bin, xopt, fopt)

    real(dp), intent(in) :: ain, bin
    real(dp), intent(out) :: xopt, fopt

    integer, parameter :: max_steps = 100
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: a, b, fa, fb, c, fc


    a = ain
    b = bin
    
    fa = myfun(a)
    fb = myfun(b)

    do i = 1,max_steps
       c = (a+b)/2.0
       fc = myfun(c)

       write(*,*) c, fc

       if (abs(b-a)/2 < TOL .or. abs(fc) < TOL) then
          exit
       end if
       
       if (fa*fc < 0.0) then
          b = c
          fb = fc
       else
          a = c
          fa = fc
       end if
    end do

    xopt = (a+b)/2
    fopt = myfun(xopt)

  end subroutine bisect

    

end program root_finding
