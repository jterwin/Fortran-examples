program root_finding

  implicit none

  integer :: i, nstep
  real :: x, f

  real :: xlin(20), flin(20,40)

  nstep = 20
  
  do i = 1,nstep

     xlin(i) = 1 + real(i)/nstep

     flin(i) = myfun(xlin(i))

  enddo

  write(*,*) xlin, flin

     
  call bisect(1.0, 2.0, x, f)

contains


  real function myfun(x)

    real, intent(in) :: x
    
    myfun = x**3 + 4.0*x**2 - 10.0
    return
    
  end function myfun

  

  subroutine bisect(ain, bin, xopt, fopt)

    real, intent(in) :: ain, bin
    real, intent(out) :: xopt, fopt

    integer :: i
    integer, parameter :: max_steps = 100
    real, parameter :: TOL = 1.0e-6

    real :: a, b, fa, fb, c, fc

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


       
  end subroutine bisect

    

end program root_finding
