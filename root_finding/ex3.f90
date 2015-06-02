program root_finding

  implicit none
  
  integer, parameter :: dp = kind(1.0d0)

  write(*,*) ""
  write(*,*) "newton's method"
  call newton(1.0_dp)

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


  subroutine newton(x0)

    real(dp), intent(in) :: x0

    integer, parameter :: max_steps = 20
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: p, p0, fp, fp0, f2p0

    p0 = x0

    do i = 1, max_steps

       fp0 = myfun(p0)
       f2p0 = myfunprime(p0)

       p = p0 - fp0/f2p0
       fp = myfun(p)

       write(*, *) i, p, fp

       if (abs(p - p0) < TOL .or. abs(fp) < TOL) then
          exit
       else
          p0 = p
       end if

    end do

  end subroutine newton



end program root_finding
