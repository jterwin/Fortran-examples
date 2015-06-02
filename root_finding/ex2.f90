program root_finding

  implicit none

  integer, parameter :: dp = kind(1.0d0)

  write(*,*) ""
  write(*,*) "secant method"
  call secant(1.0_dp, 2.0_dp)

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
  subroutine secant(p0in, p1in)

    real(dp), intent(in) :: p0in, p1in

    integer, parameter :: max_steps = 20
    real(dp), parameter :: TOL = 1.0e-8_dp

    integer :: i
    real(dp) :: p, p0, p1, fp, fp0, fp1

    p0 = p0in
    p1 = p1in

    do i = 1, max_steps

       fp0 = myfun(p0)
       fp1 = myfun(p1)

       p = p1 - fp1*(p1 - p0)/(fp1 - fp0)
       fp = myfun(p)

       write(*, *) i, p, fp

       if (abs(p - p1) < TOL .or. abs(fp) < TOL) then
          exit
       else
          p0 = p1
          p1 = p
       end if

    end do

  end subroutine secant



end program root_finding
