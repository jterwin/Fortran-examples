program newton_system

  implicit none

  integer, parameter :: dp=kind(0.d0)

  real(dp) :: x0(3)

  x0 = (/ 0.1_dp, 0.1_dp, -0.1_dp /)
  

  call newtons(x0)

  write(*,*) x0

contains



  subroutine newtons(x)

    real(dp), intent(inout) :: x(3)

    real(dp), parameter :: TOL = 1e-10_dp
    integer, parameter :: MAX_STEPS = 50

    real(dp) :: f(3), J(3,3), y(3)
    integer :: i


    integer :: n, nrhs, LDA, IPIV(3), LDB, INFO

    ! some values needed for lapack
    n = 3
    nrhs = 1
    lda = 3
    ldb = 3
    
    do i = 1,MAX_STEPS

       !
       f = myfun(x)
       J = myjac(x)
       
       call dgesv(n, nrhs, J, LDA, IPIV, f, LDB, INFO)
       ! now the solution to J*y=f is stored in f
       ! also the LU factorization of J is stored in J

       ! we want solution to J*y = -f
       y = -f

       ! update x
       x = x + y

       write(*,*) i, x, sqrt(sum(y**2)), INFO

       ! chaeck for convergence
       if (sqrt(sum(y**2)) < TOL) then
          exit
       end if

    end do

    

  end subroutine newtons



  
  function myFun(x)

    real(dp), intent(in) :: x(3)
    real(dp) :: myFun(3)


    myfun(1) = 3.0_dp*x(1)-cos(x(2)*x(3)) - 0.5_dp
    myfun(2) = x(1)**2 - 81.0*(x(2) + 0.1)**2 + sin(x(3)) + 1.06
    myfun(3) = exp(-x(1)*x(2)) + 20.0*x(3) + (10.0*3.1415-3.0)/3.0
    
    return
    
  end function myFun


  function myJac(x)

    real(dp), intent(in) :: x(3)
    real(dp) :: myJac(3,3)

    myJac(1, 1) = 3.0
    myJac(1, 2) = x(3)*sin(x(2)*x(3))
    myJac(1, 3) = x(2)*sin(x(2)*x(3))
    myJac(2, 1) = 2.0*x(1)
    myJac(2, 2) = -162.0*(x(2)+0.1)
    myJac(2, 3) = cos(x(3))
    myJac(3, 1) = -x(2)*exp(-x(1)*x(2))
    myJac(3, 2) = -x(1)*exp(-x(1)*x(2))
    myJac(3, 3) = 20.0
    return

  end function myJac


  
end program newton_system
