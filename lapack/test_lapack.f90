PROGRAM TEST

  IMPLICIT NONE

  INTEGER, PARAMETER :: SP = KIND(1.0), DP = KIND(1.0d0)

  INTEGER :: ndim1, ndim2, ndim3
  REAL(DP) :: etimes(100), dims1(100), dims2(100), dims3(100)

  INTEGER :: i, n


  ! short test
  ndim1 = 4*2+1
  dims1(1:ndim1) = logspace( (10._DP)**1, (10._DP)**3, ndim1)
  ndim2 = 4*1+1
  dims2(1:ndim2) = logspace( (10._DP)**1, (10._DP)**2, ndim2)
  ndim3 = 4*1+1
  dims3(1:ndim3) = logspace( (10._DP)**1, (10._DP)**2, ndim3)

  ! long test
  IF (.false.) THEN
     ndim1 = 4*7+1
     dims1(1:ndim1) = logspace( (10._DP)**1, (10._DP)**8, ndim1) 
     ndim2 = 5*3+1
     dims2(1:ndim2) = logspace( (10._DP)**1, (10._DP)**4, ndim2)
     ndim3 = 5*3+1
     dims3(1:ndim3) = logspace( (10._DP)**1, (10._DP)**4, ndim3)
  ENDIF


  WRITE(*,*) "","",""
  WRITE(*,*) "O(n) stuff"
  WRITE(*,'(A10,10A10)') "N", "dot_prod", "sum()", "dDOT", "(a+b)", "dAXPY", "dGTSV"
  DO i = 1,ndim1
     n = INT(dims1(i))
     CALL test1( n, etimes(1:5))
     CALL test_gtsv(n, etimes(6))
     WRITE(*,'(I10,10ES10.2)') n, etimes(1:6)
  ENDDO

  WRITE(*,*) "","",""
  WRITE(*,*) "O(n^2) stuff"
  WRITE(*,'(A10,10A10)') "N", "matmul", "dGEMV", "dTRSM"
  DO i = 1,ndim2
     n = INT(dims2(i))
     CALL test2( n, etimes(1:2))
     CALL test_TRSM(n, etimes(3))
     WRITE(*,'(I10,10ES10.2)') n, etimes(1:3)
  ENDDO

  WRITE(*,*) "","",""
  WRITE(*,*) "O(n^3) stuff"
  WRITE(*,'(A10,10A10)') "N", "matmul", "dGEMM","dGESV"
  DO i = 1,ndim3
     n = INT(dims3(i))
     CALL test3( n, etimes(1:2))
     CALL test_gesv(n, etimes(3))
     WRITE(*,'(I10,10ES10.2)') n, etimes(1:3)
  ENDDO


  ! END PROGRAM
  STOP

CONTAINS




  !
  ! test fortran's built in vs BLAS dot product and vector addition
  !
  SUBROUTINE test1(n, etime)

    ! needed for BLAS routine
    REAL(DP), EXTERNAL :: ddot

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime(5)

    INTEGER :: i
    REAL(DP) :: start_time, end_time
    REAL(DP), DIMENSION(n) :: a,b,c

    DO i=1,n
          CALL RANDOM_NUMBER(a(i))
          CALL RANDOM_NUMBER(b(i))
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    C = DOT_PRODUCT(a,b)
    CALL CPU_TIME(end_time)
    etime(1) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    C = sum(a*b)
    CALL CPU_TIME(end_time)
    etime(2) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    C = dDOT(n,a,1,b,1)
    CALL CPU_TIME(end_time)
    etime(3) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    C = a+2.5_DP*b
    CALL CPU_TIME(end_time)
    etime(4) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    CALL dAXPY(n, 2.5_DP, a, 1, b, 1)
    CALL CPU_TIME(end_time)
    etime(5) = end_time-start_time 

  END SUBROUTINE test1


  !
  ! test LAPACK routine xGESV (General Tridiagian SolVe)
  !
  SUBROUTINE test_gtsv(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(DP) :: start_time, end_time
    INTEGER, PARAMETER :: nrhs = 1
    REAL(DP) :: DL(n), D(n), DU(n), B(n,nrhs)
    INTEGER :: info

    ! fill in matrices
    DO i=1,n
       CALL RANDOM_NUMBER(D(i))
       CALL RANDOM_NUMBER(DU(i))
       CALL RANDOM_NUMBER(DL(i))
    ENDDO
    DO i=1,n
       DO j=1,nrhs
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL DGTSV(n,nrhs,DL,D,DU,B,n,info)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_gtsv




  !
  ! test matrix vector multiplication: fortran's built in matmul vs BLAS xGEMV
  !
  SUBROUTINE test2(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime(2)

    INTEGER :: i,j
    REAL(DP) :: start_time, end_time
    REAL(DP) :: A(n,n), B(n), C(n)
    
    DO i=1,n
       DO j=1,n
          CALL RANDOM_NUMBER(A(i,j))
       ENDDO
       CALL RANDOM_NUMBER(B(i))
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    C = MATMUL(A,B)
    CALL CPU_TIME(end_time)
    etime(1) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    CALL DGEMV('n',n,n,1.0_DP,A,n,B,1,0.0_DP,C,1)
    CALL CPU_TIME(end_time)
    etime(2) = end_time-start_time 

  END SUBROUTINE test2


  !
  ! test BLAS routine xTRSM (triangular solve multiple)
  !
  SUBROUTINE test_trsm(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(DP) :: start_time, end_time
    INTEGER, PARAMETER :: nrhs = 1
    REAL(DP) :: A(n,n), B(n,nrhs)

    ! fill in matrices
    DO i=1,n
       DO j=i,n
          CALL RANDOM_NUMBER(A(i,j))
       ENDDO
    ENDDO
    DO i=1,n
       DO j=1,nrhs
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL DTRSM('L','U','N','N',n,nrhs,1.0_DP,A,n,B,n)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_trsm


  !
  ! test matrix-matrix multiplication: 
  !   fortran's built in matmul vs BLAS xGEMM
  !
  SUBROUTINE test3(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime(2)

    INTEGER :: i,j
    REAL(DP) :: start_time, end_time
    REAL(DP), DIMENSION(n,n) :: A, B, C

    DO i=1,n
       DO j=1,n
          CALL RANDOM_NUMBER(A(i,j))
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    C = MATMUL(A,B)
    CALL CPU_TIME(end_time)
    etime(1) = end_time-start_time 

    ! time work
    CALL CPU_TIME(start_time)
    CALL DGEMM('n','n',n,n,n,1.0_DP,A,n,B,n,0.0_DP,C,n)
    CALL CPU_TIME(end_time)
    etime(2) = end_time-start_time 

  END SUBROUTINE test3


  !
  ! test LAPACK routine xGESV (General Equation SolVe)
  !
  SUBROUTINE test_gesv(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(DP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(DP) :: start_time, end_time
    INTEGER, PARAMETER :: nrhs = 1
    REAL(DP) :: A(n,n), B(n,nrhs)
    INTEGER :: ipv(n), info

    ! fill in matrices
    DO i=1,n
       DO j=1,n
          CALL RANDOM_NUMBER(A(i,j))
       ENDDO
    ENDDO
    DO i=1,n
       DO j=1,nrhs
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL DGESV(n,nrhs,A,n,ipv,B,n,info)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_gesv


  !
  !
  !  
  FUNCTION logspace( xmin, xmax, nx)

    REAL(DP), INTENT(IN) :: xmin, xmax
    INTEGER, INTENT(IN) :: nx
    REAL(DP), DIMENSION(nx) :: logspace

    INTEGER :: i
    REAL(DP) :: dx

    dx = (log(xmax) - log(xmin))/(nx-1)

    DO i = 1,nx
       logspace(i) = EXP( LOG(xmin) + (i-1)*dx )
    ENDDO

  END FUNCTION logspace


END PROGRAM TEST
