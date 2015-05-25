PROGRAM TEST

  USE MKL95_PRECISION, ONLY: WP=>DP

  IMPLICIT NONE

  INTEGER :: omin, omax, odiv, ndim, dims(100)
  REAL(WP) :: etimes(100)

  INTEGER :: i, n
  REAL(WP) :: do

  omin = 10**1
  omax = 10**4
  odiv = 3

  ndim = NINT(odiv*(log10(omax*1._WP)-log10(omin*1._WP))) + 1

  DO i = 1,ndim
     dims(i) = 10._WP**(LOG10(omin*1._WP)+(LOG10(omax*1._WP)-LOG10(omin*1._WP))*(i-1)/(ndim-1))
  ENDDO
  !WRITE(*,'(A10, 100I10)') "", dims(1:ndim)

  WRITE(*,*) "","",""
  WRITE(*,*) "O(n) stuff"
  WRITE(*,'(A6,10A10)') "N", "dot_prod", "sum()", "dDOT", "(a+b)", "dAXPY", "dGTSV"



  STOP


  WRITE(*,*) "","",""
  WRITE(*,*) "O(n^2) stuff"
  WRITE(*,'(A6,10A10)') "N", "matmul", "dGEMV", "dTRSM"


  WRITE(*,*) "","",""
  WRITE(*,*) "O(n^3) stuff"
  WRITE(*,'(A6,10A10)') "N", "matmul", "gemm", "trsm", "gesv"
  DO i = 1,SIZE(dims)
     n = dims(i)

     !CALL test_matmul(n, etimes(1))
     !CALL test_gemm(n, etimes(2))
     CALL test_trsm(n, etimes(3))
     CALL test_gesv(n, etimes(4))
     CALL test_gtsv(n, etimes(5))

     WRITE(*,'(I6,10ES10.2)') n, etimes(1:5)

  ENDDO

  STOP

CONTAINS


  !
  ! test fortran's built in matmul
  !
  SUBROUTINE test_matmul(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(WP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(WP) :: start_time, end_time
    REAL(WP), DIMENSION(n,n) :: A, B, C

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
    etime = end_time-start_time 

  END SUBROUTINE test_matmul


  !
  ! test BLAS routine xGEMM (matrix-matrix multiplication)
  !
  SUBROUTINE test_gemm(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(WP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(WP) :: start_time, end_time
    REAL(WP), DIMENSION(n,n) :: A, B, C

    ! fill in matrices
    DO i=1,n
       DO j=1,n
          CALL RANDOM_NUMBER(A(i,j))
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL DGEMM('n','n',n,n,n,1.0_WP,A,n,B,n,0.0_WP,C,n)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_gemm


  !
  ! test BLAS routine xTRSM (triangular solve multiple)
  !
  SUBROUTINE test_trsm(n, etime)

    INTEGER, INTENT(IN) :: n
    REAL(WP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(WP) :: start_time, end_time
    INTEGER, PARAMETER :: m = 1
    REAL(WP) :: A(n,n), B(n,m)

    ! fill in matrices
    DO i=1,n
       DO j=i,n
          CALL RANDOM_NUMBER(A(i,j))
       ENDDO
    ENDDO
    DO i=1,n
       DO j=1,m
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL DTRSM('L','U','N','N',n,m,1.0_WP,A,n,B,n)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_trsm


  !
  ! test LAPACK95 routine xGESV (triangular solve multiple)
  !
  SUBROUTINE test_gesv(n, etime)

    USE LAPACK95, ONLY: GESV

    INTEGER, INTENT(IN) :: n
    REAL(WP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(WP) :: start_time, end_time
    INTEGER, PARAMETER :: m = 1
    REAL(WP) :: A(n,n), B(n,m)
    INTEGER :: ipv(n), info

    ! fill in matrices
    DO i=1,n
       DO j=1,n
          CALL RANDOM_NUMBER(A(i,j))
       ENDDO
    ENDDO
    DO i=1,n
       DO j=1,m
          CALL RANDOM_NUMBER(B(i,j))
       ENDDO
    ENDDO

    ! time work
    CALL CPU_TIME(start_time)
    CALL GESV(A,B)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_gesv


  !
  ! test LAPACK95 routine xGESV (triangular solve multiple)
  !
  SUBROUTINE test_gtsv(n, etime)

    USE MKL95_LAPACK, ONLY: GTSV

    INTEGER, INTENT(IN) :: n
    REAL(WP), INTENT(OUT) :: etime

    INTEGER :: i,j
    REAL(WP) :: start_time, end_time
    INTEGER, PARAMETER :: nrhs = 1
    REAL(WP) :: DL(n), D(n), DU(n), B(n,nrhs)
    INTEGER :: ipv(n), info

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
    CALL GTSV(DL,D,DU,B,info)
    CALL CPU_TIME(end_time)
    etime = end_time-start_time 

  END SUBROUTINE test_gtsv


END PROGRAM TEST
